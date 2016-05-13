/*
 * Copyright 2006-2014 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb
package db

import common._
import util._
import Helpers._

import net.liftweb.http.S

import javax.sql.{DataSource}
import java.sql.{ResultSetMetaData, SQLException}
import java.sql.{Statement, ResultSet, Types, PreparedStatement, Connection, DriverManager}
import scala.collection.mutable.{HashMap, ListBuffer}
import javax.naming.{Context, InitialContext}

trait DB1

object DB1 {
  implicit def db1ToDb(in: DB1): DB = DB.theDB
}

object DB extends DB1 {

  private[db] lazy val theDB = new DB{}
}

trait DB extends Loggable {
  private val threadStore = new ThreadLocal[HashMap[ConnectionIdentifier, ConnectionHolder]]
  private val _postCommitFuncs = new ThreadLocal[List[() => Unit]]

  var globalDefaultSchemaName: Box[String] = Empty

  var queryTimeout: Box[Int] = Empty

  type LogFunc = (DBLog, Long) => Any
  private var logFuncs: List[LogFunc] = Nil

  def addLogFunc(f: LogFunc): List[LogFunc] = {
    logFuncs = logFuncs ::: List(f)
    logFuncs
  }

  def loggingEnabled_? = !logFuncs.isEmpty

  /**
   * queryCollector can be used to collect all statements executed in a single request when passed to addLogFunc
   *
   * Use S.queryLog to get the list of (statement, duration) entries or set an analyzer function using
   * S.addAnalyzer
   */
  @volatile var queryCollector: LogFunc = {
    case (query:DBLog, time) =>
  }



  /**
   * Try to obtain a Connection using the jndiName of the ConnectionIdentifier
   */
  private def jndiConnection(name: ConnectionIdentifier) : Box[Connection] = {
    val toTry: List[() => Connection] = List(
      () => {
        logger.trace("Trying JNDI lookup on java:/comp/env followed by lookup on %s".format(name.jndiName))
        (new InitialContext).lookup("java:/comp/env").asInstanceOf[Context].lookup(name.jndiName).asInstanceOf[DataSource].getConnection
      },
      () => {
        logger.trace("Trying JNDI lookup on java:/comp/env/%s".format(name.jndiName))
        (new InitialContext).lookup("java:/comp/env/" + name.jndiName).asInstanceOf[DataSource].getConnection

      },
      () => {
        logger.trace("Trying JNDI lookup on %s".format(name.jndiName))
        (new InitialContext).lookup(name.jndiName).asInstanceOf[DataSource].getConnection

      }
    )

    first(toTry) (f => tryo{t:Throwable => logger.trace("JNDI Lookup failed: "+t)}(f())) or {
      logger.trace("Unable to obtain Connection for JNDI name %s".format(name.jndiName))
      Empty
    }
  }

  /**
   * can we get a JDBC connection from JNDI?
   */
  def jndiJdbcConnAvailable_? : Boolean = jndiConnection(DefaultConnectionIdentifier).isDefined

  private val connectionManagers = new HashMap[ConnectionIdentifier, ConnectionManager]

  private val threadLocalConnectionManagers = new ThreadGlobal[Map[ConnectionIdentifier, ConnectionManager]]

  def defineConnectionManager(name: ConnectionIdentifier, mgr: ConnectionManager) {
    connectionManagers(name) = mgr
  }

  /**
  * Allows you to override the connection manager associated with particular connection identifiers for the duration
  * of the call.
  */
  def doWithConnectionManagers[T](mgrs: (ConnectionIdentifier, ConnectionManager)*)(f: => T): T = {
    val newMap = mgrs.foldLeft(threadLocalConnectionManagers.box openOr Map())(_ + _)
    threadLocalConnectionManagers.doWith(newMap)(f)
  }

  case class ConnectionHolder(conn: SuperConnection, cnt: Int, postTransaction: List[Boolean => Unit], rolledBack: Boolean)

  private def info: HashMap[ConnectionIdentifier, ConnectionHolder] = {
    threadStore.get match {
      case null =>
        val tinfo = new HashMap[ConnectionIdentifier, ConnectionHolder]
        threadStore.set(tinfo)
        tinfo

      case v => v
    }
  }

  private def postCommit: List[() => Unit] =
    _postCommitFuncs.get match {
      case null =>
        _postCommitFuncs.set(Nil)
        Nil

      case v => v
    }

  private def postCommit_=(lst: List[() => Unit]): Unit = _postCommitFuncs.set(lst)

  // remove thread-local association
  private def clearThread(success: Boolean): Unit = {
    val ks = info.keySet
    if (ks.isEmpty) {
      postCommit.foreach(f => tryo(f.apply()))

      _postCommitFuncs.remove
      threadStore.remove
    } else {
      ks.foreach(n => releaseConnectionNamed(n, !success))
      clearThread(success)
    }
  }

  private def newConnection(name: ConnectionIdentifier): SuperConnection = {
    def cmSuperConnection(cm: ConnectionManager): Box[SuperConnection] =
      cm.newSuperConnection(name) or cm.newConnection(name).map(c => new SuperConnection(c, () => cm.releaseConnection(c)))

    def jndiSuperConnection: Box[SuperConnection] = jndiConnection(name).map(c => {
      val uniqueId = if (logger.isDebugEnabled) Helpers.nextNum.toString else ""
      logger.debug("Connection ID " + uniqueId + " for JNDI connection " + name.jndiName + " opened")
      new SuperConnection(c, () => {logger.debug("Connection ID " + uniqueId + " for JNDI connection " + name.jndiName + " closed"); c.close})
    })


    val cmConn = for {
      connectionManager <- threadLocalConnectionManagers.box.flatMap(_.get(name)) or Box(connectionManagers.get(name))
      connection <- cmSuperConnection(connectionManager)
    } yield connection

    val ret = cmConn or jndiSuperConnection

    ret.foreach (_.setAutoCommit(false))

    ret openOr {
      throw new NullPointerException("Looking for Connection Identifier " + name + " but failed to find either a JNDI data source " +
                                       "with the name " + name.jndiName + " or a lift connection manager with the correct name")
    }
  }

  private class ThreadBasedConnectionManager(connections: List[ConnectionIdentifier]) {
    private var used: Set[ConnectionIdentifier] = Set()

    def use(conn: ConnectionIdentifier): Int = if (connections.contains(conn)) {
      used += conn
      1
    } else 0
  }

  private object CurrentConnectionSet extends DynoVar[ThreadBasedConnectionManager]

  /**
   * Build a LoanWrapper to pass into S.addAround() to make requests for
   * the DefaultConnectionIdentifier transactional for the complete HTTP request
   */
  def buildLoanWrapper(): LoanWrapper =
  buildLoanWrapper(List(DefaultConnectionIdentifier))

  /**
   * Build a LoanWrapper to pass into S.addAround() to make requests for
   * the List of ConnectionIdentifiers transactional for the complete HTTP request
   */
  def buildLoanWrapper(in: List[ConnectionIdentifier]): LoanWrapper =
  buildLoanWrapper(true, in)

  /**
   * Build a LoanWrapper to pass into S.addAround() to make requests for
   * the DefaultConnectionIdentifier transactional for the complete HTTP request
   */
  def buildLoanWrapper(eager: Boolean): LoanWrapper =
  buildLoanWrapper(eager, List(DefaultConnectionIdentifier))

  /**
   * Build a LoanWrapper to pass into S.addAround() to make requests for
   * the List of ConnectionIdentifiers transactional for the complete HTTP request
   */
  def buildLoanWrapper(eager: Boolean, in: List[ConnectionIdentifier]): LoanWrapper =
    new LoanWrapper {
      private object DepthCnt extends DynoVar[Boolean]

      def apply[T](f: => T): T = if (DepthCnt.is == Full(true)) f
      else DepthCnt.run(true) {

        var success = false
        if (eager) {
          def recurseMe(lst: List[ConnectionIdentifier]): T = lst match {
            case Nil =>
              try {
                try {
                  val ret = f
                  success = !S.exceptionThrown_?
                  ret
                } catch {
                  // this is the case when we want to commit the transaction
                  // but continue to throw the exception
                  case e: LiftFlowOfControlException => {
                    success = !S.exceptionThrown_?
                    throw e
                  }
                }

              } finally {
                clearThread(success)
              }

            case x :: xs => DB.use(x) {ignore => recurseMe(xs)}
          }
          recurseMe(in)
        } else {
          CurrentConnectionSet.run(new ThreadBasedConnectionManager(in)) {
            try {
              try {
                val ret = f
                success = !S.exceptionThrown_?
                ret
              } catch {
                // this is the case when we want to commit the transaction
                // but continue to throw the exception
                case e: LiftFlowOfControlException => {
                  success = !S.exceptionThrown_?
                  throw e
                }
              }
            } finally {
              clearThread(success)
            }
          }
        }

      }
    }

  private def releaseConnection(conn: SuperConnection): Unit = conn.close

  private def calcBaseCount(conn: ConnectionIdentifier): Int =
  CurrentConnectionSet.is.map(_.use(conn)) openOr 0

  private def getConnection(name: ConnectionIdentifier): SuperConnection = {
    logger.trace("Acquiring " + name + " On thread " + Thread.currentThread)
    var ret = info.get(name) match {
      case None => ConnectionHolder(newConnection(name), calcBaseCount(name) + 1, Nil, false)
      case Some(ConnectionHolder(conn, cnt, post, rb)) => ConnectionHolder(conn, cnt + 1, post, rb)
    }
    info(name) = ret
    logger.trace("Acquired " + name + " on thread " + Thread.currentThread +
              " count " + ret.cnt)
    ret.conn
  }

  private def releaseConnectionNamed(name: ConnectionIdentifier, rollback: Boolean) {
    logger.trace("Request to release %s on thread %s, auto rollback=%s".format(name,Thread.currentThread, rollback))

    (info.get(name): @unchecked) match {
      case Some(ConnectionHolder(c, 1, post, manualRollback)) => {
        // stale and unexpectedly closed connections may throw here
        try {
          if (! (c.getAutoCommit() || manualRollback)) {
            if (rollback) c.rollback
            else c.commit
          }
        } catch {
          case e: SQLException =>
            logger.error("Swallowed exception during connection release. ", e)
        } finally {
          tryo(c.releaseFunc())
          info -= name
          val rolledback = rollback | manualRollback
          logger.trace("Invoking %d postTransaction functions. rollback=%s".format(post.size, rolledback))
          post.reverse.foreach(f => tryo(f(!rolledback)))
          logger.trace("Released %s on thread %s".format(name,Thread.currentThread))
        }
      }
      case Some(ConnectionHolder(c, n, post, rb)) =>
        logger.trace("Did not release " + name + " on thread " + Thread.currentThread + " count " + (n - 1))
        info(name) = ConnectionHolder(c, n - 1, post, rb)
      case x =>
        // ignore
    }
  }

  /**
   * Append a function to be invoked after the transaction on the specified connection identifier has ended.
   * The value passed to the function indicates true for success/commit or false for failure/rollback.
   *
   * Note: the function will only be called when automatic transaction management is in effect, either by executing within
   * the context of a buildLoanWrapper or a DB.use {}
   */
  def appendPostTransaction(name: ConnectionIdentifier, func: Boolean => Unit) {
    info.get(name) match {
      case Some(ConnectionHolder(c, n, post, rb)) =>
        info(name) = ConnectionHolder(c, n, func :: post, rb)
        logger.trace("Appended postTransaction function on %s, new count=%d".format(name, post.size+1))
      case _ => throw new IllegalStateException("Tried to append postTransaction function on illegal ConnectionIdentifer or outside transaction context")
    }
  }

  /**
   * Append function to be invoked after the current transaction on DefaultConnectionIdentifier has ended
   *
   */
  def appendPostTransaction(func: Boolean => Unit):Unit = appendPostTransaction(DefaultConnectionIdentifier, func)

  private def runLogger(logged: Statement, time: Long) = logged match {
    case st: DBLog => logFuncs.foreach(_(st, time))
    case _ => // NOP
  }

  def statement[T](db: SuperConnection)(f: (Statement) => T): T = {
    Helpers.calcTime {
      val st =
      if (loggingEnabled_?) {
        DBLog.createStatement(db.connection)
      } else {
        db.createStatement
      }

      queryTimeout.foreach(to => st.setQueryTimeout(to))
      try {
        (st, f(st))
      } finally {
        st.close
      }
    } match {
      case (time, (query, res)) => runLogger(query, time); res
    }
  }

  def exec[T](db: SuperConnection, query: String)(f: (ResultSet) => T): T =
  statement(db) {
    st =>
    f(st.executeQuery(query))
  }

  private def asString(pos: Int, rs: ResultSet, md: ResultSetMetaData): String = {
    import java.sql.Types._
    md.getColumnType(pos) match {
      case ARRAY | BINARY | BLOB | DATALINK | DISTINCT | JAVA_OBJECT | LONGVARBINARY | NULL | OTHER | REF | STRUCT | VARBINARY => rs.getObject(pos) match {
        case null => null
        case s => s.toString
      }

      case DECIMAL | NUMERIC =>
        rs.getBigDecimal(pos) match {
          case null => null
          case x => x.toString
        }

      case BIGINT | INTEGER | /* DECIMAL | NUMERIC | */ SMALLINT | TINYINT => checkNull(rs, pos, rs.getLong(pos).toString)

      case BIT | BOOLEAN => checkNull(rs, pos, rs.getBoolean(pos).toString)

      case VARCHAR | CHAR | CLOB | LONGVARCHAR => rs.getString(pos)

      case DATE | TIME | TIMESTAMP => rs.getTimestamp(pos) match {
        case null => null
        case x => x.toString
      }

      case DOUBLE | FLOAT | REAL => checkNull(rs, pos, rs.getDouble(pos).toString)
    }
  }

  /*
   If the column is null, return null rather than the boxed primitive
   */
  def checkNull[T](rs: ResultSet, pos: Int, res: => T): T = {
    if (null eq rs.getObject(pos)) null.asInstanceOf[T]
    else res
  }

  private def asAny(pos: Int, rs: ResultSet, md: ResultSetMetaData): Any = {
    import java.sql.Types._
    md.getColumnType(pos) match {
      case ARRAY | BINARY | BLOB | DATALINK | DISTINCT | JAVA_OBJECT | LONGVARBINARY | NULL | OTHER | REF | STRUCT | VARBINARY => rs.getObject(pos)

      case DECIMAL | NUMERIC => rs.getBigDecimal(pos)

      case BIGINT | INTEGER | /* DECIMAL | NUMERIC | */ SMALLINT | TINYINT => checkNull(rs, pos, rs.getLong(pos))

      case BIT | BOOLEAN => checkNull(rs, pos, rs.getBoolean(pos))

      case VARCHAR | CHAR | CLOB | LONGVARCHAR => rs.getString(pos)

      case DATE | TIME | TIMESTAMP => rs.getTimestamp(pos)

      case DOUBLE | FLOAT | REAL => checkNull(rs, pos, rs.getDouble(pos))
    }
  }

  def resultSetTo(rs: ResultSet): (List[String], List[List[String]]) = {
    val md = rs.getMetaData
    val cnt = md.getColumnCount
    val cntList = (1 to cnt).toList
    val colNames = cntList.map(i => md.getColumnName(i))

    val lb = new ListBuffer[List[String]]()

    while (rs.next) {
      lb += cntList.map(i => asString(i, rs, md))
    }

    (colNames, lb.toList)
  }

  def resultSetToAny(rs: ResultSet): (List[String], List[List[Any]]) = {
    val md = rs.getMetaData
    val cnt = md.getColumnCount
    val cntList = (1 to cnt).toList
    val colNames = cntList.map(i => md.getColumnName(i))

    val lb = new ListBuffer[List[Any]]()

    while (rs.next) {
      lb += cntList.map(i => asAny(i, rs, md))
    }

    (colNames, lb.toList)
  }

  /*
   * This method handles the common task of setting arguments on a prepared
   * statement based on argument type. Returns the properly updated PreparedStatement.
   */
  private def setPreparedParams(ps : PreparedStatement, params: List[Any]): PreparedStatement = {
    params.zipWithIndex.foreach {
      case (null, idx) => ps.setNull(idx + 1, Types.VARCHAR)
      case (i: Int, idx) => ps.setInt(idx + 1, i)
      case (l: Long, idx) => ps.setLong(idx + 1, l)
      case (d: Double, idx) => ps.setDouble(idx + 1, d)
      case (f: Float, idx) => ps.setFloat(idx + 1, f)
      // Allow the user to specify how they want the Date handled based on the input type
      case (t: java.sql.Timestamp, idx) => ps.setTimestamp(idx + 1, t)
      case (d: java.sql.Date, idx) => ps.setDate(idx + 1, d)
      case (t: java.sql.Time, idx) => ps.setTime(idx + 1, t)
      /* java.util.Date has to go last, since the java.sql date/time classes subclass it. By default we
       * assume a Timestamp value */
      case (d: java.util.Date, idx) => ps.setTimestamp(idx + 1, new java.sql.Timestamp(d.getTime))
      case (b: Boolean, idx) => ps.setBoolean(idx + 1, b)
      case (s: String, idx) => ps.setString(idx + 1, s)
      case (bn: java.math.BigDecimal, idx) => ps.setBigDecimal(idx + 1, bn)
      case (obj, idx) => ps.setObject(idx + 1, obj)
    }
    ps
  }

  /**
   * Executes the given parameterized query string with the given parameters.
   * Parameters are substituted in order. For Date/Time types, passing a java.util.Date will result in a
   * Timestamp parameter. If you want a specific SQL Date/Time type, use the corresponding
   * java.sql.Date, java.sql.Time, or java.sql.Timestamp classes.
   */
  def runQuery(query: String, params: List[Any]): (List[String], List[List[String]]) =
  runQuery(query, params, DefaultConnectionIdentifier)

  /**
   * Executes the given parameterized query string with the given parameters.
   * Parameters are substituted in order. For Date/Time types, passing a java.util.Date will result in a
   * Timestamp parameter. If you want a specific SQL Date/Time type, use the corresponding
   * java.sql.Date, java.sql.Time, or java.sql.Timestamp classes.
   */
  def runQuery(query: String, params: List[Any], connectionIdentifier: ConnectionIdentifier): (List[String], List[List[String]]) = {
    use(connectionIdentifier)(conn => prepareStatement(query, conn) {
        ps => resultSetTo(setPreparedParams(ps, params).executeQuery)
      })
  }

  /**
   * Executes the given parameterized query string with the given parameters.
   * Parameters are substituted in order. For Date/Time types, passing a java.util.Date will result in a
   * Timestamp parameter. If you want a specific SQL Date/Time type, use the corresponding
   * java.sql.Date, java.sql.Time, or java.sql.Timestamp classes.
   */
  def performQuery(query: String, params: List[Any]): (List[String], List[List[Any]]) =
  performQuery(query, params, DefaultConnectionIdentifier)

  /**
   * Executes the given parameterized query string with the given parameters.
   * Parameters are substituted in order. For Date/Time types, passing a java.util.Date will result in a
   * Timestamp parameter. If you want a specific SQL Date/Time type, use the corresponding
   * java.sql.Date, java.sql.Time, or java.sql.Timestamp classes.
   */
  def performQuery(query: String, params: List[Any], connectionIdentifier: ConnectionIdentifier): (List[String], List[List[Any]]) = {
    use(connectionIdentifier)(conn => prepareStatement(query, conn) {
        ps => resultSetToAny(setPreparedParams(ps, params).executeQuery)
      })
  }

  /**
   * Executes the given parameterized update string with the given parameters.
   * Parameters are substituted in order. For Date/Time types, passing a java.util.Date will result in a
   * Timestamp parameter. If you want a specific SQL Date/Time type, use the corresponding
   * java.sql.Date, java.sql.Time, or java.sql.Timestamp classes.
   */
  def runUpdate(query: String, params: List[Any]): Int =
  runUpdate(query, params, DefaultConnectionIdentifier)

  /**
   * Executes the given parameterized update string with the given parameters.
   * Parameters are substituted in order. For Date/Time types, passing a java.util.Date will result in a
   * Timestamp parameter. If you want a specific SQL Date/Time type, use the corresponding
   * java.sql.Date, java.sql.Time, or java.sql.Timestamp classes.
   */
  def runUpdate(query: String, params: List[Any], connectionIdentifier: ConnectionIdentifier): Int = {
    use(connectionIdentifier)(conn => prepareStatement(query, conn) {
        ps => setPreparedParams(ps, params).executeUpdate
      })
  }

  def runQuery(query: String): (List[String], List[List[String]]) =
    use(DefaultConnectionIdentifier)(conn => exec(conn, query)(resultSetTo))


  def performQuery(query: String): (List[String], List[List[Any]]) =
    use(DefaultConnectionIdentifier)(conn => exec(conn, query)(resultSetToAny))


  def rollback(name: ConnectionIdentifier): Unit = {
    info.get(name) match {
      case Some(ConnectionHolder(c, n, post, _)) =>
        info(name) = ConnectionHolder(c, n, post, true)
        logger.trace("Manual rollback on %s".format(name))
        use(name)(conn => conn.rollback)
      case _ => throw new IllegalStateException("Tried to rollback transaction on illegal ConnectionIdentifer or outside transaction context")
    }
  }

  def rollback: Unit = rollback(DefaultConnectionIdentifier)

  /**
   * Executes  { @code statement } and converts the  { @code ResultSet } to model
   * instance  { @code T } using  { @code f }
   */
  def exec[T](statement: PreparedStatement)(f: (ResultSet) => T): T = {
    queryTimeout.foreach(to => statement.setQueryTimeout(to))
    val rs = statement.executeQuery
    try {
      f(rs)
    } finally {
      statement.close
      rs.close
    }
  }

  /**
   * Prepares the given statement and then passes it to the given function for use. This method
   * represents a loan pattern, and will automatically handle creation and closing of the
   * PreparedStatement.
   */
  def prepareStatement[T](statement: String, conn: SuperConnection)(f: (PreparedStatement) => T): T = {
    val st =
    if (loggingEnabled_?) {
      DBLog.prepareStatement(conn.connection, statement)
    } else {
      conn.prepareStatement(statement)
    }
    runPreparedStatement(st)(f)
  }

  /**
   * Prepares the given statement and then passes it to the given function for use. This method
   * represents a loan pattern, and will automatically handle creation and closing of the
   * PreparedStatement.
   *
   * Retrieval of generated keys is controlled with the autokeys parameter, corresponding to the
   * constants defined on java.sql.Statement: RETURN_GENERATED_KEYS or NO_GENERATED_KEYS
   */
  def prepareStatement[T](statement: String, autokeys: Int, conn: SuperConnection)(f: (PreparedStatement) => T): T = {
    val st =
    if (loggingEnabled_?) {
      DBLog.prepareStatement(conn.connection, statement, autokeys)
    } else {
      conn.prepareStatement(statement, autokeys)
    }
    runPreparedStatement(st)(f)
  }

  /**
   * Prepares the given statement and then passes it to the given function for use. This method
   * represents a loan pattern, and will automatically handle creation and closing of the
   * PreparedStatement.
   *
   * If the driver supports it, generated keys for the given column indices can be retrieved.
   */
  def prepareStatement[T](statement: String, autoColumns: Array[Int], conn: SuperConnection)(f: (PreparedStatement) => T): T = {
    val st =
    if (loggingEnabled_?) {
      DBLog.prepareStatement(conn.connection, statement, autoColumns)
    } else {
      conn.prepareStatement(statement, autoColumns)
    }
    runPreparedStatement(st)(f)
  }

  /**
   * Prepares the given statement and then passes it to the given function for use. This method
   * represents a loan pattern, and will automatically handle creation and closing of the
   * PreparedStatement.
   *
   * If the driver supports it, generated keys for the given column names can be retrieved.
   */
  def prepareStatement[T](statement: String, autoColumns: Array[String], conn: SuperConnection)(f: (PreparedStatement) => T): T = {
    val st =
    if (loggingEnabled_?) {
      DBLog.prepareStatement(conn.connection, statement, autoColumns)
    } else {
      conn.prepareStatement(statement, autoColumns)
    }
    runPreparedStatement(st)(f)
  }

  private def runPreparedStatement[T](st: PreparedStatement)(f: (PreparedStatement) => T): T = {
    queryTimeout.foreach(to => st.setQueryTimeout(to))
    Helpers.calcTime {
      try {
        (st,f(st))
      } finally {
        st.close
      }
    } match {
      case (time, (query, res)) => runLogger(query, time); res
    }
  }

  private object currentConn extends DynoVar[SuperConnection]

  def currentConnection: Box[SuperConnection] = currentConn.is

  /**
   * Executes function  { @code f } with the connection named  { @code name }. Releases the connection
   * before returning.
   *
   * Only use within a stateful request
   */
  def use[T](name: ConnectionIdentifier)(f: (SuperConnection) => T): T = {
    val conn = getConnection(name)
    currentConn.run(conn) {
      var rollback = true
      try {
        val ret = f(conn)
        rollback = S.exceptionThrown_?
        ret
      } catch {
        // this is the case when we want to commit the transaction
        // but continue to throw the exception
        case e: LiftFlowOfControlException => {
          rollback = S.exceptionThrown_?
          throw e
        }
      } finally {
        releaseConnectionNamed(name, rollback)
      }
    }
  }

  /**
  * The SQL reserved words.  These words will be changed if they are used for column or table names.
  */
  def reservedWords:  scala.collection.immutable.Set[String] = userReservedWords openOr defaultReservedWords

  /**
  *  If you need to change some of the reserved word, you can supply your own set in Boot.scala:
  * DB.userReservedWords = Full(Set("foo", "bar"))
  */
  @volatile var userReservedWords: Box[ scala.collection.immutable.Set[String]] = Empty


  /**
  * The default reserved words.
  *
  * TODO : Maybe this should be refactored to allow for driver-specific reserved words
  */
  lazy val defaultReservedWords:  scala.collection.immutable.Set[String] = scala.collection.immutable.HashSet("abort",
       "accept",
       "access",
       "add",
       "admin",
       "after",
       "all",
       "allocate",
       "alter",
       "analyze",
       "and",
       "any",
       "archive",
       "archivelog",
       "array",
       "arraylen",
       "as",
       "asc",
       "assert",
       "assign",
       "at",
       "audit",
       "authorization",
       "avg",
       "backup",
       "base_table",
       "become",
       "before",
       "begin",
       "between",
       "binary_integer",
       "blob",
       "block",
       "body",
       "boolean",
       "by",
       "cache",
       "cancel",
       "cascade",
       "case",
       "change",
       "char",
       "character",
       "char_base",
       "check",
       "checkpoint",
       "close",
       "cluster",
       "clusters",
       "cobol",
       "colauth",
       "column",
       "columns",
       "comment",
       "commit",
       "compile",
       "compress",
       "connect",
       "constant",
       "constraint",
       "constraints",
       "contents",
       "continue",
       "controlfile",
       "count",
       "crash",
       "create",
       "current",
       "currval",
       "cursor",
       "cycle",
       "database",
       "data_base",
       "datafile",
       "date",
       "dba",
       "debugoff",
       "debugon",
       "dec",
       "decimal",
       "declare",
       "default",
       "definition",
       "delay",
       "delete",
       "delta",
       "desc",
       "digits",
       "disable",
       "dismount",
       "dispose",
       "distinct",
       "do",
       "double",
       "drop",
       "dump",
       "each",
       "else",
       "elsif",
       "enable",
       "end",
       "entry",
       "escape",
       "events",
       "except",
       "exception",
       "exception_init",
       "exceptions",
       "exclusive",
       "exec",
       "execute",
       "exists",
       "exit",
       "explain",
       "extent",
       "externally",
       "false",
       "fetch",
       "file",
       "float",
       "flush",
       "for",
       "force",
       "foreign",
       "form",
       "fortran",
       "found",
       "freelist",
       "freelists",
       "from",
       "function",
       "generic",
       "go",
       "goto",
       "grant",
       "group",
       "having",
       "identified",
       "if",
       "immediate",
       "in",
       "including",
       "increment",
       "index",
       "indexes",
       "indicator",
       "initial",
       "initrans",
       "insert",
       "instance",
       "notnull", // reserved word for PostgreSQL
       "int",
       "integer",
       "intersect",
       "into",
       "is",
       "key",
       "language",
       "layer",
       "level",
       "like",
       "limit", // reserved word for PostgreSQL
       "limited",
       "link",
       "lists",
       "lock",
       "logfile",
       "long",
       "loop",
       "manage",
       "manual",
       "max",
       "maxdatafiles",
       "maxextents",
       "maxinstances",
       "maxlogfiles",
       "maxloghistory",
       "maxlogmembers",
       "maxtrans",
       "maxvalue",
       "min",
       "minextents",
       "minus",
       "minvalue",
       "mlslabel",
       "mod",
       "mode",
       "modify",
       "module",
       "mount",
       "natural",
       "new",
       "next",
       "nextval",
       "noarchivelog",
       "noaudit",
       "nocache",
       "nocompress",
       "nocycle",
       "nomaxvalue",
       "nominvalue",
       "none",
       "noorder",
       "noresetlogs",
       "normal",
       "nosort",
       "not",
       "notfound",
       "nowait",
       "null",
       "number",
       "number_base",
       "numeric",
       "of",
       "off",
       "offline",
       "old",
       "on",
       "online",
       "only",
       "open",
       "optimal",
       "option",
       "or",
       "order",
       "others",
       "out",
       "own",
       "package",
       "parallel",
       "partition",
       "pctfree",
       "pctincrease",
       "pctused",
       "plan",
       "pli",
       "positive",
       "pragma",
       "precision",
       "primary",
       "prior",
       "private",
       "privileges",
       "procedure",
       "profile",
       "public",
       "quota",
       "raise",
       "range",
       "raw",
       "read",
       "real",
       "record",
       "recover",
       "references",
       "referencing",
       "release",
       "remr",
       "rename",
       "resetlogs",
       "resource",
       "restricted",
       "return",
       "reuse",
       "reverse",
       "revoke",
       "role",
       "roles",
       "rollback",
       "row",
       "rowid",
       "rowlabel",
       "rownum",
       "rows",
       "rowtype",
       "run",
       "savepoint",
       "schema",
       "scn",
       "section",
       "segment",
       "select",
       "separate",
       "sequence",
       "session",
       "set",
       "share",
       "shared",
       "show", // MySQL reserved word
       "size",
       "smallint",
       "snapshot",
       "some",
       "sort",
       "space",
       "sql",
       "sqlbuf",
       "sqlcode",
       "sqlerrm",
       "sqlerror",
       "sqlstate",
       "start",
       "statement",
       "statement_id",
       "statistics",
       "stddev",
       "stop",
       "storage",
       "subtype",
       "successful",
       "sum",
       "switch",
       "synonym",
       "sysdate",
       "system",
       "tabauth",
       "table",
       "tables",
       "tablespace",
       "task",
       "temporary",
       "terminate",
       "then",
       "thread",
       "time",
       "timestamp", // reserved in Oracle
       "to",
       "tracing",
       "transaction",
       "trigger",
       "triggers",
       "true",
       "truncate",
       "type",
       "uid",
       "under",
       "union",
       "unique",
       "unlimited",
       "until",
       "update",
       "use",
       "user",
       "using",
       "validate",
       "values",
       "varchar",
       "varchar2",
       "variance",
       "view",
       "views",
       "when",
       "whenever",
       "where",
       "while",
       "with",
       "work",
       "write",
       "xor")
}

class SuperConnection(val connection: Connection, val releaseFunc: () => Unit, val schemaName: Box[String]) {
  def this(c: Connection, rf: () => Unit) = this (c, rf, Empty)

  lazy val brokenLimit_? = driverType.brokenLimit_?

  def createTablePostpend: String = driverType.createTablePostpend

  def supportsForeignKeys_? : Boolean = driverType.supportsForeignKeys_?

  lazy val driverType: DriverType = DriverType.calcDriver(connection)

  lazy val metaData = connection.getMetaData
}

object SuperConnection {
  implicit def superToConn(in: SuperConnection): Connection = in.connection
}

/**
 * The standard DB vendor.
 * @param driverName the name of the database driver
 * @param dbUrl the URL for the JDBC data connection
 * @param dbUser the optional username
 * @param dbPassword the optional db password
 */
class StandardDBVendor(driverName: String,
                       dbUrl: String,
                       dbUser: Box[String],
                       dbPassword: Box[String]) extends ProtoDBVendor {

  private val logger = Logger(classOf[StandardDBVendor])

  protected def createOne: Box[Connection] =  {
    tryo{t:Throwable => logger.error("Cannot load database driver: %s".format(driverName), t)}{Class.forName(driverName);()}

    (dbUser, dbPassword) match {
      case (Full(user), Full(pwd)) =>
        tryo{t:Throwable => logger.error("Unable to get database connection. url=%s, user=%s".format(dbUrl, user),t)}(DriverManager.getConnection(dbUrl, user, pwd))
      case _ =>
        tryo{t:Throwable => logger.error("Unable to get database connection. url=%s".format(dbUrl),t)}(DriverManager.getConnection(dbUrl))
    }
  }
}

trait ProtoDBVendor extends ConnectionManager {
  private val logger = Logger(classOf[ProtoDBVendor])
  private var pool: List[Connection] = Nil
  private var poolSize = 0
  private var tempMaxSize = maxPoolSize

  /**
   * Override and set to false if the maximum pool size can temporarilly be expanded to avoid pool starvation
   */
  protected def allowTemporaryPoolExpansion = true

  /**
   *  Override this method if you want something other than
   * 4 connections in the pool
   */
  protected def maxPoolSize = 4

  /**
   * The absolute maximum that this pool can extend to
   * The default is 20.  Override this method to change.
   */
  protected def doNotExpandBeyond = 20

  /**
   * The logic for whether we can expand the pool beyond the current size.  By
   * default, the logic tests allowTemporaryPoolExpansion &amp;&amp; poolSize &lt;= doNotExpandBeyond
   */
  protected def canExpand_? : Boolean = allowTemporaryPoolExpansion && poolSize <= doNotExpandBeyond

  /**
   *   How is a connection created?
   */
  protected def createOne: Box[Connection]

  /**
   * Test the connection.  By default, setAutoCommit(false),
   * but you can do a real query on your RDBMS to see if the connection is alive
   */
  protected def testConnection(conn: Connection) {
    conn.setAutoCommit(false)
  }

  def newConnection(name: ConnectionIdentifier): Box[Connection] =
    synchronized {
      pool match {
        case Nil if poolSize < tempMaxSize =>
          val ret = createOne
          ret.foreach(_.setAutoCommit(false))
          poolSize = poolSize + 1
          logger.debug("Created new pool entry. name=%s, poolSize=%d".format(name, poolSize))
          ret

        case Nil =>
          val curSize = poolSize
          logger.trace("No connection left in pool, waiting...")
          wait(50L)
          // if we've waited 50 ms and the pool is still empty, temporarily expand it
          if (pool.isEmpty && poolSize == curSize && canExpand_?) {
            tempMaxSize += 1
            logger.debug("Temporarily expanding pool. name=%s, tempMaxSize=%d".format(name, tempMaxSize))
          }
          newConnection(name)

        case x :: xs =>
          logger.trace("Found connection in pool, name=%s".format(name))
          pool = xs
          try {
            this.testConnection(x)
            Full(x)
          } catch {
            case e: Exception => try {
              logger.debug("Test connection failed, removing connection from pool, name=%s".format(name))
              poolSize = poolSize - 1
              tryo(x.close)
              newConnection(name)
            } catch {
              case e: Exception => newConnection(name)
            }
          }
      }
    }

  def releaseConnection(conn: Connection): Unit = synchronized {
    if (tempMaxSize > maxPoolSize) {
      tryo {conn.close()}
      tempMaxSize -= 1
      poolSize -= 1
    } else {
      pool = conn :: pool
    }
    logger.debug("Released connection. poolSize=%d".format(poolSize))
    notifyAll
  }

  def closeAllConnections_!(): Unit = _closeAllConnections_!(0)


  private def _closeAllConnections_!(cnt: Int): Unit = synchronized {
    logger.info("Closing all connections")
    if (poolSize <= 0 || cnt > 10) ()
    else {
      pool.foreach {c => tryo(c.close); poolSize -= 1}
      pool = Nil

      if (poolSize > 0) wait(250)

      _closeAllConnections_!(cnt + 1)
    }
  }
}
