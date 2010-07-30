/*
 * Copyright 2006-2010 WorldWide Conferencing, LLC
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

package net.liftweb {
package mapper {

import _root_.java.lang.reflect.{InvocationHandler,Method,Proxy}
import _root_.java.sql.{Array => SqlArray, _}

import _root_.net.liftweb.util._
import _root_.net.liftweb.common.{Box,Loggable}

trait DBLogEntry {
  def statement : String
  def duration : Long
}
object DBLogEntry {
  def unapply(obj : Any) = obj match {
    case entry : DBLogEntry => Some(entry.statement,entry.duration)
    case _ => None
  }
}
case class DBStatementEntry(statement : String, duration : Long) extends DBLogEntry
case class DBMetaEntry(statement : String, duration : Long) extends DBLogEntry

/**
 * This trait is applied to JDBC statements and similar constructs that can log operations.
 *
 * To enable logging of DB operations, use DB.addLogFunc
 */
trait DBLog {
  protected var executedStatements = List[DBLogEntry]()

  /*
  Some convenience methods to simplify the statements. We defined methods that can either take a raw description,
  or a function that can use the result of the operation to construct a description.
  */
  protected def logStatement[T](description : String)(f : => T) : T = logStatement({ignore : T => description})(f)

  protected def logStatement[T](description : T => String)(f : => T) : T = Helpers.calcTime(f) match {
      case (duration, result) => executedStatements ::= DBStatementEntry(description(result), duration); result
  }

  protected def logMeta[T](description : String)(f : => T) : T = logMeta({ignore : T => description})(f)

  protected def logMeta[T](description : T => String)(f : => T) : T = Helpers.calcTime(f) match {
      case (duration, result) => executedStatements ::= DBMetaEntry(description(result), duration); result
  }

  /** Return a list of all of the DBStatementEntry instances in the log buffer */
  def statementEntries : List[DBStatementEntry] = executedStatements.filter(_.isInstanceOf[DBStatementEntry]).reverse.asInstanceOf[List[DBStatementEntry]]

  /** Return a list of all of the DBMetaEntry instances in the log buffer */
  def metaEntries : List[DBMetaEntry] = executedStatements.filter(_.isInstanceOf[DBMetaEntry]).reverse.asInstanceOf[List[DBMetaEntry]]

  /** Return all log buffer entries */
  def allEntries : List[DBLogEntry] = executedStatements.reverse
}

object DBLog {
  def createStatement (conn : Connection) = {
    val stmt = conn.createStatement
    Proxy.newProxyInstance(this.getClass.getClassLoader,
                           Array(classOf[java.sql.Statement], classOf[DBLog]),
                           new LoggedStatementHandler(stmt)).asInstanceOf[Statement]
  }

  def prepareStatement (conn : Connection, query : String) =
    proxyPreparedStatement(conn.prepareStatement(query), query)

  def prepareStatement (conn : Connection, query : String, autoKeys : Int) =
    proxyPreparedStatement(conn.prepareStatement(query, autoKeys), query)

  def prepareStatement (conn : Connection, query : String, autoKeys : Array[Int]) =
    proxyPreparedStatement(conn.prepareStatement(query, autoKeys), query)

  def prepareStatement (conn : Connection, query : String, autoKeys : Array[String]) =
    proxyPreparedStatement(conn.prepareStatement(query, autoKeys), query)

  private def proxyPreparedStatement(stmt : => PreparedStatement, query : String) = {
    try {
      Proxy.newProxyInstance(this.getClass.getClassLoader,
                             Array(classOf[java.sql.PreparedStatement], classOf[DBLog]),
                             new LoggedPreparedStatementHandler(query, stmt)).asInstanceOf[PreparedStatement]
    } catch {
      case sqle : SQLException => throw new SQLException("Error preparing statement: \"%s\"".format(query), sqle)
    }
  }

  /**
   * This class corresponds to a logged version of java.sql.Statement. All operations
   * are supported via dynamic dispatch. This is done so that we can support both
   * JDBC3 and JDBC4 without having two code trees.
   *
   * To enable logging of DB operations, use DB.addLogFunc
   */
  sealed private[DBLog] class LoggedStatementHandler(underlying : Statement) extends InvocationHandler with DBLog with Loggable {
    def underlyingClassname = "java.sql.Statement"
    lazy val representative : Class[_] = Class.forName(underlyingClassname)

    def invoke (proxy : Object, method : Method, args : Array[Object]) : Object = method.getName match {
      // Handle DBLog methods first. We have to do this since the end user expects a DBLog interface
      // via the proxy.
      case "statementEntries" => this.statementEntries
      case "metaEntries" => this.metaEntries
      case "allEntries" => this.allEntries

      // The rest are from Statement
      case "addBatch" => {
        logStatement("Batched: \"%s\"".format(args(0))) {
          chain(method,  args)
        }
      }
      case "cancel" => {
        logMeta("Cancelled Statement") {
          chain(method,  Array())
        }
      }
      case "clearBatch" => {
        logMeta("Cleared Batch") {
          chain(method,  Array())
        }
      }
      case "clearWarnings" => {
        logMeta("Cleared Warnings") {
          chain(method,  Array())
        }
      }
      case "close" => {
        logMeta("Closed Statement") {
          chain(method,  Array())
        }
      }
      case "execute" if args.length == 1 => {
        logStatement({ret : Object => "\"%s\" : result = %s".format(args(0), ret)}) {
            chain(method,  args)
        }
      }
      case "execute" if args(1).getClass == classOf[Int]  => {
        logStatement({ret : Object => "Exec \"%s\", Auto-gen keys = %s : result = %s".format(args(0), StatementConstantDescriptions.genKeyDescriptions(args(1).asInstanceOf[Int]), ret)}) {
          chain(method,  args)
        }
      }
      case "execute" => {
        logStatement({ret : Object => "Exec \"%s\", Auto-gen keys for columns %s".format(args(0), args(1).asInstanceOf[Array[_]].mkString(", "), ret)}) {
            chain(method,  args)
        }
      }
      case "executeBatch" => {
        logStatement({result : Object => "Exec batch, counts = " + result.asInstanceOf[Array[Int]].mkString("(", ", ", ")")}) {
            chain(method,  Array())
        }
      }
      case "executeQuery" => {
        logStatement({rs : Object => "Exec query \"%s\" : rs = %s".format(args(0),rs)}) {
            chain(method,  args)
        }
      }
      case "executeUpdate" if args.length == 1 => {
        logStatement({count : Object => "Exec update \"%s\" : count = %d".format(args(0),count)}) {
            chain(method,  args)
        }
      }
      case "executeUpdate" if args(1).getClass == classOf[Int] => {
        logStatement({count : Object => "Exec update \"%s\", Auto-gen keys = %s".format(args(0), StatementConstantDescriptions.genKeyDescriptions(args(1).asInstanceOf[Int]), count)}) {
          chain(method,  args)
        }
      }
      case "executeUpdate" => {
        logStatement({count : Object => "Exec update \"%s\", Auto-gen keys for columns %s".format(args(0), args(1).asInstanceOf[Array[_]].mkString(", "), count)}) {
          chain(method,  args)
        }
      }
      case "getConnection" => {
        logMeta("Get underlying Connection") {
          chain(method,  Array())
        }
      }
      case "getFetchDirection" => {
        logMeta({ret : Object => "Get fetch direction : " + StatementConstantDescriptions.fetchDirDescriptions(ret.asInstanceOf[Int])}) {
            chain(method,  Array())
        }
      }
      case "getFetchSize" => {
        logMeta({size : Object => "Get fetch size : " + size}) {
            chain(method,  Array())
        }
      }
      case "getGeneratedKeys" => {
        logMeta({rs : Object => "Get generated keys : rs = " + rs}) {
            chain(method,  Array())
        }
      }
      case "getMaxFieldSize" => {
        logMeta({size : Object => "Get max field size : " + size}) {
            chain(method,  Array())
        }
      }
      case "getMaxRows" => {
        logMeta({maxRows : Object => "Get max rows : " + maxRows}) {
            chain(method,  Array())
        }
      }
      case "getMoreResults" if args.length == 0 => {
        logMeta({hasMore : Object => "Get more results : " + hasMore}) {
            chain(method,  Array())
        }
      }
      case "getMoreResults" => {
        logMeta({ret : Object => "Get more results (%s) : %s".format(StatementConstantDescriptions.getMoreResultsDescriptions(args(0).asInstanceOf[Int]), ret)}) {
            chain(method,  args)
        }
      }
      case "getQueryTimeout" => {
        logMeta({timeout : Object => "Get query timeout : %d seconds ".format(timeout)}) {
            chain(method,  Array())
        }
      }
      case "getResultSet" => {
        logMeta({rs : Object => "Get result set : " + rs}) {
            chain(method,  Array())
        }
      }
      case "getResultSetConcurrency" => {
        logMeta({ret : Object => "Get result set concurrency : " + StatementConstantDescriptions.resultSetConcurrencyDescs(ret.asInstanceOf[Int])}) {
            chain(method,  Array())
        }
      }
      case "getResultSetHoldability" => {
        logMeta({ret : Object => "Get ResultSet holdability : " + StatementConstantDescriptions.resultSetHoldabilityDescs(ret.asInstanceOf[Int])}) {
            chain(method,  Array())
        }
      }
      case "getResultSetType" => {
        logMeta({ret : Object => "Get ResultSet type : " + StatementConstantDescriptions.resultSetTypeDescs(ret.asInstanceOf[Int])}) {
            chain(method,  Array())
        }
      }
      case "getUpdateCount" => {
        logMeta({count : Object => "Get update count : " + count}) {
            chain(method,  Array())
        }
      }
      case "getWarnings" => {
        logMeta({ret : Object => "Get SQL Warnings: " + Box.!!(ret).map(_.toString).openOr("None")}) {
            chain(method,  Array())
        }
      }
      case "isClosed" => {
        logMeta({ret : Object => "Check isClosed : " + ret}) {
            chain(method,  Array())
        }
      }
      case "isPoolable" => {
        logMeta({ret : Object => "Check isPoolable : " + ret}) {
            chain(method,  Array())
        }
      }
      case "setCursorName" => {
        logMeta("Set cursor name = %s" + args(0)) {
            chain(method,  args)
        }
      }
      case "setEscapeProcessing" => {
        logMeta("Set escape processing = " + args(0)) {
            chain(method,  args)
        }
      }
      case "setFetchDirection" => {
        logMeta("Set fetch direction = " + StatementConstantDescriptions.fetchDirDescriptions(args(0).asInstanceOf[Int])) {
            chain(method,  args)
        }
      }
      case "setFetchSize" => {
        logMeta("Set fetch size = " + args(0)) {
            chain(method,  args)
        }
      }
      case "setMaxFieldSize" => {
        logMeta("Set max field size = " + args(0)) {
            chain(method,  args)
        }
      }
      case "setMaxRows" => {
        logMeta("Set max rows = " + args(0)) {
            chain(method,  args)
        }
      }
      case "setPoolable" => {
        logMeta("Set poolable = " + args(0)) {
            chain(method,  args)
        }
      }
      case "setQueryTimeout" => {
        logMeta("Set query timeout = " + args(0)) {
            chain(method,  args)
        }
      }
      case "toString" => {
        // We'll call into our own representation here
        this.toString
      }        
        
      // These are from wrapper and are required
      case "isWrapperFor" => args(0).getClass match {
        case `representative` => Boolean.box(true)
        case _ => chain(method,  args)
      }
      case "unwrap" => args(0).getClass match {
        case `representative` => underlying
        case _ => chain(method,  args)
      }

      case methodName => throw new NoSuchMethodException(methodName + " is not implemented here")
    }

    protected def chain(method : Method, args : Array[Object]) : Object =
    try {
      val m = representative.getMethod(method.getName, method.getParameterTypes : _*)

      m.invoke(underlying, args : _*)
    } catch {
      case ite: java.lang.reflect.InvocationTargetException => throw ite.getCause
      case nsme : NoSuchMethodException => logger.warn("Could not locate method %s for %s : %s".format(method.getName, underlyingClassname, nsme.getMessage))
      throw nsme
    }

    /* This toString only gets invoked if we target this instance as a
     * LoggedStatementHandler directly, or via the proxied "toString" above.
     */
    override def toString = "Logged Statements =\n" + executedStatements.reverse.map("  " + _).mkString("\n")
  }

  /**
   * This class corresponds to a logged version of java.sql.PreparedStatement. All operations
   * should be supported.
   *
   * To enable logging of DB operations, use DB.addLogFunc
   */
  sealed private[DBLog] class LoggedPreparedStatementHandler (stmt : String, underlying : PreparedStatement) extends LoggedStatementHandler(underlying) {
    override def underlyingClassname = "java.sql.PreparedStatement"

    private var paramMap = Map.empty[Int,Any]

    // utility method to fill in params
    private def paramified : String = {
      val sb = new StringBuilder(500)
      def substitute (in : String, index : Int): Unit = in.indexOf('?') match {
        case -1 => 
	  sb.append(in)

        case j => 
	  sb.append(in.substring(0,j))
	  sb.append(paramMap(index))
	  substitute(in.substring(j + 1), index + 1)
      }
      
      substitute(stmt, 1)
      sb.toString
    }

    override def invoke (proxy : Object, method : Method, args : Array[Object]) : Object = {
      method.getName match {
        // All of the simple cases can be handled in one spot
        case "setArray" | "setBigDecimal" | "setBoolean" | "setByte" |
             "setBytes" | "setDouble" | "setFloat" | "setInt" | "setLong" |
             "setNString" | "setRef" | "setRowId" | "setShort" | "setSQLXML"
             => {
          paramMap += args(0).asInstanceOf[Int] -> args(1)
          chain(method,  args)
        }

        // Everything else gets special treatment

        case "addBatch" => {
          logStatement("Batching \"%s\"".format(paramified)) {
              chain(method,  Array())
          }
        }

        case "clearParameters" => {
          paramMap = Map.empty[Int,Any]
          logMeta("Clear parameters") {
              chain(method,  Array())
          }
        }

        case "execute" => {
          logStatement({ret : Object => "Exec \"%s\" : %s".format(paramified, ret)}) {
              chain(method,  Array())
          }
        }

        case "executeQuery" => {
          logStatement({rs : Object => "Exec query \"%s\" : %s".format(paramified, rs)}) {
              chain(method,  Array())
          }
        }

        case "executeUpdate" => {
          logStatement({ret : Object => "Exec update \"%s\" : updated %d rows".format(paramified, ret)}) {
              chain(method,  Array())
          }
        }

        case "getMetaData" => {
          logMeta({ret : Object => "Get metadata : " + ret}) {
              chain(method,  Array())
          }
        }

        case "getParameterMetaData" => {
          logMeta({ret : Object => "Get param metadata : " + ret}) {
              chain(method,  Array())
          }
        }

        case "setAsciiStream" if args.length == 2 => {
            paramMap += args(0).asInstanceOf[Int] -> "(Ascii Stream: %s)".format(args(1))
            chain(method,  args)
        }

        case "setAsciiStream" => {
            paramMap += args(0).asInstanceOf[Int] -> "(Ascii Stream: %s (%d bytes))".format(args(1), args(2))
            chain(method,  args)
        }

        case "setBinaryStream" if args.length == 2 => {
            paramMap += args(0).asInstanceOf[Int] -> "(Binary Stream: %s)".format(args(1))
            chain(method,  args)
        }

        case "setBinaryStream" => {
            paramMap += args(0).asInstanceOf[Int] -> "(Binary Stream: %s (%d bytes))".format(args(1), args(2))
            chain(method,  args)
        }

        case "setBlob" if args.length == 2 => {
            paramMap += args(0).asInstanceOf[Int] -> "(Blob : %s)".format(args(1))
            chain(method,  args)
        }

        case "setBlob" => {
            paramMap += args(0).asInstanceOf[Int] -> "(Blob : %s (%d bytes))".format(args(1), args(2))
            chain(method,  args)
        }

        case "setCharacterStream" if args.length == 2 => {
            paramMap += args(0).asInstanceOf[Int] -> "(Char stream : %s)".format(args(1))
            chain(method,  args)
        }

        case "setCharacterStream" => {
            paramMap += args(0).asInstanceOf[Int] -> "(Char stream : %s (%d bytes))".format(args(1), args(2))
            chain(method,  args)
        }

        case "setClob" if args.length == 2 => {
            paramMap += args(0).asInstanceOf[Int] -> "(Clob : %s)".format(args(1))
            chain(method,  args)
        }

        case "setClob" => {
            paramMap += args(0).asInstanceOf[Int] -> "(Clob : %s (%d bytes))".format(args(1), args(2))
            chain(method,  args)
        }

        case "setDate" if args.length == 2 => {
            paramMap += args(0).asInstanceOf[Int] -> args(1)
            chain(method,  args)
        }

        case "setDate" => {
            paramMap += args(0).asInstanceOf[Int] -> (args(1) + ":" + args(2))
            chain(method,  args)
        }

        case "setNCharacterStream" if args.length == 2 => {
            paramMap += args(0).asInstanceOf[Int] -> "(NChar Stream : %s)".format(args(1))
            chain(method,  args)
        }

        case "setNCharacterStream" => {
            paramMap += args(0).asInstanceOf[Int] -> "(NChar Stream : %s (%d bytes))".format(args(1), args(2))
            chain(method,  args)
        }

        case "setNClob" if args.length == 2 => {
            paramMap += args(0).asInstanceOf[Int] -> "(NClob : %s)".format(args(1))
            chain(method,  args)
        }

        case "setNClob" => {
            paramMap += args(0).asInstanceOf[Int] -> "(NClob : %s (%d bytes))".format(args(1), args(2))
            chain(method,  args)
        }

        case "setNull" => {
            paramMap += args(0).asInstanceOf[Int] -> "NULL"
            chain(method,  args)
        }

        case "setObject" if (args.length >= 2 && args.length < 4) => {
            paramMap += args(0).asInstanceOf[Int] -> args(1)
            chain(method, args)
        }

        case "setObject" if args.length == 4 => {
            paramMap += args(0).asInstanceOf[Int] -> "%s (scale %d)".format(args(1), args(3))
            chain(method, args)
        }

        case "setString" => {
            paramMap += args(0).asInstanceOf[Int] -> "\"%s\"".format(args(1))
            chain(method,  args)
        }

        case "setTime" if args.length == 2 => {
            paramMap += args(0).asInstanceOf[Int] -> args(1)
            chain(method,  args)
        }

        case "setTime" => {
            paramMap += args(0).asInstanceOf[Int] -> (args(1) + ":" + args(2))
            chain(method,  args)
        }

        case "setTimestamp" if args.length == 2 => {
            paramMap += args(0).asInstanceOf[Int] -> args(1)
            chain(method,  args)
        }

        case "setTimestamp" => {
            paramMap += args(0).asInstanceOf[Int] -> (args(1) + ":" + args(2))
            chain(method,  args)
        }

        case "setUnicodeStream" => {
            paramMap += args(0).asInstanceOf[Int] -> "(Unicode Stream : %s (%d bytes))".format(args(1), args(2))
            chain(method,  args)
        }

        case "setURL" => {
            paramMap += args(0).asInstanceOf[Int] -> "\"%s\"".format(args(1))
            chain(method,  args)
        }

        // Chain up to LoggedStatement if we don't handle it here
        case _ => super.invoke(proxy, method, args)
      }
    }
  }
}

/**
 * This object defines some conversions from Int JDBC constants to
 * descriptive strings
 */
object StatementConstantDescriptions {
    def genKeyDescriptions (in : Int) = in match {
        case Statement.NO_GENERATED_KEYS => "NO_GENERATED_KEYS"
        case Statement.RETURN_GENERATED_KEYS => "RETURN_GENERATED_KEYS"
        case x => "Invalid Generated Keys Constant: " + x
    }

    def fetchDirDescriptions (in : Int) = in match {
        case ResultSet.FETCH_FORWARD => "FETCH_FORWARD"
        case ResultSet.FETCH_REVERSE => "FETCH_REVERSE"
        case ResultSet.FETCH_UNKNOWN => "FETCH_UNKNOWN"
        case x => "Invalid Fetch Direction Constant: " + x
    }

    def getMoreResultsDescriptions (in : Int) = in match {
        case Statement.CLOSE_CURRENT_RESULT => "CLOSE_CURRENT_RESULT"
        case Statement.KEEP_CURRENT_RESULT => "KEEP_CURRENT_RESULT"
        case Statement.CLOSE_ALL_RESULTS => "CLOSE_ALL_RESULTS"
        case x => "Invalid getMoreResults constant: " + x
    }

    def resultSetConcurrencyDescs (in : Int) = in match {
        case ResultSet.CONCUR_READ_ONLY => "CONCUR_READ_ONLY"
        case ResultSet.CONCUR_UPDATABLE => "CONCUR_UPDATABLE"
        case x => "Invalid ResultSet concurrency constant: " + x
    }

    def resultSetHoldabilityDescs (in : Int) = in match {
        case ResultSet.HOLD_CURSORS_OVER_COMMIT => "HOLD_CURSORS_OVER_COMMIT"
        case ResultSet.CLOSE_CURSORS_AT_COMMIT => "CLOSE_CURSORS_AT_COMMIT"
        case x => "Invalid ResultSet holdability constant: " + x
    }

    def resultSetTypeDescs (in : Int) = in match {
        case ResultSet.TYPE_FORWARD_ONLY => "TYPE_FORWARD_ONLY"
        case ResultSet.TYPE_SCROLL_INSENSITIVE => "TYPE_SCROLL_INSENSITIVE"
        case ResultSet.TYPE_SCROLL_SENSITIVE => "TYPE_SCROLL_SENSITIVE"
        case x => "Invalid ResultSet type constant: " + x
    }
}

}
}
