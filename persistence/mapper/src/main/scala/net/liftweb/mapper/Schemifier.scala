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

import _root_.java.sql.{Connection, ResultSet, DatabaseMetaData}
import _root_.scala.collection.mutable.{HashMap, ListBuffer}
import _root_.net.liftweb.common.{Full, Box, Loggable}
import _root_.net.liftweb.util.Helpers
import Helpers._

/**
 * Given a list of MetaMappers, make sure the database has the right schema
 * <ul>
 * <li>Make sure all the tables exists</li>
 * <li>Make sure the columns in the tables are correct</li>
 * <li>Create the indexes</li>
 * <li>Create the foreign keys</li>
 * </ul>
 */
object Schemifier extends Loggable {
  implicit def superToRegConnection(sc: SuperConnection): Connection = sc.connection

  /**
   * Convenience function to be passed to schemify. Will log executed statements at the info level
   * using Schemifier's logger
   * 
   */
  def infoF(msg: => AnyRef) = logger.info(msg)
 
  /**
   * Convenience function to be passed to schemify. Will not log any executed statements 
   */ 
  def neverF(msg: => AnyRef) = {}
  
 
  def schemify(performWrite: Boolean, logFunc: (=> AnyRef) => Unit, stables: BaseMetaMapper*): List[String] = schemify(performWrite, logFunc, DefaultConnectionIdentifier, stables :_*)

  case class Collector(funcs: List[() => Any], cmds: List[String]) {
    def +(other: Collector) = Collector(funcs ::: other.funcs, cmds ::: other.cmds)
  }

  private def using[RetType <: Any, VarType <: ResultSet](f: => VarType)(f2: VarType => RetType): RetType = {
    val theVar = f
    try {
      f2(theVar)
    } finally {
      theVar.close()
    }
  }

  def schemify(performWrite: Boolean, logFunc: (=> AnyRef) => Unit, dbId: ConnectionIdentifier, stables: BaseMetaMapper*): List[String] = {
    val tables = stables.toList
    DB.use(dbId) { con =>
      // Some databases (Sybase) don't like doing transactional DDL, so we disable transactions
      if (con.driverType.schemifierMustAutoCommit_? && !con.connection.getAutoCommit()) {
        con.connection.commit
        con.connection.setAutoCommit(true)
      }

      val connection = con // SuperConnection(con)
      val driver = DriverType.calcDriver(connection)
      val actualTableNames = new HashMap[String, String]
      if (performWrite) tables.foreach(_.beforeSchemifier)
      val toRun =
      tables.foldLeft(Collector(Nil, Nil))((b, t) => b + ensureTable(performWrite, logFunc, t, connection, actualTableNames)) +
      tables.foldLeft(Collector(Nil, Nil))((b, t) => b + ensureColumns(performWrite, logFunc, t, connection, actualTableNames)) +
      tables.foldLeft(Collector(Nil, Nil))((b, t) => b + ensureIndexes(performWrite, logFunc, t, connection, actualTableNames)) +
      tables.foldLeft(Collector(Nil, Nil))((b, t) => b + ensureConstraints(performWrite, logFunc, t, dbId, connection, actualTableNames))

      /*
       val toRun = tables.flatMap(t => ensureTable(performWrite, t, connection, actualTableNames) ) :::
       tables.flatMap{t => ensureColumns(performWrite, t, connection, actualTableNames)} :::
       tables.flatMap{t => ensureIndexes(performWrite, t, connection, actualTableNames)} :::
       tables.flatMap{t => ensureConstraints(performWrite, t, connection, actualTableNames)}
       */

      if (performWrite) {
        tables.foreach(_.afterSchemifier)
        toRun.funcs.foreach(f => f())
      }

      toRun.cmds
    }
  }

  def destroyTables_!!(logFunc: (=> AnyRef) => Unit, stables: BaseMetaMapper*): Unit = destroyTables_!!(DefaultConnectionIdentifier, logFunc, stables :_*)

  def destroyTables_!!(dbId: ConnectionIdentifier, logFunc: (=> AnyRef) => Unit, stables: BaseMetaMapper*): Unit =
  destroyTables_!!(dbId, 0, logFunc,  stables.toList)

  def destroyTables_!!(dbId: ConnectionIdentifier,cnt: Int, logFunc: (=> AnyRef) => Unit, stables: List[BaseMetaMapper]) {
    val th = new HashMap[String, String]()
    (DB.use(dbId) {
        conn =>
        val sConn = conn // SuperConnection(conn)
        val tables = stables.toList.filter(t => hasTable_?(t, sConn, th))

        tables.foreach{
          table =>
          try {
            val ct = "DROP TABLE "+table._dbTableNameLC
            val st = conn.createStatement
            st.execute(ct)
            logFunc(ct)
            st.close
          } catch {
            case e: Exception => // dispose... probably just an SQL Exception
          }
        }

        tables
      }) match {
      case t if t.length > 0 && cnt < 1000 => destroyTables_!!(dbId, cnt + 1, logFunc, t)
      case _ =>
    }
  }

  /**
   * Retrieves schema name where the unqualified db objects are searched.
   */
  def getDefaultSchemaName(connection: SuperConnection): String =
  (connection.schemaName or connection.driverType.defaultSchemaName or DB.globalDefaultSchemaName).openOr(connection.getMetaData.getUserName)


  private def hasTable_? (table: BaseMetaMapper, connection: SuperConnection, actualTableNames: HashMap[String, String]): Boolean = {
    val md = connection.getMetaData
    using(md.getTables(null, getDefaultSchemaName(connection), null, null)){ rs =>
      def hasTable(rs: ResultSet): Boolean =
      if (!rs.next) false
      else rs.getString(3) match {
        case s if s.toLowerCase == table._dbTableNameLC.toLowerCase => actualTableNames(table._dbTableNameLC) = s; true
        case _ => hasTable(rs)
      }

      hasTable(rs)
    }
  }


  /**
   * Creates an SQL command and optionally executes it.
   *
   * @param performWrite Whether the SQL command should be executed.
   * @param logFunc Logger.
   * @param connection Database connection.
   * @param makeSql Factory for SQL command.
   *
   * @returns SQL command.
   */
  private def maybeWrite(performWrite: Boolean, logFunc: (=> AnyRef) => Unit, connection: SuperConnection) (makeSql: () => String) : String ={
    val ct = makeSql()
    if (performWrite) {
      logFunc(ct)
      val st = connection.createStatement
      st.execute(ct)
      st.close
    }
    ct
  }

  private def ensureTable(performWrite: Boolean, logFunc: (=> AnyRef) => Unit, table: BaseMetaMapper, connection: SuperConnection, actualTableNames: HashMap[String, String]): Collector = {
    val hasTable = hasTable_?(table, connection, actualTableNames)
    val cmds = new ListBuffer[String]()
    if (!hasTable) {
      cmds += maybeWrite(performWrite, logFunc, connection) {
        () => "CREATE TABLE "+table._dbTableNameLC+" ("+createColumns(table, connection).mkString(" , ")+") "+connection.createTablePostpend
      }
      if (!connection.driverType.pkDefinedByIndexColumn_?) {
        // Add primary key only when it has not been created by the index field itself.
        table.mappedFields.filter{f => f.dbPrimaryKey_?}.foreach {
          pkField =>
            connection.driverType.primaryKeySetup(table._dbTableNameLC, pkField._dbColumnNameLC) foreach { command =>
              cmds += maybeWrite(performWrite, logFunc, connection) {
                () => command
              }
            }
        }
      }
      hasTable_?(table, connection, actualTableNames)
      Collector(table.dbAddTable.toList, cmds.toList)
    } else Collector(Nil, cmds.toList)
  }

  private def createColumns(table: BaseMetaMapper, connection: SuperConnection): Seq[String] = {
    table.mappedFields.flatMap(_.fieldCreatorString(connection.driverType))
  }

  private def ensureColumns(performWrite: Boolean, logFunc: (=> AnyRef) => Unit, table: BaseMetaMapper, connection: SuperConnection, actualTableNames: HashMap[String, String]): Collector = {
    val cmds = new ListBuffer[String]()
    val rc = table.mappedFields.toList.flatMap {
      field =>
      var hasColumn = 0
      var cols: List[String] = Nil
      val totalColCnt = field.dbColumnCount
      val md = connection.getMetaData

      using(md.getColumns(null, getDefaultSchemaName(connection), actualTableNames(table._dbTableNameLC), null))(rs =>
        while (hasColumn < totalColCnt && rs.next) {
          val tableName = rs.getString(3).toLowerCase
          val columnName = rs.getString(4).toLowerCase

          if (tableName == table._dbTableNameLC.toLowerCase && field.dbColumnNames(field.name).map(_.toLowerCase).contains(columnName)) {
            cols = columnName :: cols
            hasColumn = hasColumn + 1
          }
        })

      // FIXME deal with column types
      (field.dbColumnNames(field.name).filter(f => !cols.map(_.toLowerCase).contains(f.toLowerCase))).foreach {
        colName =>
        cmds += maybeWrite(performWrite, logFunc, connection) {
          () => "ALTER TABLE "+table._dbTableNameLC+" "+connection.driverType.alterAddColumn+" "+field.fieldCreatorString(connection.driverType, colName)
        }
        if ((!connection.driverType.pkDefinedByIndexColumn_?) && field.dbPrimaryKey_?) {
          // Add primary key only when it has not been created by the index field itself.
          cmds += maybeWrite(performWrite, logFunc, connection) {
            () => "ALTER TABLE "+table._dbTableNameLC+" ADD CONSTRAINT "+table._dbTableNameLC+"_PK PRIMARY KEY("+field._dbColumnNameLC+")"
          }
        }
      }

      field.dbAddedColumn.toList

    }

    Collector(rc, cmds.toList)

  }

  private def ensureIndexes(performWrite: Boolean, logFunc: (=> AnyRef) => Unit, table: BaseMetaMapper, connection: SuperConnection, actualTableNames: HashMap[String, String]): Collector = {
    val cmds = new ListBuffer[String]()
    // val byColumn = new HashMap[String, List[(String, String, Int)]]()
    val byName = new HashMap[String, List[String]]()

    val md = connection.getMetaData
    val q = using(md.getIndexInfo(null, getDefaultSchemaName(connection), actualTableNames(table._dbTableNameLC), false, false)) {rs =>
      def quad(rs: ResultSet): List[(String, String, Int)] = {
        if (!rs.next) Nil else {
          if (rs.getString(3).equalsIgnoreCase(table._dbTableNameLC)) {
            // Skip index statistics
            if (rs.getShort(7) != DatabaseMetaData.tableIndexStatistic) {
              (rs.getString(6).toLowerCase, rs.getString(9).toLowerCase, rs.getInt(8)) :: quad(rs)
            }
            else quad(rs)
          }
          else Nil
        }
      }
      quad(rs)
    }
    // val q = quad(rs)
    // q.foreach{case (name, col, pos) => byColumn.get(col) match {case Some(li) => byColumn(col) = (name, col, pos) :: li case _ => byColumn(col) = List((name, col, pos))}}
    q.foreach{case (name, col, pos) => byName.get(name) match {case Some(li) => byName(name) = col :: li case _ => byName(name) = List(col)}}
    val indexedFields: List[List[String]] = byName.map{case (name, value) => value.sort(_ < _)}.toList
    //rs.close

    val single = table.mappedFields.filter{f => f.dbIndexed_?}.toList.flatMap {
      field =>
      if (!indexedFields.contains(List(field._dbColumnNameLC.toLowerCase))) {
        cmds += maybeWrite(performWrite, logFunc, connection) {
          () => "CREATE INDEX "+(table._dbTableNameLC+"_"+field._dbColumnNameLC)+" ON "+table._dbTableNameLC+" ( "+field._dbColumnNameLC+" )"
        }
        field.dbAddedIndex.toList
      } else Nil
    }

    table.dbIndexes.foreach {
      index =>

      val columns = index.columns.toList

      val standardCreationStatement = (table._dbTableNameLC+"_"+columns.map(_.field._dbColumnNameLC).mkString("_"))+" ON "+table._dbTableNameLC+" ( "+columns.map(_.indexDesc).comma+" )"

      val createStatement = index match {
        case i: net.liftweb.mapper.Index[_] => "CREATE INDEX " + standardCreationStatement
        case i: UniqueIndex[_] => "CREATE UNIQUE INDEX " + standardCreationStatement
        case GenericIndex(createFunc, _, _) => createFunc(table._dbTableNameLC, columns.map(_.field._dbColumnNameLC))
        case _ => logger.error("Invalid index: " + index); ""
      }

      val fn = columns.map(_.field._dbColumnNameLC.toLowerCase).sort(_ < _)
      if (!indexedFields.contains(fn)) {
        cmds += maybeWrite(performWrite, logFunc, connection) {
          () => createStatement
        }
      }
    }

    Collector(single, cmds.toList)
  }

  private def ensureConstraints(performWrite: Boolean, logFunc: (=> AnyRef) => Unit, table: BaseMetaMapper, dbId: ConnectionIdentifier, connection: SuperConnection, actualTableNames: HashMap[String, String]): Collector = {
    val cmds = new ListBuffer[String]()
    val ret = if (connection.supportsForeignKeys_? && MapperRules.createForeignKeys_?(dbId)) {
      table.mappedFields.flatMap{f => f match {case f: BaseMappedField with BaseForeignKey => List(f); case _ => Nil}}.toList.flatMap {
        field =>

        val other = field.dbKeyToTable
        val otherTable = actualTableNames(other._dbTableNameLC)
        val myTable = actualTableNames(table._dbTableNameLC)

        val md = connection.getMetaData
        // val rs = md.getCrossReference(null, null,otherTable , null, null, myTable)
        var foundIt = false
        using(md.getImportedKeys(null, getDefaultSchemaName(connection), myTable))(rs =>
          //val rs = md.getCrossReference(null, null,myTable , null, null, otherTable)
          while (!foundIt && rs.next) {
            val pkName = rs.getString(4)
            val fkName = rs.getString(8)
            foundIt = (field._dbColumnNameLC.toLowerCase == fkName.toLowerCase && field.dbKeyToColumn._dbColumnNameLC.toLowerCase == pkName.toLowerCase)
          })

        if (!foundIt) {
          cmds += maybeWrite(performWrite, logFunc, connection) {
            () => "ALTER TABLE "+table._dbTableNameLC+" ADD FOREIGN KEY ( "+field._dbColumnNameLC+" ) REFERENCES "+other._dbTableNameLC+" ( "+field.dbKeyToColumn._dbColumnNameLC+" ) "
          }
          field.dbAddedForeignKey.toList
        } else Nil
      }
    } else Nil

    Collector(ret, cmds.toList)
  }
}

}
}
