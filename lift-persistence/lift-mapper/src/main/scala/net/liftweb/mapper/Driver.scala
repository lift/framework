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

import _root_.java.sql.{Connection,PreparedStatement,ResultSet,Statement}
import _root_.net.liftweb.common._

/**
 * JDBC Driver Abstraction base class. New driver types should extend this base
 * class. New drivers should "register" in the companion object
 * DriverType.calcDriver method.
 */
abstract class DriverType(val name : String) {
  def binaryColumnType: String
  def clobColumnType: String
  def varcharColumnType(len : Int) : String = "VARCHAR(%d)".format(len)
  def booleanColumnType: String
  def dateTimeColumnType: String
  def dateColumnType: String
  def timeColumnType: String
  def integerColumnType: String
  def integerIndexColumnType: String
  def enumColumnType: String
  def longForeignKeyColumnType: String
  def longIndexColumnType: String
  def enumListColumnType: String
  def longColumnType: String
  def doubleColumnType: String

  /**
   * This specifies that the driver supports FKs in tables. Note that
   * to enable FK generation in Schemifier, you also need to set
   * MapperRules.createForeignKeys_? to true before running it.
   */
  def supportsForeignKeys_? : Boolean = false

  /**
   * This indicates that Schemifier needs to run with a non-transacted
   * connection. Certain databases require that gathering information
   * on tables (which Schemifier uses for updates) run outside of a transaction.
   */
  def schemifierMustAutoCommit_? : Boolean = false

  def createTablePostpend: String = ""

  /**
   * Whether this database supports LIMIT clause in SELECTs.
   */
  def brokenLimit_? : Boolean = false

  /**
   * Whether the primary key has been defined by the index column.
   */
  def pkDefinedByIndexColumn_? : Boolean = false

  /**
   * Maximum value of the LIMIT clause in SELECT.
   */
  def maxSelectLimit : String = _root_.java.lang.Long.MAX_VALUE.toString

  /**
    * Performs an insert and optionally returns the ResultSet of the generated keys that were inserted. If no keys are
    * specified, return the number of rows updated.
    *
    * @param conn A connection that the method can optionally use if it needs to execute ancillary statements
    * @param query The prepared query string to use for the insert
    * @param setter A function that will set the parameters on the prepared statement
    * @param pkName Zero or more generated column names that need to be returned
    */
  def performInsert [T](conn : SuperConnection, query : String, setter : PreparedStatement => Unit, tableName : String, genKeyNames : List[String])(handler : Either[ResultSet,Int] => T) : T =
    genKeyNames match {
      case Nil =>
        DB.prepareStatement(query, conn) {
          stmt =>
            setter(stmt)
            handler(Right(stmt.executeUpdate))
        }
      case pk =>
        performInsertWithGenKeys(conn, query, setter, tableName, pk, handler)
    }

  /*
   * Subclasses should override this method if they don't have proper getGeneratedKey support (JDBC3)
   */
  protected def performInsertWithGenKeys [T](conn : SuperConnection, query : String, setter : PreparedStatement => Unit, tableName : String, genKeyNames : List[String], handler : Either[ResultSet,Int] => T) : T =
    DB.prepareStatement(query, Statement.RETURN_GENERATED_KEYS, conn) {
      stmt =>
        setter(stmt)
        stmt.executeUpdate
        handler(Left(stmt.getGeneratedKeys))
    }

  /**
   * Name of the default db schema. If not set, then the schema is assumed to
   * equal the db user name.
   */
  def defaultSchemaName : Box[String] = Empty

  type TypeMapFunc = PartialFunction[Int,Int]
  /**
   * Allow the driver to do specific remapping of column types for cases
   * where not all types are supported. Classes that want to do custom type
   * mapping for columns should override the customColumnTypeMap method.
   */
  def columnTypeMap : TypeMapFunc =
    customColumnTypeMap orElse {
      case x => x
    }

  /**
   * Allows the Vendor-specific Driver to do custom type mapping for a particular
   * column type.
   */
  protected def customColumnTypeMap : TypeMapFunc = new TypeMapFunc {
    def apply (in : Int) = -1
    def isDefinedAt (in : Int) = false
  }

  /**
   * This method can be overriden by DriverType impls to allow for custom setup
   * of Primary Key Columns (creating sequeneces or special indices, for example).
   * The List of commands will be executed in order.
   */
  def primaryKeySetup(tableName : String, columnName : String) : List[String] = {
      List("ALTER TABLE "+tableName+" ADD CONSTRAINT "+tableName+"_PK PRIMARY KEY("+columnName+")")
  }

  /** This defines the syntax for adding a column in an alter. This is
   *  used because some DBs (Oracle, for one) use slightly different syntax. */
  def alterAddColumn = "ADD COLUMN"
}

object DriverType {
  var calcDriver:  Connection => DriverType = conn => {
    val meta = conn.getMetaData

    (meta.getDatabaseProductName,meta.getDatabaseMajorVersion,meta.getDatabaseMinorVersion) match {
      case (DerbyDriver.name,_,_) => DerbyDriver
      case (MySqlDriver.name,_,_) => MySqlDriver
      case (PostgreSqlDriver.name, major, minor) if ((major == 8 && minor >= 2) || major > 8) => PostgreSqlDriver
      case (PostgreSqlDriver.name, _, _) => PostgreSqlOldDriver
      case (H2Driver.name,_,_) => H2Driver
      case (SqlServerDriver.name,major,_) if major >= 9 => SqlServerDriver
      case (SqlServerDriver.name,_,_) => SqlServerPre2005Driver
      case (SybaseASEDriver.name,_,_) => SybaseASEDriver
      case (OracleDriver.name,_,_) => OracleDriver
      case (MaxDbDriver.name,_,_) => MaxDbDriver
      case x => throw new Exception(
        "Lift mapper does not support JDBC driver %s.\n".format(x) +
        "See http://wiki.liftweb.net/index.php/Category:Database for a list of supported databases.")
    }
  }
}

object DerbyDriver extends DriverType("Apache Derby") {
  def binaryColumnType = "LONG VARCHAR FOR BIT DATA"
  def booleanColumnType = "SMALLINT"
  def clobColumnType = "LONG VARCHAR"
  def dateTimeColumnType = "TIMESTAMP"
  def dateColumnType = "DATE"
  def timeColumnType = "TIME"
  def integerColumnType = "INTEGER"
  def integerIndexColumnType = "INTEGER NOT NULL GENERATED BY DEFAULT AS IDENTITY"
  def enumColumnType = "BIGINT"
  def longForeignKeyColumnType = "BIGINT"
  def longIndexColumnType = "BIGINT NOT NULL GENERATED BY DEFAULT AS IDENTITY"
  def enumListColumnType = "BIGINT"
  def longColumnType = "BIGINT"
  def doubleColumnType = "DOUBLE"

  override def brokenLimit_? : Boolean = true
}

object MySqlDriver extends DriverType("MySQL") {
  def binaryColumnType = "MEDIUMBLOB"
  def clobColumnType = "LONGTEXT"
  def booleanColumnType = "BOOLEAN"
  def dateTimeColumnType = "DATETIME"
  def dateColumnType = "DATE"
  def timeColumnType = "TIME"
  def integerColumnType = "INTEGER"
  def integerIndexColumnType = "INTEGER NOT NULL AUTO_INCREMENT UNIQUE"
  def enumColumnType = "BIGINT"
  def longForeignKeyColumnType = "BIGINT UNSIGNED"
  def longIndexColumnType = "BIGINT UNSIGNED NOT NULL AUTO_INCREMENT UNIQUE KEY"
  def enumListColumnType = "BIGINT"
  def longColumnType = "BIGINT"
  def doubleColumnType = "DOUBLE"

  override def createTablePostpend: String = " ENGINE = InnoDB "
}

object H2Driver extends DriverType("H2") {
  def binaryColumnType = "BINARY"
  def clobColumnType = "LONGVARCHAR"
  def booleanColumnType = "BOOLEAN"
  def dateTimeColumnType = "TIMESTAMP"
  def dateColumnType = "DATE"
  def timeColumnType = "TIME"
  def integerColumnType = "INTEGER"
  def integerIndexColumnType = "INTEGER NOT NULL AUTO_INCREMENT"
  def enumColumnType = "BIGINT"
  def longForeignKeyColumnType = "BIGINT"
  def longIndexColumnType = "BIGINT NOT NULL AUTO_INCREMENT"
  def enumListColumnType = "BIGINT"
  def longColumnType = "BIGINT"
  def doubleColumnType = "DOUBLE"

  /**
   * Whether the primary key has been defined by the index column.
   * H2 creates primary key for a table, when AUTO_INCREMENT type
   * is used. <--- NOT TRUE
   * I went into the H2 console, created a table with auto_increment
   * and was able to insert duplicate ids. Then I created it with
   * AUTO_INCREMENT PRIMARY KEY and it did not allow it.
   */
  override def pkDefinedByIndexColumn_? : Boolean = false  //changed to false by nafg
  override def supportsForeignKeys_? = true
  override def maxSelectLimit = "0";
  override def defaultSchemaName : Box[String] = Full("PUBLIC")
}

/**
 * Provides some base definitions for PostgreSql databases.
 */
abstract class BasePostgreSQLDriver extends DriverType("PostgreSQL") {
  def binaryColumnType = "BYTEA"
  def clobColumnType = "TEXT"
  def booleanColumnType = "BOOLEAN"
  def dateTimeColumnType = "TIMESTAMP"
  def dateColumnType = "DATE"
  def timeColumnType = "TIME"
  def integerColumnType = "INTEGER"
  def integerIndexColumnType = "SERIAL"
  def enumColumnType = "BIGINT"
  def longForeignKeyColumnType = "BIGINT"
  def longIndexColumnType = "BIGSERIAL"
  def enumListColumnType = "BIGINT"
  def longColumnType = "BIGINT"
  def doubleColumnType = "DOUBLE PRECISION"

  override def maxSelectLimit = "ALL"

  /**
   * "$user" schema is searched before "public", but it does not exist by default,
   * so "public" is our default choice.
   */
  override def defaultSchemaName : Box[String] = Full("public")
}

/**
 * PostgreSql driver for versions 8.2 and up. Tested with:
 *
 * <ul>
 *   <li>8.3</li>
 * </ul>
 */
object PostgreSqlDriver extends BasePostgreSQLDriver {
  /* PostgreSQL doesn't support generated keys via the JDBC driver. Instead, we use the RETURNING clause on the insert.
   * From: http://www.postgresql.org/docs/8.2/static/sql-insert.html
   */
  override def performInsertWithGenKeys [T](conn : SuperConnection, query : String, setter : PreparedStatement => Unit, tableName : String, genKeyNames : List[String], handler : Either[ResultSet,Int] => T) : T =
    DB.prepareStatement(query + " RETURNING " + genKeyNames.mkString(","), conn) {
      stmt =>
        setter(stmt)
        handler(Left(stmt.executeQuery))
    }

  override def supportsForeignKeys_? = true
}

/**
 * PostgreSql driver for versions 8.1 and earlier. Tested with
 *
 * <ul>
 *   <li>8.1</li>
 *   <li>8.0</li>
 * </ul>
 *
 * Successfuly use of earlier versions should be reported to liftweb@googlegroups.com.
 */
object PostgreSqlOldDriver extends BasePostgreSQLDriver {
  /* PostgreSQL doesn't support generated keys via the JDBC driver.
   * Instead, we use the lastval() function to get the last inserted
   * key from the DB.
   */
  override def performInsertWithGenKeys [T](conn : SuperConnection, query : String, setter : PreparedStatement => Unit, tableName : String, genKeyNames : List[String], handler : Either[ResultSet,Int] => T) : T = {
      DB.prepareStatement(query, conn) {
        stmt =>
          setter(stmt)
          stmt.executeUpdate
      }
      val pkValueQuery = genKeyNames.map(String.format("currval('%s_%s_seq')", tableName, _)).mkString(", ")
      DB.statement(conn) {
        stmt =>
          handler(Left(stmt.executeQuery("SELECT " + pkValueQuery)))
      }
  }
}


abstract class SqlServerBaseDriver extends DriverType("Microsoft SQL Server") {
  def binaryColumnType = "IMAGE"
  def booleanColumnType = "BIT"
  override def varcharColumnType(len : Int) : String = "NVARCHAR(%d)".format(len)
  def clobColumnType = "NTEXT"
  def dateTimeColumnType = "DATETIME"
  def dateColumnType = "DATE"
  def timeColumnType = "TIME"
  def integerColumnType = "INT"
  def integerIndexColumnType = "INT IDENTITY NOT NULL"
  def enumColumnType = "BIGINT"
  def longForeignKeyColumnType = "BIGINT"
  def longIndexColumnType = "BIGINT IDENTITY NOT NULL"
  def enumListColumnType = "BIGINT"
  def longColumnType = "BIGINT"
  def doubleColumnType = "FLOAT"

  override def supportsForeignKeys_? = true

  override def defaultSchemaName : Box[String] = Full("dbo")

  // Microsoft doesn't use "COLUMN" syntax when adding a column to a table
  override def alterAddColumn = "ADD"

}

/**
 * Microsoft SQL Server driver for versions 2000 and below
 */
object SqlServerPre2005Driver extends SqlServerBaseDriver

object SqlServerDriver extends SqlServerBaseDriver {
  override def binaryColumnType = "VARBINARY(MAX)"
  override def clobColumnType = "NVARCHAR(MAX)"
}

/**
 * Sybase ASE Driver. Tested with ASE version 15, but should
 * work with lower versions as well.
 */
object SybaseASEDriver extends SqlServerBaseDriver {
  override val name = "ASE"
  override def binaryColumnType = "VARBINARY(MAX)"
  override def clobColumnType = "NVARCHAR(MAX)"
  override def brokenLimit_? = true
  override def schemifierMustAutoCommit_? = true
}

/**
 * Driver for Oracle databases. Tested with:
 *
 * <ul>
 *  <li>Oracle XE 10.2.0.1</li>
 *  <li>Oracle Database 11g Enterprise Edition Release 11.2.0.1.0 - 64bit Production</li>
 * </ul>
 *
 * Other working install versions should be reported to liftweb@googlegroups.com.
 */
object OracleDriver extends DriverType("Oracle") {
  def binaryColumnType = "LONG RAW"
  def booleanColumnType = "NUMBER"
  def clobColumnType = "CLOB"
  def dateTimeColumnType = "TIMESTAMP"
  /*
   * It's unclear whether DATE would suffice here. The PL/SQL ref at
   * http://download.oracle.com/docs/cd/B19306_01/java.102/b14355/apxref.htm
   * seems to indicate that DATE and TIMESTAMP can both be used
   * for java.sql.Date and java.sql.Time representations.
   */
  def dateColumnType = "TIMESTAMP"
  def timeColumnType = "TIMESTAMP"
  def integerColumnType = "NUMBER"
  def integerIndexColumnType = "NUMBER NOT NULL"
  def enumColumnType = "NUMBER"
  def longForeignKeyColumnType = "NUMBER"
  def longIndexColumnType = "NUMBER NOT NULL"
  def enumListColumnType = "NUMBER"
  def longColumnType = "NUMBER"
  def doubleColumnType = "NUMBER"

  /**
   * Whether this database supports LIMIT clause in SELECTs.
   */
  override def brokenLimit_? : Boolean = true

  import _root_.java.sql.Types
  override def customColumnTypeMap = {
    case Types.BOOLEAN => Types.INTEGER
  }

  override def primaryKeySetup(tableName : String, columnName : String) : List[String] = {
    /*
     * This trigger and sequence setup is taken from http://www.databaseanswers.org/sql_scripts/ora_sequence.htm
     */
    super.primaryKeySetup(tableName, columnName) :::
    List("CREATE SEQUENCE " + tableName + "_sequence START WITH 1 INCREMENT BY 1",
         "CREATE OR REPLACE TRIGGER " + tableName + "_trigger BEFORE INSERT ON " + tableName + " " +
         "FOR EACH ROW " +
         "WHEN (new." + columnName + " is null) " +
         "BEGIN " +
         "SELECT " + tableName + "_sequence.nextval INTO :new." + columnName + " FROM DUAL; " +
         "END;")
  }

  // Oracle supports returning generated keys only if we specify the names of the column(s) to return.
  override def performInsertWithGenKeys [T](conn : SuperConnection, query : String, setter : PreparedStatement => Unit, tableName : String , genKeyNames : List[String], handler : Either[ResultSet,Int] => T) : T =
    DB.prepareStatement(query, genKeyNames.toArray, conn) {
      stmt =>
        setter(stmt)
        stmt.executeUpdate
        handler(Left(stmt.getGeneratedKeys))
    }

  // Oracle doesn't use "COLUMN" syntax when adding a column to a table
  override def alterAddColumn = "ADD"

  override def supportsForeignKeys_? = true
}

object MaxDbDriver extends DriverType("MaxDB") {
  def binaryColumnType = "BLOB"
  def booleanColumnType = "BOOLEAN"
  def clobColumnType = "CLOB"
  def dateTimeColumnType = "TIMESTAMP"
  def dateColumnType = "DATE"
  def timeColumnType = "TIME"
  def integerColumnType = "INTEGER"
  def integerIndexColumnType = "FIXED(10) DEFAULT SERIAL"
  def enumColumnType = "FIXED(38)"
  def longForeignKeyColumnType = "FIXED(38)"
  def longIndexColumnType = "FIXED(38) DEFAULT SERIAL"
  def enumListColumnType = "FIXED(38)"
  def longColumnType = "FIXED(38)"
  def doubleColumnType = "FLOAT(38)"
}
}
}
