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

import _root_.java.sql.Types
import _root_.java.lang.reflect.Method
import _root_.java.util.Date
import _root_.net.liftweb.util._
import Helpers._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.json._

abstract class MappedBinary[T<:Mapper[T]](val fieldOwner: T) extends MappedField[Array[Byte], T] {
  private val data : FatLazy[Array[Byte]] =  FatLazy(defaultValue)
  private val orgData: FatLazy[Array[Byte]] = FatLazy(defaultValue)

  protected def real_i_set_!(value : Array[Byte]) : Array[Byte] = {
    data() = value
    this.dirty_?( true)
    value
  }

  def dbFieldClass = classOf[Array[Byte]]

  /**
  * Get the JDBC SQL Type for this field
  */
  //  def getTargetSQLType(field : String) = Types.BINARY
  def targetSQLType = Types.BINARY

  def defaultValue: Array[Byte] = null
  override def writePermission_? = true
  override def readPermission_? = true

  protected def i_is_! = data.get

  protected def i_was_! = orgData.get

  protected[mapper] def doneWithSave() {orgData.setFrom(data)}

  protected def i_obscure_!(in : Array[Byte]) : Array[Byte] = {
    new Array[Byte](0)
  }

  override def renderJs_? = false

  def asJsExp: JsExp = throw new NullPointerException("No way")

  def asJsonValue: Box[JsonAST.JValue] = Full(is match {
    case null => JsonAST.JNull
    case value => JsonAST.JString(base64Encode(value))
  })

  override def setFromAny(f: Any): Array[Byte] = f match {
    case null | JsonAST.JNull => this.set(null)
    case JsonAST.JString(base64) => this.set(base64Decode(base64))
    case array: Array[Byte] => this.set(array)
    case s => this.set(s.toString.getBytes("UTF-8"))
  }

  def jdbcFriendly(field : String) : Object = is

  def real_convertToJDBCFriendly(value: Array[Byte]): Object = value

  def buildSetActualValue(accessor: Method, inst: AnyRef, columnName: String): (T, AnyRef) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedBinary[T] =>
    val toSet = v match {
      case null => null
      case ba: Array[Byte] => ba
      case other => other.toString.getBytes("UTF-8")
    }
    f.data() = toSet
    f.orgData() = toSet
  })

  def buildSetLongValue(accessor : Method, columnName : String): (T, Long, Boolean) => Unit = null
  def buildSetStringValue(accessor : Method, columnName : String): (T, String) => Unit  = null
  def buildSetDateValue(accessor : Method, columnName : String): (T, Date) => Unit = null
  def buildSetBooleanValue(accessor : Method, columnName : String): (T, Boolean, Boolean) => Unit = null

  /**
  * Given the driver type, return the string required to create the column in the database
  */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.binaryColumnType + notNullAppender()
}

abstract class MappedText[T<:Mapper[T]](val fieldOwner: T) extends MappedField[String, T] {
  private val data : FatLazy[String] =  FatLazy(defaultValue)
  private val orgData: FatLazy[String] = FatLazy(defaultValue)

  protected def real_i_set_!(value: String): String = {
    data() = value
    this.dirty_?( true)
    value
  }

  def dbFieldClass = classOf[String]

  /**
  * Get the JDBC SQL Type for this field
  */
  //  def getTargetSQLType(field : String) = Types.BINARY
  def targetSQLType = Types.VARCHAR

  def defaultValue: String = null
  override def writePermission_? = true
  override def readPermission_? = true

  protected def i_is_! = data.get

  protected def i_was_! = orgData.get

  protected[mapper] def doneWithSave() {orgData.setFrom(data)}

  def asJsExp: JsExp = JE.Str(is)

  def asJsonValue: Box[JsonAST.JValue] = Full(is match {
    case null => JsonAST.JNull
    case str => JsonAST.JString(str)
  })

  protected def i_obscure_!(in: String): String = ""

    override def setFromAny(in: Any): String = {
    in match {
      case JsonAST.JNull => this.set(null)
      case JsonAST.JString(str) => this.set(str)
      case seq: Seq[_] if !seq.isEmpty => seq.map(setFromAny).apply(0)
      case (s: String) :: _ => this.set(s)
      case s :: _ => this.setFromAny(s)
      case null => this.set(null)
      case s: String => this.set(s)
      case Some(s: String) => this.set(s)
      case Full(s: String) => this.set(s)
      case None | Empty | Failure(_, _, _) => this.set(null)
      case o => this.set(o.toString)
    }
  }

  def jdbcFriendly(field : String): Object = real_convertToJDBCFriendly(data.get)


  def real_convertToJDBCFriendly(value: String): Object = value match {
    case null => null
    case s => s
  }

  def buildSetActualValue(accessor: Method, inst: AnyRef, columnName: String): (T, AnyRef) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedText[T] =>
    val toSet = v match {
      case null => null
      case s: String => s
      case ba: Array[Byte] => new String(ba, "UTF-8")
      case clob: _root_.java.sql.Clob => clob.getSubString(1,clob.length.toInt)
      case other => other.toString
    }
    f.data() = toSet
    f.orgData() = toSet
  })

  def buildSetLongValue(accessor : Method, columnName : String): (T, Long, Boolean) => Unit = null
  def buildSetStringValue(accessor : Method, columnName : String): (T, String) => Unit  = (inst, v) => doField(inst, accessor, {case f: MappedText[T] =>
    val toSet = v match {
      case null => null
      case other => other
    }
    f.data() = toSet
    f.orgData() = toSet
  })
  def buildSetDateValue(accessor : Method, columnName : String): (T, Date) => Unit = null
  def buildSetBooleanValue(accessor : Method, columnName : String): (T, Boolean, Boolean) => Unit = null

  /**
  * Given the driver type, return the string required to create the column in the database
  */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.clobColumnType + notNullAppender()
}

abstract class MappedFakeClob[T<:Mapper[T]](val fieldOwner: T) extends MappedField[String, T] {
  private val data : FatLazy[String] =  FatLazy(defaultValue)
  private val orgData: FatLazy[String] = FatLazy(defaultValue)

  protected def real_i_set_!(value: String): String = {
    data() = value
    this.dirty_?( true)
    value
  }

  def dbFieldClass = classOf[String]

  /**
  * Get the JDBC SQL Type for this field
  */
  //  def getTargetSQLType(field : String) = Types.BINARY
  def targetSQLType = Types.BINARY

  def defaultValue: String = null
  override def writePermission_? = true
  override def readPermission_? = true

  protected def i_is_! = data.get

  protected def i_was_! = orgData.get

  protected[mapper] def doneWithSave() {orgData.setFrom(data)}

  protected def i_obscure_!(in: String): String = ""

  def asJsExp: JsExp = JE.Str(is)

  def asJsonValue: Box[JsonAST.JValue] = Full(is match {
    case null => JsonAST.JNull
    case str => JsonAST.JString(str)
  })


    override def setFromAny(in: Any): String = {
    in match {
      case JsonAST.JNull => this.set(null)
      case JsonAST.JString(str) => this.set(str)
      case seq: Seq[_] if !seq.isEmpty => seq.map(setFromAny).apply(0)
      case (s: String) :: _ => this.set(s)
      case s :: _ => this.setFromAny(s)
      case null => this.set(null)
      case s: String => this.set(s)
      case Some(s: String) => this.set(s)
      case Full(s: String) => this.set(s)
      case None | Empty | Failure(_, _, _) => this.set(null)
      case o => this.set(o.toString)
    }
  }

  def jdbcFriendly(field : String): Object = real_convertToJDBCFriendly(data.get)


  def real_convertToJDBCFriendly(value: String): Object = value match {
    case null => null
    case s => s.getBytes("UTF-8")
  }

  def buildSetActualValue(accessor: Method, inst: AnyRef, columnName: String): (T, AnyRef) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedFakeClob[T] =>
    val toSet = v match {
      case null => null
      case ba: Array[Byte] => new String(ba, "UTF-8")
      case clob: _root_.java.sql.Clob => clob.getSubString(1,clob.length.toInt)
      case other => other.toString
    }
    f.data() = toSet
    f.orgData() = toSet
  })

  def buildSetLongValue(accessor : Method, columnName : String): (T, Long, Boolean) => Unit = null
  def buildSetStringValue(accessor : Method, columnName : String): (T, String) => Unit  = (inst, v) => doField(inst, accessor, {case f: MappedFakeClob[T] =>
    val toSet = v match {
      case null => null
      case other => other
    }
    f.data() = toSet
    f.orgData() = toSet
  })
  def buildSetDateValue(accessor : Method, columnName : String): (T, Date) => Unit = null
  def buildSetBooleanValue(accessor : Method, columnName : String): (T, Boolean, Boolean) => Unit = null

  /**
  * Given the driver type, return the string required to create the column in the database
  */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.binaryColumnType + notNullAppender()
}

}
}
