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

import _root_.java.sql.{ResultSet, Types}
import _root_.java.lang.reflect.Method
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.http.{S, SHtml}
import _root_.java.util.Date
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.json._
import _root_.net.liftweb.http.js._
import _root_.scala.xml._

abstract class MappedBoolean[T<:Mapper[T]](val fieldOwner: T) extends MappedField[Boolean, T] {
  private var data : Box[Boolean] = Full(defaultValue)
  private var orgData: Box[Boolean] = Full(defaultValue)

  def defaultValue: Boolean = false

  def dbFieldClass = classOf[Boolean]

  /**
   * Get the JDBC SQL Type for this field
   */
  def targetSQLType = Types.BOOLEAN

  protected def i_is_! = data openOr false
  protected def i_was_! = orgData openOr false
  protected[mapper] def doneWithSave() {orgData = data}

  protected def real_i_set_!(value : Boolean) : Boolean = {
    dirty_?(data.map(_ != value) openOr true)
    data = Full(value)
    value
  }
  override def readPermission_? = true
  override def writePermission_? = true

     def asJsonValue: Box[JsonAST.JValue] = Full(JsonAST.JBool(is))

  def real_convertToJDBCFriendly(value: Boolean): Object = new _root_.java.lang.Integer(if (value) 1 else 0)

  def jdbcFriendly(field : String) = data.map(v => new _root_.java.lang.Integer(if(v) 1 else 0)) openOr null

  def asJsExp: JsExp = if (is) JE.JsTrue else JE.JsFalse

  override def setFromAny(in: Any): Boolean = {
    in match {
      case b: Boolean => this.set(b)
      case JsonAST.JBool(v) => this.set(v)
      case (b: Boolean) :: _ => this.set(b)
      case Some(b: Boolean) => this.set(b)
      case Full(b: Boolean) => this.set(b)
      case Empty | Failure(_, _, _) | None => this.set(false)
      case (s: String) :: _ => this.set(toBoolean(s))
      case s :: _ => this.setFromAny(s)
      case null => this.set(false)
      case s: String => this.set(toBoolean(s))
      case o => this.set(toBoolean(o))
    }
  }

  protected def i_obscure_!(in : Boolean) = false

  def buildSetActualValue(accessor : Method, inst : AnyRef, columnName : String) : (T, AnyRef) => Unit = {
    inst match {
      case null => {(inst : T, v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedBoolean[T]]; tv.data = Full(false)}}
      case _ => {(inst : T, v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedBoolean[T]]; tv.data = Full(toBoolean(v))}}
    }
  }

  private def allSet(in: Box[Boolean]) {
    this.data = in
    this.orgData = in
  }

  def buildSetLongValue(accessor : Method, columnName : String): (T, Long, Boolean) => Unit =
    (inst, v, isNull) => doField(inst, accessor, {case tv: MappedBoolean[T] => tv.allSet(if (isNull) Empty else Full(v != 0L))})

  def buildSetStringValue(accessor : Method, columnName : String): (T, String) => Unit =
    (inst, v) => doField(inst, accessor, {case tv: MappedBoolean[T] => tv.allSet(if (v == null) Empty else Full(toBoolean(v)))})

  def buildSetDateValue(accessor: Method, columnName: String): (T, Date) => Unit =
    (inst, v) => doField(inst, accessor, {case tv: MappedBoolean[T] => tv.allSet(if (v == null) Empty else Full(true))})

  def buildSetBooleanValue(accessor: Method, columnName : String) : (T, Boolean, Boolean) => Unit   =
    (inst, v, isNull) => doField(inst, accessor, {case tv: MappedBoolean[T] => tv.allSet(if (isNull) Empty else Full(v))})


  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.booleanColumnType + notNullAppender()


  /**
   * Create an input field for the item
   */
  override def _toForm: Box[NodeSeq] = Full(SHtml.checkbox(is,this.apply _))
}

}
}
