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
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._
import _root_.java.util.Date
import _root_.net.liftweb.http._
// import _root_.net.liftweb.widgets.autocomplete._
import _root_.net.liftweb.json._
import _root_.net.liftweb.http.jquery.{JqSHtml}
import _root_.scala.xml.NodeSeq
import js._

abstract class MappedEnum[T<:Mapper[T], ENUM <: Enumeration](val fieldOwner: T, val enum: ENUM) extends MappedField[ENUM#Value, T] {
  private var data: ENUM#Value = defaultValue
  private var orgData: ENUM#Value = defaultValue
  def defaultValue: ENUM#Value = enum.iterator.next
  def dbFieldClass = classOf[ENUM#Value]

  /**
   * Get the JDBC SQL Type for this field
   */
  def targetSQLType = Types.BIGINT

  protected def i_is_! = data
  protected def i_was_! = orgData
  /**
     * Called after the field is saved to the database
     */
  override protected[mapper] def doneWithSave() {
    orgData = data
  }

  protected def real_i_set_!(value: ENUM#Value): ENUM#Value = {
    if (value != data) {
      data = value
      dirty_?(true)
    }
    data
  }
  override def readPermission_? = true
  override def writePermission_? = true

  def real_convertToJDBCFriendly(value: ENUM#Value): Object = new _root_.java.lang.Integer(value.id)

  def toInt = is.id
  def fromInt(in: Int): ENUM#Value = enum(in)

  def jdbcFriendly(field: String) = new _root_.java.lang.Integer(toInt)
  override def jdbcFriendly = new _root_.java.lang.Integer(toInt)

  def asJsExp: JsExp = JE.Num(is.id)

  def asJsonValue: Box[JsonAST.JValue] = Full(JsonAST.JInt(is.id))


  override def setFromAny(in: Any): ENUM#Value = {
    in match {
      case JsonAST.JInt(bi) => this.set(fromInt(bi.intValue))
      case n: Int => this.set(fromInt(n))
      case n: Long => this.set(fromInt(n.toInt))
      case n: Number => this.set(fromInt(n.intValue))
      case (n: Number) :: _ => this.set(fromInt(n.intValue))
      case Some(n: Number) => this.set(fromInt(n.intValue))
      case Full(n: Number) => this.set(fromInt(n.intValue))
      case None | Empty | Failure(_, _, _) => this.set(defaultValue)
      case (s: String) :: _ => this.set(fromInt(Helpers.toInt(s)))
      case vs: ENUM#Value => this.set(vs)
      case null => this.set(defaultValue)
      case s: String => this.set(fromInt(Helpers.toInt(s)))
      case o => this.set(fromInt(Helpers.toInt(o)))
    }
  }

  protected def i_obscure_!(in : ENUM#Value) = defaultValue

  private def st(in: ENUM#Value) {
    data = in
    orgData = in
  }

  def buildSetActualValue(accessor: Method, data: AnyRef, columnName: String) : (T, AnyRef) => Unit =
    (inst, v) => doField(inst, accessor, {case f: MappedEnum[T, ENUM] => f.st(if (v eq null) defaultValue else fromInt(Helpers.toInt(v.toString)))})

  def buildSetLongValue(accessor: Method, columnName: String): (T, Long, Boolean) => Unit =
    (inst, v, isNull) => doField(inst, accessor, {case f: MappedEnum[T, ENUM] => f.st(if (isNull) defaultValue else fromInt(v.toInt))})

  def buildSetStringValue(accessor: Method, columnName: String): (T, String) => Unit =
    (inst, v) => doField(inst, accessor, {case f: MappedEnum[T, ENUM] => f.st(if (v eq null) defaultValue else fromInt(Helpers.toInt(v)))})

  def buildSetDateValue(accessor: Method, columnName: String): (T, Date) => Unit =
    (inst, v) => doField(inst, accessor, {case f: MappedEnum[T, ENUM] => f.st(if (v eq null) defaultValue else fromInt(Helpers.toInt(v)))})

  def buildSetBooleanValue(accessor: Method, columnName: String): (T, Boolean, Boolean) => Unit =
    (inst, v, isNull) => doField(inst, accessor, {case f: MappedEnum[T, ENUM] => f.st(defaultValue)})

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.enumColumnType + notNullAppender()

  /*
  Mapper dependency on Widgets is the wrong order.  There should be a trait in Widgets that's
  mixed into this class that provides autocomplete.  dpp 2009/12/01

  /**
   * Whether or not to use autocomplete in toForm
   */
  def autocomplete_? = false
*/

  /**
    * Build a list for the select.  Return a tuple of (String, String) where the first string
    * is the id.string of the Value and the second string is the Text name of the Value.
    */
  def buildDisplayList: List[(Int, String)] = enum.map(a => (a.id, a.toString)).toList

  /**
   * Create an input field for the item
   */
  override def _toForm: Box[NodeSeq] =
  /*
    if (autocomplete_?)
      Full(AutoComplete.autocompleteObj[Int](buildDisplayList, Full(toInt),
                                      v => this.set(fromInt(v))))
    else
    */
      Full(SHtml.selectObj[Int](buildDisplayList, Full(toInt),
                                v => this.set(fromInt(v))))
}

abstract class MappedIntIndex[T<:Mapper[T]](owner : T) extends MappedInt[T](owner) with IndexedField[Int] {

  override def writePermission_? = false // not writable

  override def dbPrimaryKey_? = true

  override def defaultValue = -1

  def defined_? = i_is_! != defaultValue

  override def dbIndexFieldIndicatesSaved_? = {i_is_! != defaultValue}

  def makeKeyJDBCFriendly(in : Int) = new _root_.java.lang.Integer(in)

  def convertKey(in : String): Box[Int] = {
    if (in eq null) Empty
    try {
      val what = if (in.startsWith(name + "=")) in.substring((name + "=").length) else in
      Full(Integer.parseInt(what))
    } catch {
      case _ => Empty
    }
  }

  override def dbDisplay_? = false

  def convertKey(in : Int): Box[Int] = {
    if (in < 0) Empty
    else Full(in)
  }

  def convertKey(in : Long): Box[Int] = {
    if (in < 0 || in > Integer.MAX_VALUE) Empty
    else Full(in.asInstanceOf[Int])
  }

  def convertKey(in : AnyRef): Box[Int] = {
    if ((in eq null) || (in eq None)) None
    try {
      convertKey(in.toString)
    } catch {
      case _ => Empty
    }
  }

  override def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.integerIndexColumnType + notNullAppender()

}


abstract class MappedInt[T<:Mapper[T]](val fieldOwner: T) extends MappedField[Int, T] {
  private var data: Int = defaultValue
  private var orgData: Int = defaultValue

  def defaultValue = 0
  def dbFieldClass = classOf[Int]

  /**
   * Get the JDBC SQL Type for this field
   */
  def targetSQLType = Types.INTEGER

  protected def i_is_! = data
  protected def i_was_! = orgData
  /**
     * Called after the field is saved to the database
     */
  override protected[mapper] def doneWithSave() {
    orgData = data
  }

  def asJsExp: JsExp = JE.Num(is)

  def asJsonValue: Box[JsonAST.JValue] = Full(JsonAST.JInt(is))

  protected def real_i_set_!(value : Int) : Int = {
    if (value != data) {
      data = value
      this.dirty_?( true)
    }
    data
  }
  override def readPermission_? = true
  override def writePermission_? = true

  def +(in: Int): Int = is + in

  def real_convertToJDBCFriendly(value: Int): Object = new _root_.java.lang.Integer(value)

  def jdbcFriendly(field : String) = new _root_.java.lang.Integer(is)

  override def setFromAny(in: Any): Int = {
    in match {
      case n: Int => this.set(n)
      case JsonAST.JInt(bigint) => this.set(bigint.intValue)
      case n: Number => this.set(n.intValue)
      case (n: Number) :: _ => this.set(n.intValue)
      case Some(n: Number) => this.set(n.intValue)
      case Full(n: Number) => this.set(n.intValue)
      case None | Empty | Failure(_, _, _) => this.set(0)
      case (s: String) :: _ => this.set(toInt(s))
      case null => this.set(0)
      case s: String => this.set(toInt(s))
      case o => this.set(toInt(o))
    }
  }

  protected def i_obscure_!(in : Int) = 0

  private def st(in: Int) {
    data = in
    orgData = in
  }

  def buildSetActualValue(accessor: Method, v: AnyRef, columnName: String): (T, AnyRef) => Unit =
    (inst, v) => doField(inst, accessor, {case f: MappedInt[T] => f.st(toInt(v))})

  def buildSetLongValue(accessor: Method, columnName: String): (T, Long, Boolean) => Unit =
    (inst, v, isNull) => doField(inst, accessor, {case f: MappedInt[T] => f.st(if (isNull) 0 else v.toInt)})

  def buildSetStringValue(accessor: Method, columnName: String): (T, String) => Unit =
    (inst, v) => doField(inst, accessor, {case f: MappedInt[T] => f.st(toInt(v))})

  def buildSetDateValue(accessor: Method, columnName: String): (T, Date) => Unit =
    (inst, v) => doField(inst, accessor, {case f: MappedInt[T] => f.st(toInt(v))})

  def buildSetBooleanValue(accessor: Method, columnName: String): (T, Boolean, Boolean) => Unit =
    (inst, v, isNull) => doField(inst, accessor, {case f: MappedInt[T] => f.st(if (isNull || !v) 0 else 1)})

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.integerColumnType + notNullAppender()
}

}
}
