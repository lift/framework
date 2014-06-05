/*
 * Copyright 2006-2011 WorldWide Conferencing, LLC
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
package mapper

import java.sql.{ResultSet, Types}
import java.lang.reflect.Method
import net.liftweb.common._
import net.liftweb.util._
import Helpers._
import java.util.Date
import net.liftweb.http._
import reflect.runtime.universe._
import net.liftweb.json._
import net.liftweb.http.jquery.{JqSHtml}
import xml.{Text, NodeSeq}
import js._



/**
 * Warning: Do not use unnamed Enumerations with 2.8.1 as this will cause too many items to be displayed in the dropdown.
 *
 * See https://issues.scala-lang.org/browse/SI-3687 for details
 */ 
abstract class MappedEnum[T<:Mapper[T], ENUM <: Enumeration](val fieldOwner: T, val enum: ENUM)(implicit val manifest: TypeTag[ENUM#Value]) extends MappedField[ENUM#Value, T] {
  private var data: ENUM#Value = defaultValue
  private var orgData: ENUM#Value = defaultValue
  def defaultValue: ENUM#Value = enum.values.iterator.next
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

  /**
   * Get the source field metadata for the field
   * @return the source field metadata for the field
   */
  def sourceInfoMetadata(): SourceFieldMetadata{type ST = ENUM#Value} =
    SourceFieldMetadataRep(name, manifest, new FieldConverter {
      /**
       * The type of the field
       */
      type T = ENUM#Value

      /**
       * Convert the field to a String
       * @param v the field value
       * @return the string representation of the field value
       */
      def asString(v: T): String = v.toString

      /**
       * Convert the field into NodeSeq, if possible
       * @param v the field value
       * @return a NodeSeq if the field can be represented as one
       */
      def asNodeSeq(v: T): Box[NodeSeq] = Full(Text(asString(v)))

      /**
       * Convert the field into a JSON value
       * @param v the field value
       * @return the JSON representation of the field
       */
      def asJson(v: T): Box[JValue] = Full(JsonAST.JInt(v.id))

      /**
       * If the field can represent a sequence of SourceFields,
       * get that
       * @param v the field value
       * @return the field as a sequence of SourceFields
       */
      def asSeq(v: T): Box[Seq[SourceFieldInfo]] = Empty
    })

  protected def real_i_set_!(value: ENUM#Value): ENUM#Value = {
    if (value != data) {
      data = value
      dirty_?(true)

    }
    data
  }
  override def readPermission_? = true
  override def writePermission_? = true

  def real_convertToJDBCFriendly(value: ENUM#Value): Object = new java.lang.Integer(value.id)

  def toInt = get.id
  def fromInt(in: Int): ENUM#Value = enum(in)

  def jdbcFriendly(field: String) = new java.lang.Integer(toInt)
  override def jdbcFriendly = new java.lang.Integer(toInt)

  def asJsExp: JsExp = JE.Num(get.id)

  def asJsonValue: Box[JsonAST.JValue] = Full(JsonAST.JInt(get.id))


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
  def buildDisplayList: List[(Int, String)] = enum.values.toList.map(a => (a.id, a.toString))

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

  def makeKeyJDBCFriendly(in : Int) = new java.lang.Integer(in)

  def convertKey(in : String): Box[Int] = {
    if (in eq null) Empty
    try {
      val what = if (in.startsWith(name + "=")) in.substring((name + "=").length) else in
      Full(Integer.parseInt(what))
    } catch {
      case _: Exception => Empty
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
      case _: Exception => Empty
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

  import scala.reflect.runtime.universe._
  def manifest: TypeTag[Int] = typeTag[Int]

  /**
   * Get the source field metadata for the field
   * @return the source field metadata for the field
   */
  def sourceInfoMetadata(): SourceFieldMetadata{type ST = Int} =
    SourceFieldMetadataRep(name, manifest, new FieldConverter {
      /**
       * The type of the field
       */
      type T = Int

      /**
       * Convert the field to a String
       * @param v the field value
       * @return the string representation of the field value
       */
      def asString(v: T): String = v.toString

      /**
       * Convert the field into NodeSeq, if possible
       * @param v the field value
       * @return a NodeSeq if the field can be represented as one
       */
      def asNodeSeq(v: T): Box[NodeSeq] = Full(Text(asString(v)))

      /**
       * Convert the field into a JSON value
       * @param v the field value
       * @return the JSON representation of the field
       */
      def asJson(v: T): Box[JValue] = Full(JsonAST.JInt(v))

      /**
       * If the field can represent a sequence of SourceFields,
       * get that
       * @param v the field value
       * @return the field as a sequence of SourceFields
       */
      def asSeq(v: T): Box[Seq[SourceFieldInfo]] = Empty
    })

  protected def i_is_! = data
  protected def i_was_! = orgData
  /**
     * Called after the field is saved to the database
     */
  override protected[mapper] def doneWithSave() {
    orgData = data
  }

  def asJsExp: JsExp = JE.Num(get)

  def asJsonValue: Box[JsonAST.JValue] = Full(JsonAST.JInt(get))

  protected def real_i_set_!(value : Int) : Int = {
    if (value != data) {
      data = value
      this.dirty_?( true)
    }
    data
  }
  override def readPermission_? = true
  override def writePermission_? = true

  def +(in: Int): Int = get + in

  def real_convertToJDBCFriendly(value: Int): Object = new java.lang.Integer(value)

  def jdbcFriendly(field : String) = new java.lang.Integer(get)

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

