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

import java.sql.Types
import java.lang.reflect.Method
import net.liftweb.util.Helpers._
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import xml.{Text, NodeSeq}
import net.liftweb.http.{S, SHtml}
import net.liftweb.http.js._
import net.liftweb.json._


abstract class MappedLongIndex[T<:Mapper[T]](theOwner: T) extends MappedLong[T](theOwner) with IndexedField[Long] {

  override def writePermission_? = false // not writable

  override def dbIndexed_? = true

  def defined_? = i_is_! != defaultValue
  override def dbPrimaryKey_? = true

  override def defaultValue = -1L

  override def dbIndexFieldIndicatesSaved_? = {i_is_! != defaultValue}

  def makeKeyJDBCFriendly(in: Long) = new java.lang.Long(in)

  def convertKey(in: String): Box[Long] = {
    if (in eq null) Empty
    else tryo(toLong(if (in.startsWith(name + "=")) in.substring((name + "=").length) else in))
  }

  override def dbDisplay_? = false

  def convertKey(in : Long): Box[Long] = {
    if (in < 0L) Empty
    else Full(in)
  }

  def convertKey(in : Int): Box[Long] = {
    if (in < 0) Empty
    else Full(in)
  }

  def convertKey(in : AnyRef): Box[Long] = {
    if ((in eq null) || (in eq None)) Empty
    else tryo(convertKey(in.toString)).flatMap(s => s)
  }

  override def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.longIndexColumnType  + notNullAppender()

}

import scala.reflect.runtime.universe._

abstract class MappedEnumList[T<:Mapper[T], ENUM <: Enumeration](val fieldOwner: T, val enum: ENUM)(implicit val manifest: TypeTag[Seq[ENUM#Value]]) extends MappedField[Seq[ENUM#Value], T] {
  type MyElem = ENUM#Value
  type MyType = Seq[MyElem]

  private var data: Seq[ENUM#Value] = defaultValue
  private var orgData: Seq[ENUM#Value] = defaultValue

  def defaultValue: Seq[ENUM#Value] = Nil
  def dbFieldClass = classOf[Seq[ENUM#Value]]

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
  def sourceInfoMetadata(): SourceFieldMetadata{type ST = Seq[ENUM#Value]} =
    SourceFieldMetadataRep(name, manifest, new FieldConverter {
      /**
       * The type of the field
       */
      type T = Seq[ENUM#Value]

      /**
       * Convert the field to a String
       * @param v the field value
       * @return the string representation of the field value
       */
      def asString(v: T): String = v.map(_.toString).mkString(", ")

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
      def asJson(v: T): Box[JValue] = Full(JArray(v.toList.map(x => JsonAST.JInt(x.id))))

      /**
       * If the field can represent a sequence of SourceFields,
       * get that
       * @param v the field value
       * @return the field as a sequence of SourceFields
       */
      def asSeq(v: T): Box[Seq[SourceFieldInfo]] = Empty
    })

  protected def real_i_set_!(value: Seq[ENUM#Value]): Seq[ENUM#Value] = {
    if (value != data) {
      data = value
      dirty_?(true)
    }
    data
  }
  override def readPermission_? = true
  override def writePermission_? = true

  def asJsExp: JsExp = JE.JsArray(get.map(v => JE.Num(v.id)) :_*)

  def asJsonValue: Box[JsonAST.JValue] = Full(JsonAST.JInt(toLong))

  def real_convertToJDBCFriendly(value: Seq[ENUM#Value]): Object = new java.lang.Long(Helpers.toLong(value))

  private def rot(in: Int): Long = 1L << in

  private def toLong: Long = get.foldLeft(0L)((a,b) => a + rot(b.id))

  def fromLong(in: Long): Seq[ENUM#Value] =
    enum.values.iterator.toList.filter(v => (in & rot(v.id)) != 0)

  def jdbcFriendly(field: String) = new java.lang.Long(toLong)
  override def jdbcFriendly = new java.lang.Long(toLong)



  override def setFromAny(in: Any): Seq[ENUM#Value] = {
    in match {
      case JsonAST.JInt(bi) => this.set(fromLong(bi.longValue))
      case n: Long => this.set( fromLong(n))
      case n: Number => this.set(fromLong(n.longValue))
      case (n: Number) :: _ => this.set(fromLong(n.longValue))
      case Some(n: Number) => this.set(fromLong(n.longValue))
      case None => this.set(Nil)
      case (s: String) :: _ => this.set(fromLong(Helpers.toLong(s)))
      case vs: List[_] => this.set(vs.asInstanceOf[List[ENUM#Value]])
      case null => this.set(Nil)
      case s: String => this.set(fromLong(Helpers.toLong(s)))
      case o => this.set(fromLong(Helpers.toLong(o)))
    }
  }

  protected def i_obscure_!(in : Seq[ENUM#Value]) = Nil

  private def st(in: Seq[ENUM#Value]) {
    data = in
    orgData = in
  }

  def buildSetActualValue(accessor: Method, data: AnyRef, columnName: String): (T, AnyRef) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedEnumList[T, ENUM] => f.st(if (v eq null) defaultValue else fromLong(Helpers.toLong(v)))})

  def buildSetLongValue(accessor: Method, columnName: String): (T, Long, Boolean) => Unit =
  (inst, v, isNull) => doField(inst, accessor, {case f: MappedEnumList[T, ENUM] => f.st(if (isNull) defaultValue else fromLong(v))})

  def buildSetStringValue(accessor: Method, columnName: String): (T, String) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedEnumList[T, ENUM] => f.st(if (v eq null) defaultValue else fromLong(Helpers.toLong(v)))})

  def buildSetDateValue(accessor: Method, columnName: String): (T, Date) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedEnumList[T, ENUM] => f.st(if (v eq null) defaultValue else fromLong(Helpers.toLong(v)))})

  def buildSetBooleanValue(accessor : Method, columnName : String): (T, Boolean, Boolean) => Unit =
  (inst, v, isNull) => doField(inst, accessor, {case f: MappedEnumList[T, ENUM] => f.st(defaultValue)})

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.enumListColumnType + notNullAppender()

  /**
   * Create an input field for the item
   */
  override def _toForm: Box[NodeSeq] =
  Full(SHtml.checkbox[ENUM#Value](enum.values.iterator.toList, get,this(_)).toForm)
}

/**
 * Mix with MappedLong to give a default time of millis
 */
trait DefaultMillis extends TypedField[Long] {
  override def defaultValue = millis
}


abstract class MappedNullableLong[T<:Mapper[T]](val fieldOwner: T) extends MappedNullableField[Long, T] {
  private var data: Box[Long] = defaultValue
  private var orgData: Box[Long] = defaultValue

  def defaultValue: Box[Long] = Empty
  def dbFieldClass = classOf[Box[Long]]

  /**
   * Get the JDBC SQL Type for this field
   */
  def targetSQLType = Types.BIGINT

  import scala.reflect.runtime.universe._
  def manifest: TypeTag[Box[Long]] = typeTag[Box[Long]]

  /**
   * Get the source field metadata for the field
   * @return the source field metadata for the field
   */
  def sourceInfoMetadata(): SourceFieldMetadata{type ST = Box[Long]} =
    SourceFieldMetadataRep(name, manifest, new FieldConverter {
      /**
       * The type of the field
       */
      type T = Box[Long]

      /**
       * Convert the field to a String
       * @param v the field value
       * @return the string representation of the field value
       */
      def asString(v: T): String = v.map(_.toString) openOr ""

      /**
       * Convert the field into NodeSeq, if possible
       * @param v the field value
       * @return a NodeSeq if the field can be represented as one
       */
      def asNodeSeq(v: T): Box[NodeSeq] = v.map(x => Text(x.toString))

      /**
       * Convert the field into a JSON value
       * @param v the field value
       * @return the JSON representation of the field
       */
      def asJson(v: T): Box[JValue] = v.map(JsonAST.JInt(_))

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

  protected def real_i_set_!(value: Box[Long]): Box[Long] = {
    if (value != data) {
      data = value
      dirty_?(true)
    }
    data
  }

  def asJsExp: JsExp = get.map(v => JE.Num(v)) openOr JE.JsNull

  def asJsonValue: Box[JsonAST.JValue] =
    Full(get.map(v => JsonAST.JInt(v)) openOr JsonAST.JNull)

  override def readPermission_? = true
  override def writePermission_? = true

  def real_convertToJDBCFriendly(value: Box[Long]): Object = value match {
    case Full(value) => new java.lang.Long(value)
    case _ => null
  }

  // def asJsExp = JE.Num(is)

  def jdbcFriendly(field : String) = real_convertToJDBCFriendly(i_is_!)
  override def jdbcFriendly = real_convertToJDBCFriendly(i_is_!)

  override def setFromAny(in: Any): Box[Long] = {
    in match {
      case n: Long => this.set(Full(n))
      case n: Number => this.set(Full(n.longValue))
      case JsonAST.JNothing | JsonAST.JNull => this.set(Empty)
      case JsonAST.JInt(n) => this.set(Full(n.longValue))
      case (n: Number) :: _ => this.set(Full(n.longValue))
      case Some(n: Number) => this.set(Full(n.longValue))
      case Full(n: Number) => this.set(Full(n.longValue))
      case Empty | Failure(_, _, _) => this.set(Empty)
      case None => this.set(Empty)
      case (s: String) :: _ => this.set(Helpers.asLong(s))
      case s :: _ => this.setFromAny(s)
      case null => this.set(Empty)
      case s: String => this.set(Helpers.asLong(s))
      case o => this.set(Helpers.asLong(o))
    }
  }

  protected def i_obscure_!(in: Box[Long]) = defaultValue

  private def st(in: Box[Long]) {
    data = in
    orgData = in
  }

  def buildSetActualValue(accessor: Method, data: AnyRef, columnName: String) : (T, AnyRef) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedNullableLong[T] => f.st(asLong(v))})

  def buildSetLongValue(accessor: Method, columnName : String) : (T, Long, Boolean) => Unit =
  (inst, v, isNull) => doField(inst, accessor, {case f: MappedNullableLong[T] => f.st(if (isNull) Empty else Full(v))})

  def buildSetStringValue(accessor: Method, columnName: String): (T, String) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedNullableLong[T] => f.st(asLong(v))})

  def buildSetDateValue(accessor : Method, columnName : String) : (T, Date) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedNullableLong[T] => f.st(if (v == null) Empty else Full(v.getTime))})

  def buildSetBooleanValue(accessor : Method, columnName : String) : (T, Boolean, Boolean) => Unit = null

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.longColumnType + notNullAppender()
}

abstract class MappedLong[T<:Mapper[T]](val fieldOwner: T) extends MappedField[Long, T] {
  private var data: Long = defaultValue
  private var orgData: Long = defaultValue

  import scala.reflect.runtime.universe._
  def manifest: TypeTag[Long] = typeTag[Long]

  /**
   * Get the source field metadata for the field
   * @return the source field metadata for the field
   */
  def sourceInfoMetadata(): SourceFieldMetadata{type ST = Long} =
    SourceFieldMetadataRep(name, manifest, new FieldConverter {
      /**
       * The type of the field
       */
      type T = Long

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

  def defaultValue: Long = 0L
  def dbFieldClass = classOf[Long]

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

  protected def real_i_set_!(value : Long): Long = {
    if (value != data) {
      data = value
      dirty_?(true)
    }
    data
  }

  def asJsExp: JsExp = JE.Num(get)

  def asJsonValue: Box[JsonAST.JValue] = Full(JsonAST.JInt(get))

  override def readPermission_? = true
  override def writePermission_? = true

  def real_convertToJDBCFriendly(value: Long): Object = new java.lang.Long(value)

  // def asJsExp: JsExp = JE.Num(is)

  def jdbcFriendly(field : String) = new java.lang.Long(i_is_!)
  override def jdbcFriendly = new java.lang.Long(i_is_!)

  override def setFromAny(in: Any): Long = {
    in match {
      case n: Long => this.set(n)
      case JsonAST.JInt(bigint) => this.set(bigint.longValue)
      case n: Number => this.set(n.longValue)
      case (n: Number) :: _ => this.set(n.longValue)
      case Some(n: Number) => this.set(n.longValue)
      case Full(n: Number) => this.set(n.longValue)
      case Empty | Failure(_, _, _) => this.set(0L)
      case None => this.set(0L)
      case (s: String) :: _ => this.set(toLong(s))
      case s :: _ => this.setFromAny(s)
      case null => this.set(0L)
      case s: String => this.set(toLong(s))
      case o => this.set(toLong(o))
    }
  }

  protected def i_obscure_!(in : Long) = defaultValue

  private def st(in: Long) {
    data = in
    orgData = in
  }

  def buildSetActualValue(accessor: Method, data: AnyRef, columnName: String) : (T, AnyRef) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedLong[T] => f.st(toLong(v))})

  def buildSetLongValue(accessor: Method, columnName : String) : (T, Long, Boolean) => Unit =
  (inst, v, isNull) => doField(inst, accessor, {case f: MappedLong[T] => f.st(if (isNull) defaultValue else v)})

  def buildSetStringValue(accessor: Method, columnName: String): (T, String) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedLong[T] => f.st(toLong(v))})

  def buildSetDateValue(accessor : Method, columnName : String) : (T, Date) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedLong[T] => f.st(if (v == null) defaultValue else v.getTime)})

  def buildSetBooleanValue(accessor : Method, columnName : String) : (T, Boolean, Boolean) => Unit = null

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.longColumnType + notNullAppender()
}

