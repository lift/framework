/*
 * Copyright 2009-2011 WorldWide Conferencing, LLC
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
import xml.{Text, NodeSeq}
import js._
import net.liftweb.json._

abstract class MappedDouble[T<:Mapper[T]](val fieldOwner: T) extends MappedField[Double, T] {
	private var data: Double = defaultValue
	private var orgData: Double = defaultValue

	private def st(in: Double) {
		data = in
		orgData = in
	}

	def defaultValue: Double = 0.0
	def dbFieldClass = classOf[Double]

	protected def i_is_! = data
	protected def i_was_! = orgData

	override def doneWithSave() {
		orgData = data
	}

  import scala.reflect.runtime.universe._
  def manifest: TypeTag[Double] = typeTag[Double]

  /**
   * Get the source field metadata for the field
   * @return the source field metadata for the field
   */
  def sourceInfoMetadata(): SourceFieldMetadata{type ST = Double} =
    SourceFieldMetadataRep(name, manifest, new FieldConverter {
      /**
       * The type of the field
       */
      type T = Double

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
      def asJson(v: T): Box[JValue] = Full(JsonAST.JDouble(v))

      /**
       * If the field can represent a sequence of SourceFields,
       * get that
       * @param v the field value
       * @return the field as a sequence of SourceFields
       */
      def asSeq(v: T): Box[Seq[SourceFieldInfo]] = Empty
    })

	def toDouble(in: Any): Double = {
		in match {
			case null => 0.0
			case i: Int => i
			case n: Long => n
			case n : Number => n.doubleValue
			case (n: Number) :: _ => n.doubleValue
            case Full(n) => toDouble(n) // fixes issue 185
            case x: EmptyBox => 0.0
			case Some(n) => toDouble(n)
			case None => 0.0
			case s: String => s.toDouble
			case x :: xs => toDouble(x)
			case o => toDouble(o.toString)
		}
	}

	override def readPermission_? = true
	override def writePermission_? = true

	protected def i_obscure_!(in : Double) = defaultValue

	protected def real_i_set_!(value : Double): Double = {
		if (value != data) {
			data = value
			dirty_?(true)
		}
		data
	}

	def asJsExp: JsExp = JE.Num(get)

  def asJsonValue: Box[JsonAST.JValue] = Full(JsonAST.JDouble(get))

	override def setFromAny(in: Any): Double = {
		in match {
		  case JsonAST.JDouble(db) => this.set(db)
		  case JsonAST.JInt(bi) => this.set(bi.doubleValue)
			case n: Double => this.set(n)
			case n: Number => this.set(n.doubleValue)
			case (n: Number) :: _ => this.set(n.doubleValue)
			case Some(n: Number) => this.set(n.doubleValue)
			case None => this.set(0.0)
			case (s: String) :: _ => this.set(toDouble(s))
			case null => this.set(0L)
			case s: String => this.set(toDouble(s))
			case o => this.set(toDouble(o))
		}
	}

	def real_convertToJDBCFriendly(value: Double): Object = new java.lang.Double(value)

	/**
	* Get the JDBC SQL Type for this field
	*/
	def targetSQLType = Types.DOUBLE
	def jdbcFriendly(field : String) = new java.lang.Double(i_is_!)
	def buildSetBooleanValue(accessor : Method, columnName : String) : (T, Boolean, Boolean) => Unit = null
	def buildSetDateValue(accessor : Method, columnName : String) : (T, Date) => Unit =
		(inst, v) => doField(inst, accessor, {case f: MappedDouble[T] => f.st(if (v == null) defaultValue else v.getTime)})

	def buildSetStringValue(accessor: Method, columnName: String): (T, String) =>
		Unit = (inst, v) => doField(inst, accessor, {case f: MappedDouble[T] => f.st(toDouble(v))})

	def buildSetLongValue(accessor: Method, columnName : String) : (T, Long, Boolean) =>
		Unit = (inst, v, isNull) => doField(inst, accessor, {case f: MappedDouble[T] => f.st(if (isNull) defaultValue else v)})

	def buildSetActualValue(accessor: Method, data: AnyRef, columnName: String) : (T, AnyRef) =>
		Unit = (inst, v) => doField(inst, accessor, {case f: MappedDouble[T] => f.st(toDouble(v))})

	def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.doubleColumnType + notNullAppender()
}

