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

import _root_.java.math.{MathContext,RoundingMode}
import _root_.java.sql.{ResultSet, Types}
import _root_.java.lang.reflect.Method
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.http.{S, SHtml}
import _root_.java.util.Date
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.json._
import _root_.net.liftweb.http.js._
import _root_.scala.xml.NodeSeq

/**
 * <p>
 * A field that maps to a decimal value. Decimal precision and rounding
 * are controlled via the context parameter. The default value is zero.
 * </p>
 *
 * <p><b><i>Note:</i></b><br/>
 * Using MathContext.UNLIMITED, whether explicitly or implicitly, means
 * that no precision or scaling will be used for the SQL field definition; the
 * default scale for DECIMAL is zero per the SQL standard, but the precision
 * for DECIMAL is vendor-specific. For example, PostgreSQL uses maximum precision
 * if it's not specified, but SQL Server uses a default precision of 18.
 * </p>
 *
 * @author Derek Chen-Becker
 *
 * @param fieldOwner The Mapper that owns this field
 * @param context The MathContext that controls precision and rounding
 * @param scale Controls the scale of the underlying BigDecimal
 */
abstract class MappedDecimal[T <: Mapper[T]] (val fieldOwner : T, val context : MathContext, val scale : Int) extends MappedField[BigDecimal,T] {

  /**
   * Constructs a MappedDecimal with the specified initial value and context.
   * The scale is taken from the initial value.
   *
   * @param fieldOwner The Mapper that owns this field
   * @param value The initial value
   * @param context The MathContext that controls precision and rounding
   */
  def this(fieldOwner : T, value : BigDecimal, context : MathContext) = {
    this(fieldOwner, context, value.scale)
    setAll(value)
  }

  /**
   * Constructs a MappedDecimal with the specified initial value. The context
   * is set to MathContext.UNLIMITED (see note above about default precision).
   * The scale is taken from the initial value.
   *
   * @param fieldOwner The Mapper that owns this field
   * @param value The initial value
   */
  def this(fieldOwner : T, value : BigDecimal) = {
    this(fieldOwner, MathContext.UNLIMITED, value.scale)
    setAll(value)
  }

  private val zero = BigDecimal("0")

  def defaultValue = zero.setScale(scale)

  def dbFieldClass = classOf[BigDecimal]

  private var data : BigDecimal = defaultValue
  private var orgData : BigDecimal = defaultValue

  private def st (in : BigDecimal) = {
    data = in
    orgData = in
  }

  protected def i_is_! = data
  protected def i_was_! = orgData

  override def doneWithSave() {
    orgData = data
  }

  override def readPermission_? = true
  override def writePermission_? = true

  protected def i_obscure_!(in : BigDecimal) = defaultValue

  protected def real_i_set_!(value : BigDecimal): BigDecimal = {
    if (value != data) {
      data = value
      dirty_?(true)
    }
    data
  }

  def asJsExp: JsExp = JE.Num(is)
  def asJsonValue: Box[JsonAST.JValue] = Full(JsonAST.JDouble(is.doubleValue))

  def setFromAny (in : Any) : BigDecimal =
    in match {
      // FIXME set for big decimal
      // case JsonAST.JDouble(db) => MappedDecimal.this.setAll(java.math.BigDecimal.valueOf(db))
      // case JsonAST.JInt(bi) => MappedDecimal.this.set(new java.math.BigDecimal(bi.bigInteger))
      case bd : BigDecimal => setAll(bd)
      case n :: _ => setFromString(n.toString)
      case Some(n) => setFromString(n.toString)
      case Full(n) => setFromString(n.toString)
      case None | Empty | Failure(_, _, _) | null => setFromString("0")
      case n => setFromString(n.toString)
    }

  def setFromString (in : String) : BigDecimal = {
    this.setAll(BigDecimal(in))
    data
  }

  /** Set the value along with proper scale, precision, and rounding */
  protected def setAll (in : BigDecimal) = this.set(coerce(in))

  // Set the scale on the given input
  protected def coerce (in : BigDecimal) = new BigDecimal(in.bigDecimal.setScale(scale, context.getRoundingMode))

  def targetSQLType = Types.DECIMAL

  def jdbcFriendly(field : String) = i_is_!.bigDecimal

  def real_convertToJDBCFriendly(value: BigDecimal): Object = value.bigDecimal

  def buildSetBooleanValue(accessor : Method, columnName : String) : (T, Boolean, Boolean) => Unit = null

  def buildSetDateValue(accessor : Method, columnName : String) : (T, Date) => Unit =
    (inst, v) => doField(inst, accessor, {case f: MappedDecimal[T] => f.set(if (v == null) defaultValue else coerce(BigDecimal(v.getTime)))})

  def buildSetStringValue(accessor: Method, columnName: String): (T, String) =>
    Unit = (inst, v) => doField(inst, accessor, {case f: MappedDecimal[T] => f.set(if (v == null) defaultValue else coerce(BigDecimal(v)))})

  def buildSetLongValue(accessor: Method, columnName : String) : (T, Long, Boolean) =>
    Unit = (inst, v, isNull) => doField(inst, accessor, {case f: MappedDecimal[T] => f.set(if (isNull) defaultValue else coerce(BigDecimal(v)))})

  def buildSetActualValue(accessor: Method, data: AnyRef, columnName: String) : (T, AnyRef) =>
    Unit = (inst, v) => doField(inst, accessor, {case f: MappedDecimal[T] => f.set(if (v == null) defaultValue else coerce(BigDecimal(v.toString)))})

  /**
   * Returns the SQL creation string for this field. See the note at the
   * top of the page concerning default precision.
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = {
    val suffix = if (context.getPrecision == 0) {
      ""
    } else {
      "(" + context.getPrecision + "," + scale + ")"
    } 

    colName + " DECIMAL" + suffix + notNullAppender()
  }
}

}
}
