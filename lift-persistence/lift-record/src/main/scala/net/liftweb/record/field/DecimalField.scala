/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb.record.field

import _root_.java.math.{BigDecimal => JBigDecimal,MathContext,RoundingMode}
import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http.{S}
import Helpers._
import S._

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
 * @param rec The Record that owns this field
 * @param context The MathContext that controls precision and rounding
 * @param scale Controls the scale of the underlying BigDecimal
 */
class DecimalField[OwnerType <: Record[OwnerType]](rec: OwnerType, val context : MathContext, val scale : Int) extends NumericField[BigDecimal, OwnerType] {

  /**
   * Constructs a DecimalField with the specified initial value and context.
   * The scale is taken from the initial value.
   *
   * @param rec The Record that owns this field
   * @param value The initial value
   * @param context The MathContext that controls precision and rounding
   */
  def this(rec : OwnerType, value : BigDecimal, context : MathContext) = {
    this(rec, context, value.scale)
    setFromAny(value)
  }

  /**
   * Constructs a DecimalField with the specified initial value. The context
   * is set to MathContext.UNLIMITED (see note above about default precision).
   * The scale is taken from the initial value.
   *
   * @param rec The Record that owns this field
   * @param value The initial value
   */
  def this(rec : OwnerType, value : BigDecimal) = {
    this(rec, MathContext.UNLIMITED, value.scale)
    setFromAny(value)
  }

  private val zero = BigDecimal("0")

  def defaultValue = zero.setScale(scale)

  def owner = rec

  def setFromAny (in : Any) : Box[BigDecimal] =
    in match {
      case n :: _ => setFromString(n.toString)
      case Some(n) => setFromString(n.toString)
      case Full(n) => setFromString(n.toString)
      case None | Empty | Failure(_, _, _) | null => setFromString("0")
      case n => setFromString(n.toString)
    }

  def setFromString (s : String) : Box[BigDecimal] = {
    try {
      Full(this.setAll(BigDecimal(s)))
    } catch {
      case e: Exception => valueCouldNotBeSet = true; Empty
    }
  }

  /** Set the value along with proper scale, precision, and rounding */
  protected def setAll (in : BigDecimal) = this.set(new BigDecimal(in.bigDecimal.setScale(scale, context.getRoundingMode)))
}

import _root_.java.sql.{ResultSet, Types}
import _root_.net.liftweb.mapper.{DriverType}

/**
 * An Decimal field holding DB related logic
 */
abstract class DBDecimalField[OwnerType <: DBRecord[OwnerType]](rec: OwnerType, context : MathContext, scale : Int) extends DecimalField[OwnerType](rec, context, scale)
  with JDBCFieldFlavor[JBigDecimal]{

  def targetSQLType = Types.DECIMAL

  /**
   * Returns the SQL creation string for this field. See the note for DecimalField
   * concerning default precision.
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = {
    val suffix = if (context.getPrecision == 0) {
      ""
    } else {
      "(" + context.getPrecision + "," + scale + ")"
    }

    colName + " DECIMAL" + suffix
  }

  def jdbcFriendly(field : String) : JBigDecimal = value.bigDecimal

}
