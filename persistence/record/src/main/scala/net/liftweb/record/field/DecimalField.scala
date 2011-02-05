/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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
package record {
package field {

import _root_.java.math.{BigDecimal => JBigDecimal,MathContext,RoundingMode}
import _root_.scala.xml._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http.{S}
import _root_.net.liftweb.json.JsonAST.JValue
import _root_.net.liftweb.util._
import Helpers._
import S._


trait DecimalTypedField extends NumericTypedField[BigDecimal] {
  protected val scale: Int
  protected val context: MathContext
  private val zero = BigDecimal("0")

  def defaultValue = zero.setScale(scale)

  def setFromAny(in : Any): Box[BigDecimal] = setNumericFromAny(in, n => BigDecimal(n.toString))

  def setFromString (s : String) : Box[BigDecimal] = setBox(tryo(BigDecimal(s)))

  def set_!(in: BigDecimal): BigDecimal = new BigDecimal(in.bigDecimal.setScale(scale, context.getRoundingMode))

  def asJValue = asJString(_.toString)
  def setFromJValue(jvalue: JValue) = setFromJString(jvalue)(s => tryo(BigDecimal(s)))
}


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
class DecimalField[OwnerType <: Record[OwnerType]](rec: OwnerType, val context : MathContext, val scale : Int)
  extends Field[BigDecimal, OwnerType] with MandatoryTypedField[BigDecimal] with DecimalTypedField {

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
    set(value)
  }

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
    set(value)
  }

  def owner = rec
}


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
class OptionalDecimalField[OwnerType <: Record[OwnerType]](rec: OwnerType, val context : MathContext, val scale : Int)
  extends Field[BigDecimal, OwnerType] with OptionalTypedField[BigDecimal] with DecimalTypedField {

  /**
   * Constructs a DecimalField with the specified initial value. The context
   * is set to MathContext.UNLIMITED (see note above about default precision).
   * The scale is taken from the initial value.
   *
   * @param rec The Record that owns this field
   * @param value The initial value
   * @param scale the scale of the decimal field, since there might be no value
   */
  def this(rec : OwnerType, value : Box[BigDecimal], scale : Int) = {
    this(rec, MathContext.UNLIMITED, scale)
    setBox(value)
  }

  /**
   * Constructs a DecimalField with the specified initial value and context.
   * The scale is taken from the initial value.
   *
   * @param rec The Record that owns this field
   * @param value The initial value
   * @param scale the scale of the decimal field, since there might be no value
   * @param context The MathContext that controls precision and rounding
   */
  def this(rec : OwnerType, value : Box[BigDecimal], scale : Int, context : MathContext) = {
    this(rec, context, scale)
    setBox(value)
  }

  def owner = rec
}

}
}
}
