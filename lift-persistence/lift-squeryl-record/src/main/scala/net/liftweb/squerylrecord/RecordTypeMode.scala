/*
 * Copyright 2010 WorldWide Conferencing, LLC
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

package net.liftweb {
package squerylrecord {

import _root_.java.util.{Calendar, Date}
import _root_.net.liftweb.record.{MandatoryTypedField, OptionalTypedField, TypedField}
import _root_.org.squeryl.PrimitiveTypeMode
import _root_.org.squeryl.dsl.{BooleanExpression, DateExpression, EnumExpression, NumericalExpression, StringExpression}
import _root_.org.squeryl.dsl.ast.SelectElementReference
import _root_.org.squeryl.internals.FieldReferenceLinker

object RecordTypeMode extends RecordTypeMode

trait RecordTypeMode extends PrimitiveTypeMode {
  implicit def long2ScalarLong(l: MandatoryTypedField[Long]) =
    new SelectElementReference[LongType](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperLongType) with NumericalExpression[Long]

  implicit def int2ScalarInt(i: MandatoryTypedField[Int]) = 
    new SelectElementReference[Int](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperIntType) with NumericalExpression[Int]

  implicit def double2ScalarDouble(d: MandatoryTypedField[Double]) =
    new SelectElementReference[Double](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperDoubleType) with NumericalExpression[Double]

//  implicit def float2ScalarFloat(d: Float) =
//    new SelectElementReference[Float](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperFloatType) with NumericalExpression[Float]

  implicit def string2ScalarString(s: MandatoryTypedField[String]) =
    new SelectElementReference[String](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperStringType) with StringExpression[String]

  implicit def bool2ScalarBoolean(b: MandatoryTypedField[Boolean]) =
    new SelectElementReference[Boolean](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperBooleanType) with BooleanExpression[Boolean]

  implicit def date2ScalarDate(b: MandatoryTypedField[Calendar]) =
    new SelectElementReference[Date](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperDateType) with DateExpression[Date]

  implicit def decimal2ScalarBoolean(i: MandatoryTypedField[BigDecimal]) =
    new SelectElementReference[BigDecimal](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperBigDecimalType) with NumericalExpression[BigDecimal]

  implicit def enum2EnumExpr[EnumType <: Enumeration](l: MandatoryTypedField[EnumType#Value]) = {
    val n = FieldReferenceLinker.takeLastAccessedFieldReference.get
    new SelectElementReference[Enumeration#Value](n)(n.createEnumerationMapper) with EnumExpression[Enumeration#Value]
  }

  implicit def optionString2ScalarString(i: OptionalTypedField[String]) =
    new SelectElementReference[Option[String]](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperStringTypeOption) with StringExpression[Option[String]]

  implicit def optionInt2ScalarInt(i: OptionalTypedField[Int]) =
    new SelectElementReference[Option[Int]](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperIntTypeOption) with NumericalExpression[Option[Int]]

  implicit def optionLong2ScalarLong(i: OptionalTypedField[Long]) =
    new SelectElementReference[Option[Long]](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperLongTypeOption) with NumericalExpression[Option[Long]]

  implicit def optionDouble2ScalarDouble(i: OptionalTypedField[Double]) =
    new SelectElementReference[Option[Double]](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperDoubleTypeOption) with NumericalExpression[Option[Double]]
  
//  implicit def optionFloat2ScalarFloat(i: OptionalTypedField[Float]) =
//    new SelectElementReference[Option[Float]](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperFloatTypeOption) with NumericalExpression[Option[Float]]

  implicit def optionBoolean2ScalarBoolean(i: OptionalTypedField[Boolean]) =
    new SelectElementReference[Option[Boolean]](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperBooleanTypeOption) with BooleanExpression[Option[Boolean]]

  implicit def optionDate2ScalarDate(i: OptionalTypedField[Calendar]) =
    new SelectElementReference[Option[Date]](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperDateTypeOption) with DateExpression[Option[Date]]

  implicit def optionDecimal2ScalarBoolean(i: OptionalTypedField[BigDecimal]) =
    new SelectElementReference[Option[BigDecimal]](FieldReferenceLinker.takeLastAccessedFieldReference.get)(createOutMapperBigDecimalTypeOption) with NumericalExpression[Option[BigDecimal]]

  implicit def optionEnum2ScalaEnum[EnumType <: Enumeration](l: OptionalTypedField[EnumType#Value]) = {  
    val n = FieldReferenceLinker.takeLastAccessedFieldReference.get
    new SelectElementReference[Option[Enumeration#Value]](n)(n.createEnumerationOptionMapper) with EnumExpression[Option[Enumeration#Value]]
  }
}

}
}
