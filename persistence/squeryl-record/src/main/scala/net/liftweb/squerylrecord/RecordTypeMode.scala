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

package net.liftweb
package squerylrecord

import record.{ MandatoryTypedField, OptionalTypedField, TypedField, Record}
import record.field.{EnumNameField, OptionalEnumNameField, EnumField, OptionalEnumField}

import org.squeryl.{ PrimitiveTypeMode, Schema, Query }
import org.squeryl.dsl.{ BooleanExpression, DateExpression, EnumExpression, NumericalExpression, StringExpression, NonNumericalExpression }
import org.squeryl.dsl.ast.{ SelectElementReference, SelectElement, ConstantExpressionNode, RightHandSideOfIn }
import org.squeryl.internals.{ AttributeValidOnNonNumericalColumn, AttributeValidOnNumericalColumn, FieldReferenceLinker, OutMapper }

import java.util.{ Calendar, Date }
import java.sql.Timestamp

/**
 * All methods from this object should be imported when creating queries using the Squeryl DSL with lift records.
 * 
 * It provides implicit conversions for all record field types to the underlying primitive types. Thus, you can use
 * record fields in the Squeryl DSL as if they were primitive types.
 */
object RecordTypeMode extends RecordTypeMode

trait RecordTypeMode extends PrimitiveTypeMode {

  /** Conversion of mandatory Long fields to Squeryl Expressions. */
  implicit def long2ScalarLong(f: MandatoryTypedField[Long]) = convertNumericalMandatory(f, createOutMapperLongType)

  /** Conversion of mandatory Int fields to Squeryl Expressions. */
  implicit def int2ScalarInt(f: MandatoryTypedField[Int]) = convertNumericalMandatory(f, createOutMapperIntType)

  /** Conversion of mandatory Double fields to Squeryl Expressions. */
  implicit def double2ScalarDouble(f: MandatoryTypedField[Double]) = convertNumericalMandatory(f, createOutMapperDoubleType)

  /** Conversion of mandatory BigDecimal fields to Squeryl Expressions. */
  implicit def decimal2ScalarDecimal(f: MandatoryTypedField[BigDecimal]) = convertNumericalMandatory(f, createOutMapperBigDecimalType)

  /** Conversion of optional Int fields to Squeryl Expressions. */
  implicit def optionInt2ScalarInt(f: OptionalTypedField[Int]) = convertNumericalOptional(f, createOutMapperIntTypeOption)
  
  /** Conversion needed for outer joins */
  implicit def optionIntField2OptionInt(f: Option[TypedField[Int]]) = convertNumericalOption(f, createOutMapperIntTypeOption)

  /** Conversion of optional Long fields to Squeryl Expressions. */
  implicit def optionLong2ScalarLong(f: OptionalTypedField[Long]) = convertNumericalOptional(f, createOutMapperLongTypeOption)
  
  /** Conversion needed for outer joins */
  implicit def optionLongField2OptionLong(f: Option[TypedField[Long]]) = convertNumericalOption(f, createOutMapperLongTypeOption)

  /** Conversion of optional Double fields to Squeryl Expressions. */
  implicit def optionDouble2ScalarDouble(f: OptionalTypedField[Double]) = convertNumericalOptional(f, createOutMapperDoubleTypeOption)

  /** Conversion needed for outer joins */
  implicit def optionDoubleField2OptionDouble(f: Option[TypedField[Double]]) = convertNumericalOption(f, createOutMapperDoubleTypeOption)

  /** Conversion of optional BigDecimal fields to Squeryl Expressions. */
  implicit def optionDecimal2ScalarBoolean(f: OptionalTypedField[BigDecimal]) = convertNumericalOptional(f, createOutMapperBigDecimalTypeOption)

  /** Conversion needed for outer joins */
  implicit def optionDecimalField2OptionDecimal(f: Option[TypedField[BigDecimal]]) = convertNumericalOption(f, createOutMapperBigDecimalTypeOption)

  
  /** Conversion of mandatory String fields to Squeryl Expressions. */
  implicit def string2ScalarString(f: MandatoryTypedField[String]) = fieldReference match {
    case Some(e) => new SelectElementReference[String](e)(createOutMapperStringType) with StringExpression[String] with SquerylRecordNonNumericalExpression[String]
    case None => new ConstantExpressionNode[String](f.get)(createOutMapperStringType) with StringExpression[String] with SquerylRecordNonNumericalExpression[String]
  }

  /** Conversion of optional String fields to Squeryl Expressions. */
  implicit def optionString2ScalarString(f: OptionalTypedField[String]) = fieldReference match {
    case Some(e) => new SelectElementReference[Option[String]](e)(createOutMapperStringTypeOption) with StringExpression[Option[String]] with SquerylRecordNonNumericalExpression[Option[String]]
    case None => new ConstantExpressionNode[Option[String]](f.get)(createOutMapperStringTypeOption) with StringExpression[Option[String]] with SquerylRecordNonNumericalExpression[Option[String]]
  }
  
  /** Needed for outer joins */
  implicit def optionStringField2OptionString(f: Option[TypedField[String]]) = fieldReference match {
    case Some(e) => new SelectElementReference[String](e)(createOutMapperStringType) with StringExpression[String] with SquerylRecordNonNumericalExpression[String]
    case None => new ConstantExpressionNode[String](getValueOrNull(f))(createOutMapperStringType) with StringExpression[String] with SquerylRecordNonNumericalExpression[String]
  }

  /** Conversion of mandatory Boolean fields to Squeryl Expressions. */
  implicit def bool2ScalarBoolean(f: MandatoryTypedField[Boolean]) = fieldReference match {
    case Some(e) => new SelectElementReference[Boolean](e)(createOutMapperBooleanType) with BooleanExpression[Boolean] with SquerylRecordNonNumericalExpression[Boolean]
    case None => new ConstantExpressionNode[Boolean](f.get)(createOutMapperBooleanType) with BooleanExpression[Boolean] with SquerylRecordNonNumericalExpression[Boolean]
  }
  
  /** Conversion of optional Boolean fields to Squeryl Expressions. */
  implicit def optionBoolean2ScalarBoolean(f: OptionalTypedField[Boolean]) = fieldReference match {
    case Some(e) => new SelectElementReference[Option[Boolean]](e)(createOutMapperBooleanTypeOption) with BooleanExpression[Option[Boolean]] with SquerylRecordNonNumericalExpression[Option[Boolean]]
    case None => new ConstantExpressionNode[Option[Boolean]](f.get)(createOutMapperBooleanTypeOption) with BooleanExpression[Option[Boolean]] with SquerylRecordNonNumericalExpression[Option[Boolean]]
  }
  
  /** Needed for outer joins. */
  implicit def optionBooleanField2Boolean(f: Option[TypedField[Boolean]]) = fieldReference match {
    case Some(e) => new SelectElementReference[Boolean](e)(createOutMapperBooleanType) with BooleanExpression[Boolean] with SquerylRecordNonNumericalExpression[Boolean]
    case None => new ConstantExpressionNode[Boolean](getValue(f).getOrElse(false))(createOutMapperBooleanType) with BooleanExpression[Boolean] with SquerylRecordNonNumericalExpression[Boolean]
  }
  
  /** Conversion of mandatory Calendar fields to Squeryl Expressions. */
  implicit def date2ScalarDate(f: MandatoryTypedField[Calendar]) = fieldReference match {
    case Some(e) => new SelectElementReference[Timestamp](e)(createOutMapperTimestampType) with DateExpression[Timestamp] with SquerylRecordNonNumericalExpression[Timestamp]
    case None => new ConstantExpressionNode[Timestamp](new Timestamp(f.get.getTimeInMillis))(createOutMapperTimestampType) with DateExpression[Timestamp] with SquerylRecordNonNumericalExpression[Timestamp]
  }

  /** Conversion of optional Calendar fields to Squeryl Expressions. */
  implicit def optionDate2ScalarDate(f: OptionalTypedField[Calendar]) = fieldReference match {
    case Some(e) => new SelectElementReference[Option[Timestamp]](e)(createOutMapperTimestampTypeOption) with DateExpression[Option[Timestamp]] with SquerylRecordNonNumericalExpression[Option[Timestamp]]
    case None => {
      val date = f.get match {
        case Some(calendar) => Some(new Timestamp(calendar.getTimeInMillis))
        case None => None
      }
      new ConstantExpressionNode[Option[Timestamp]](date)(createOutMapperTimestampTypeOption) with DateExpression[Option[Timestamp]] with SquerylRecordNonNumericalExpression[Option[Timestamp]]
    }
  }
  
  /** Needed for inner selects. The cast is possible here because the type is not 
   * used in the in query. Only the AST of the query is needed. */
  //implicit def queryStringField2QueryString[T <: TypedField[String]](q: Query[T]): Query[String] = q.asInstanceOf[Query[String]]
  
  /** Needed for outer joins. */
  implicit def optionDateField2OptionDate(f: Option[TypedField[Calendar]]) = fieldReference match {
    case Some(e) => new SelectElementReference[Timestamp](e)(createOutMapperTimestampType) with DateExpression[Timestamp] with SquerylRecordNonNumericalExpression[Timestamp]
    case None => new ConstantExpressionNode[Timestamp](getValue(f).map(field => new Timestamp(field.getTimeInMillis)).orNull)(createOutMapperTimestampType) with DateExpression[Timestamp] with SquerylRecordNonNumericalExpression[Timestamp]
  }
  
  /** Needed for inner queries on date fields */
  //implicit def dateField2Timestamp(f: MandatoryTypedField[Calendar]) = new java.sql.Timestamp(f.get.getTime.getTime)
  //implicit def optionalDateField2Timestamp(f: OptionalTypedField[Calendar]): Option[java.sql.Timestamp] = f.get.map(d => new java.sql.Timestamp(d.getTime.getTime))
  implicit def calendarFieldQuery2RightHandSideOfIn[F <: TypedField[Calendar]](q: org.squeryl.Query[F]) = new RightHandSideOfIn[Timestamp](q.ast)

  /**
   * Needed for queries on constant calendar values.
   */
  implicit def calendarToTimestampExpression(c: Calendar) = dateToTimestampExpression(c.getTime)

  /**
   * Neeed for queries on constant date values.
   */
  implicit def dateToTimestampExpression(d: java.util.Date) = 
    new ConstantExpressionNode[Timestamp](new java.sql.Timestamp(d.getTime))(createOutMapperTimestampType) with DateExpression[Timestamp] with SquerylRecordNonNumericalExpression[Timestamp]
  
  /** Conversion of mandatory Enum fields to Squeryl Expressions. */
  implicit def enum2EnumExpr[EnumType <: Enumeration](f: MandatoryTypedField[EnumType#Value]) = fieldReference match {
    case Some(e) => new SelectElementReference[Enumeration#Value](e)(e.createEnumerationMapper(f.defaultValue)) with EnumExpression[Enumeration#Value] with SquerylRecordNonNumericalExpression[Enumeration#Value]
    case None => new ConstantExpressionNode[Enumeration#Value](f.get)(outMapperFromEnumValue(f.get)) with EnumExpression[Enumeration#Value] with SquerylRecordNonNumericalExpression[Enumeration#Value]
  }
  
  def reifySingleton[T](m: Manifest[T]) = {
    val cls = m.runtimeClass
    val field = cls.getField("MODULE$")
    field.get(null).asInstanceOf[T]
  }
    
  /** Conversion of optional Enum fields to Squeryl Expressions. */
  implicit def optionEnum2ScalaEnum[EnumType <: Enumeration](f: OptionalTypedField[EnumType#Value])(implicit m: Manifest[EnumType]) = 
    fieldReference match {
    	case Some(e) =>
    	  new SelectElementReference[Option[Enumeration#Value]](e)(e.createEnumerationOptionMapper(Some(reifySingleton(m).values.iterator.next))) with EnumExpression[Option[Enumeration#Value]] with SquerylRecordNonNumericalExpression[Option[Enumeration#Value]]
    	case None => 
    	  new ConstantExpressionNode[Option[Enumeration#Value]](f.get)(outMapperOptionFromOptionEnumValue(f.get).orNull) with EnumExpression[Option[Enumeration#Value]] with SquerylRecordNonNumericalExpression[Option[Enumeration#Value]]
  	}
  
  /** Needed for outer joins. */
  implicit def optionEnumField2OptionEnum[EnumType <: Enumeration](f: Option[TypedField[EnumType#Value]])(implicit m: Manifest[EnumType]) = fieldReference match {
    case Some(e) =>
      new SelectElementReference[Enumeration#Value](e)(e.createEnumerationMapper(reifySingleton(m).values.iterator.next)) with EnumExpression[Enumeration#Value] with SquerylRecordNonNumericalExpression[Enumeration#Value]
    case None => new ConstantExpressionNode[Enumeration#Value](getValue(f).orNull)({
      val enumOption = f flatMap { f1: TypedField[EnumType#Value] => f1.valueBox.toOption } 
      val outMapperOption: Option[OutMapper[Enumeration#Value]] = enumOption map { e: EnumType#Value => outMapperFromEnumValue(e) : OutMapper[Enumeration#Value] /*crashes scala 2.9.1 without explicit type */ } 
      outMapperOption.orNull
    }) with EnumExpression[Enumeration#Value] with SquerylRecordNonNumericalExpression[Enumeration#Value]
  }
  
  implicit def enumFieldQuery2RightHandSideOfIn[EnumType <: Enumeration, T <: Record[T]](q: org.squeryl.Query[EnumNameField[T, EnumType]]) = new RightHandSideOfIn[Enumeration#Value](q.ast)
  
  
  /** Needed for inner queries on certain non-numerical fields: */
  /*implicit def mandatoryTypedField2Value[T](f: MandatoryTypedField[T]): T = f.get
  implicit def optionalTypedField2Value[T](f: OptionalTypedField[T]): Option[T] = f.get*/
  
  implicit def typedFieldQuery2RightHandSideOfIn[T, F <: TypedField[T]](q: org.squeryl.Query[F]) = new RightHandSideOfIn[T](q.ast)


  /**
   * Helper method for converting mandatory numerical fields to Squeryl Expressions.
   */
  private def convertNumericalMandatory[T](f: MandatoryTypedField[T], outMapper: OutMapper[T]) = fieldReference match {
    case Some(e) => new SelectElementReference[T](e)(outMapper) with NumericalExpression[T] with SquerylRecordNumericalExpression[T]
    case None => new ConstantExpressionNode[T](f.get)(outMapper) with NumericalExpression[T] with SquerylRecordNumericalExpression[T]
  }

  /**
   * Helper method for converting optional numerical fields to Squeryl Expressions.
   */
  private def convertNumericalOptional[T](f: OptionalTypedField[T], outMapper: OutMapper[Option[T]]) = fieldReference match {
    case Some(e: SelectElement) => new SelectElementReference[Option[T]](e)(outMapper) with NumericalExpression[Option[T]] with SquerylRecordNumericalExpression[Option[T]]
    case None => new ConstantExpressionNode[Option[T]](f.get)(outMapper) with NumericalExpression[Option[T]] with SquerylRecordNumericalExpression[Option[T]]
  }
  
  private def convertNumericalOption[T](f: Option[TypedField[T]], outMapper: OutMapper[Option[T]]) = fieldReference match {
    case Some(e) => new SelectElementReference[Option[T]](e)(outMapper) with NumericalExpression[Option[T]] with SquerylRecordNumericalExpression[Option[T]]
    case None => new ConstantExpressionNode[Option[T]](getValue(f))(outMapper) with NumericalExpression[Option[T]] with SquerylRecordNumericalExpression[Option[T]]
  }
  
  private def getValue[T](f: Option[TypedField[T]]): Option[T] = f match {
    case Some(field) => field.valueBox
    case None => None
  }
  
  private def getValueOrNull[T <: AnyRef](f: Option[TypedField[T]]): T = f match {
    case Some(field) => field.valueBox.openOr(null.asInstanceOf[T])
    case None => null.asInstanceOf[T]
  }

  /**
   * Returns the field that was last referenced by Squeryl. Can also be None.
   */
  private def fieldReference = FieldReferenceLinker.takeLastAccessedFieldReference

}

/**
 * Record-Specific extensions to numerical Squeryl Expressions.
 */
trait SquerylRecordNumericalExpression[T] { this: NumericalExpression[T] =>

  /**
   * Can be used instead of the often conflicting "is" function.
   */
  def defineAs(columnAttributes: AttributeValidOnNumericalColumn*)(implicit restrictUsageWithinSchema: Schema) = {
    is(columnAttributes: _*)(restrictUsageWithinSchema)
  }
}

/**
 * Record-Specific extensions to non-numerical Squeryl Expressions.
 */
trait SquerylRecordNonNumericalExpression[T] { this: NonNumericalExpression[T] =>

  /**
   * Can be used instead of the often conflicting "is" function.
   */
  def defineAs(columnAttributes: AttributeValidOnNonNumericalColumn*)(implicit restrictUsageWithinSchema: Schema) = {
    is(columnAttributes: _*)(restrictUsageWithinSchema)
  }
}
