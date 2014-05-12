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

import common.{ Box, Full }
import record.{ BaseField, MetaRecord, Record, TypedField, OwnedField }
import record.field._
import org.squeryl.internals.{ FieldMetaData, PosoMetaData, FieldMetaDataFactory }
import org.squeryl.annotations.Column
import java.lang.reflect.{ Method, Field }
import java.lang.annotation.Annotation
import java.sql.{ ResultSet, Timestamp }
import java.util.{ Calendar, Date }
import scala.collection.immutable.Map
import net.liftweb.util.Settable
import net.liftweb.record.OptionalTypedField

/** FieldMetaDataFactory that allows Squeryl to use Records as model objects. */
class RecordMetaDataFactory extends FieldMetaDataFactory {
  private val rec: { type R0 <: Record[R0] } = null
  private type Rec = rec.R0

  /** Cache MetaRecords by the model object class (Record class) */
  private var metaRecordsByClass: Map[Class[Rec], MetaRecord[Rec]] = Map.empty


  /** Given a model object class (Record class) and field name, return the BaseField from the meta record */
  private def findMetaField(clasz: Class[Rec], name: String): BaseField = {
    def fieldFrom(mr: MetaRecord[Rec]): BaseField =
      mr.asInstanceOf[Record[Rec]].fieldByName(name) match {
        case Full(f: BaseField) => f
        case Full(_) => org.squeryl.internals.Utils.throwError("field " + name + " in Record metadata for " + clasz + " is not a TypedField")
        case _ => org.squeryl.internals.Utils.throwError("failed to find field " + name + " in Record metadata for " + clasz)
      }

    metaRecordsByClass get clasz match {
      case Some(mr) => fieldFrom(mr)
      case None =>
        try {
          val rec = clasz.newInstance.asInstanceOf[Record[Rec]]
          val mr = rec.meta
          metaRecordsByClass = metaRecordsByClass updated (clasz, mr)
          fieldFrom(mr)
        } catch {
          case ex: Exception => org.squeryl.internals.Utils.throwError("failed to find MetaRecord for " + clasz + " due to exception " + ex.toString)
        }
    }
  }

  /** Build a Squeryl FieldMetaData for a particular field in a Record */
  def build(parentMetaData: PosoMetaData[_], name: String,
    property: (Option[Field], Option[Method], Option[Method], Set[Annotation]),
    sampleInstance4OptionTypeDeduction: AnyRef, isOptimisticCounter: Boolean): FieldMetaData = {
    if (!isRecord(parentMetaData.clasz) || isOptimisticCounter) {
      // Either this is not a Record class, in which case we'll
      //treat it as a normal class in primitive type mode, or the field
      //was mixed in by the Optimisitic trait and is not a Record field.
      return SquerylRecord.posoMetaDataFactory.build(parentMetaData, name, property,
        sampleInstance4OptionTypeDeduction, isOptimisticCounter)
    }

    val metaField = findMetaField(parentMetaData.clasz.asInstanceOf[Class[Rec]], name)

    val (field, getter, setter, annotations) = property

    val colAnnotation = annotations.find(a => a.isInstanceOf[Column]).map(a => a.asInstanceOf[Column])

    val fieldsValueType = metaField match {
      case (f: SquerylRecordField) => f.classOfPersistentField
      case (_: BooleanTypedField) => classOf[Boolean]
      case (_: DateTimeTypedField) => classOf[Timestamp]
      case (_: DoubleTypedField) => classOf[Double]
      case (_: IntTypedField) => classOf[java.lang.Integer]
      case (_: LongTypedField) => classOf[java.lang.Long]
      case (_: DecimalTypedField) => classOf[BigDecimal]
      case (_: TimeZoneTypedField) => classOf[String]
      case (_: StringTypedField) => classOf[String]
      case (_: PasswordTypedField) => classOf[String]
      case (_: BinaryTypedField) => classOf[Array[Byte]]
      case (_: LocaleTypedField) => classOf[String]
      case (_: EnumTypedField[_]) => classOf[Int]
      case (_: EnumNameTypedField[_]) => classOf[String]
      case _ => org.squeryl.internals.Utils.throwError("Unsupported field type. Consider implementing " +
        "SquerylRecordField for defining the persistent class." +
        "Field: " + metaField)
    }

    new FieldMetaData(
      parentMetaData,
      name,
      fieldsValueType, // if isOption, this fieldType is the type param of Option, i.e. the T in Option[T]
      fieldsValueType, //in primitive type mode fieldType == wrappedFieldType, in custom type mode wrappedFieldType is the 'real' type, i.e. the (primitive) type that jdbc understands
      None, //val customTypeFactory: Option[AnyRef=>Product1[Any]],
      metaField.optional_?,
      getter,
      setter,
      field,
      colAnnotation,
      isOptimisticCounter,
      metaField) {

      override def length = {
        import java.math.MathContext
        val fieldLength =
          metaField match {
            case (stringTypedField: StringTypedField) => Some(stringTypedField.maxLength)
            case decimalField: DecimalField[_] => {
              val precision = decimalField.context.getPrecision();
              if (precision != 0)
                Some(precision)
              else
                None
            }
            case decimalField: OptionalDecimalField[_] => {
              val precision = decimalField.context.getPrecision();
              if (precision != 0)
                Some(precision)
              else
                None
            }
            case _ => None
          }
        fieldLength getOrElse super.length
      }

      override def scale = {
        val fieldScale =
          metaField match {
            case decimalField: DecimalField[_] => Some(decimalField.scale)
            case decimalField: OptionalDecimalField[_] => Some(decimalField.scale)
            case _ => None
          }
        fieldScale getOrElse super.scale
      }

      private def fieldFor(o: AnyRef) = getter.get.invoke(o) match {
        case tf: TypedField[_] => tf
        case other => org.squeryl.internals.Utils.throwError("Field's used with Squeryl must inherit from net.liftweb.record.TypedField : " + other )
      }

      /**
       * Sets the value which was retrieved from the DB into the appropriate Record field
       */
      override def set(target: AnyRef, value: AnyRef) = target match {
          case record: Record[_] =>
            record.runSafe {
            	val typedField: TypedField[_] = fieldFor(target)
            	typedField.setFromAny(Box !! value)
            	typedField.resetDirty
            }
          case other =>
            org.squeryl.internals.Utils.throwError("RecordMetaDataFactory can not set fields on non Record objects : " + other)
      }

      override def setFromResultSet(target: AnyRef, rs: ResultSet, index: Int) = set(target, resultSetHandler(rs, index))

      /**
       * Extracts the value from the field referenced by o that will be stored in the DB
       */
      override def get(o: AnyRef) = fieldFor(o) match {
        case enumField: EnumTypedField[_] => enumField.valueBox match {
          case Full(enum: Enumeration#Value) => enum.id: java.lang.Integer
          case _ => null
        }
        case enumNameField: EnumNameTypedField[_] => enumNameField.valueBox match {
          case Full(enum: Enumeration#Value) => enum.toString
          case _ => null
        }
        case other => other.valueBox match {
          case Full(c: Calendar) => new Timestamp(c.getTime.getTime)
          case Full(other: AnyRef) => other
          case _ => null
        }
      }
    }
  }

  /**
   * Checks if the given class is a subclass of Record. A special handling is only
   * needed for such subtypes. For other classes, use the standard squeryl methods.
   */
  private def isRecord(clasz: Class[_]) = {
    classOf[Record[_]].isAssignableFrom(clasz)
  }

  /**
   * For records, the constructor must not be used directly when
   * constructing Objects. Instead, the createRecord method must be called.
   */
  def createPosoFactory(posoMetaData: PosoMetaData[_]): () => AnyRef = {
    if (!isRecord(posoMetaData.clasz)) {
      // No record class - use standard poso meta data factory
      return SquerylRecord.posoMetaDataFactory.createPosoFactory(posoMetaData);
    }

    // Extract the MetaRecord for the companion object. This
    // is done only once for each class.
    val metaRecord = Class.forName(posoMetaData.clasz.getName +
      "$").getField("MODULE$").get(null).asInstanceOf[MetaRecord[_]]

    () => metaRecord.createRecord.asInstanceOf[AnyRef]
  }

  /**
   * There needs to be a special handling for squeryl-record when single fields are selected.
   *
   * The problem was that record fields reference the record itself and thus Squeryl was of the
   * opinion that the whole record should be returned, as well as the selected field.
   * It is described in detail in this bug report:
   * https://www.assembla.com/spaces/liftweb/tickets/876-record-squeryl-selecting-unspecified-columns-in-generated-sql
   *
   * By overriding this function, the reference to the record is excluded from
   * the reference finding algorithm in Squeryl.
   */
  override def hideFromYieldInspection(o: AnyRef, f: Field): Boolean = {
    o.isInstanceOf[OwnedField[_]] && isRecord(f.getType)
  }

}
