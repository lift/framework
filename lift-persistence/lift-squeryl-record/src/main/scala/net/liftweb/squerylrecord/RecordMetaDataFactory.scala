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

import _root_.java.lang.reflect.{Method, Field}
import _root_.java.lang.annotation.Annotation
import _root_.java.sql.{ResultSet, Timestamp}
import _root_.java.util.{Calendar, Date}
import _root_.net.liftweb.common.{Box, Full}
import _root_.net.liftweb.record.{BaseField, MetaRecord, Record, TypedField}
import _root_.net.liftweb.record.field._
import _root_.org.squeryl.annotations.Column
import _root_.org.squeryl.internals.{FieldMetaData, PosoMetaData, FieldMetaDataFactory}
import _root_.scala.collection.immutable.Map

/** FieldMetaDataFactory that allows Squeryl to use Records as model objects. */
class RecordMetaDataFactory extends FieldMetaDataFactory {
  /** Cache MetaRecords by the model object class (Record class) */
  private var metaRecordsByClass: Map[Class[_], MetaRecord[_]] = Map.empty

  /** Given a model object class (Record class) and field name, return the BaseField from the meta record */
  private def findMetaField(clasz: Class[_], name: String): BaseField = {
    def fieldFrom(mr: MetaRecord[_]): BaseField =
      mr.asInstanceOf[Record[_]].fieldByName(name) match {
        case Full(f: BaseField) => f
        case Full(_) => error("field " + name + " in Record metadata for " + clasz + " is not a TypedField")
        case _ => error("failed to find field " + name + " in Record metadata for " + clasz)
      }

    metaRecordsByClass get clasz match {
      case Some(mr) => fieldFrom(mr)
      case None =>
        try {
          val rec = clasz.newInstance.asInstanceOf[Record[_]]
          val mr = rec.meta
          metaRecordsByClass = metaRecordsByClass updated (clasz, mr)
          fieldFrom(mr)
        } catch {
          case ex => error("failed to find MetaRecord for " + clasz + " due to exception " + ex.toString)
        }
    }
  }

  /** Build a Squeryl FieldMetaData for a particular field in a Record */
  def build(parentMetaData: PosoMetaData[_], name: String,
            property: (Option[Field], Option[Method], Option[Method], Set[Annotation]),
            sampleInstance4OptionTypeDeduction: AnyRef, isOptimisticCounter: Boolean): FieldMetaData = {
    val metaField = findMetaField(parentMetaData.clasz, name)

    val (field, getter, setter, annotations) = property
    val colAnnotation = annotations.find(a => a.isInstanceOf[Column]).map(a => a.asInstanceOf[Column])

    val fieldsValueType = metaField match {
      case (_: BooleanTypedField)  => classOf[Boolean]
      case (_: DateTimeTypedField) => classOf[Timestamp]
      case (_: DoubleTypedField)   => classOf[Double]
      case (_: IntTypedField)      => classOf[Int]
      case (_: LongTypedField)     => classOf[Long]
      case (_: StringTypedField)   => classOf[String]
      case (_: EnumTypedField[_])   => classOf[Enumeration#Value]
      case _ => error("unsupported field type : " + metaField)
    } 

    val overrideColLength = metaField match {
      case (stringTypedField: StringTypedField) => Some(stringTypedField.maxLength)
      case _ => None
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

      override def length = overrideColLength getOrElse super.length

      private def fieldFor(o: AnyRef) = getter.get.invoke(o).asInstanceOf[TypedField[AnyRef]]

      override def setFromResultSet(target: AnyRef, rs: ResultSet, index: Int) =
        fieldFor(target).setFromAny(Box!!resultSetHandler(rs, index))

      override def get(o: AnyRef) = fieldFor(o).valueBox match {
        case Full(c: Calendar) => new Timestamp(c.getTime.getTime)
        case Full(other) => other
        case _ => null
      }
    }
  }
}

}
}
