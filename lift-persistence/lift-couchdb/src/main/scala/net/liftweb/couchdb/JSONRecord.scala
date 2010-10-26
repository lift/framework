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
package couchdb {

import _root_.java.math.MathContext
import _root_.java.util.Calendar
import _root_.scala.collection.immutable.TreeSet
import _root_.scala.reflect.Manifest
import _root_.scala.xml.NodeSeq
import _root_.net.liftweb.common.{Box, Empty, Failure, Full}
import Box.{box2Iterable, option2Box}
import _root_.net.liftweb.http.js.{JsExp, JsObj}
import _root_.net.liftweb.json.JsonParser
import _root_.net.liftweb.json.JsonAST.{JArray, JBool, JInt, JDouble, JField, JNothing, JNull, JObject, JString, JValue}
import _root_.net.liftweb.record.{Field, MandatoryTypedField, MetaRecord, Record}
import _root_.net.liftweb.record.FieldHelpers.expectedA
import _root_.net.liftweb.record.field._
import _root_.net.liftweb.util.ThreadGlobal
import _root_.net.liftweb.util.BasicTypesHelpers.{toBoolean, toInt}
import _root_.net.liftweb.util.ControlHelpers.tryo
import _root_.net.liftweb.util.Helpers.{base64Decode, base64Encode}
import _root_.net.liftweb.util.TimeHelpers.{boxParseInternetDate, toInternetDate}

private[couchdb] object JSONRecordHelpers {

  /* For the moment, I couldn't find any other way to bridge JValue and JsExp, so I wrote something simple here */
  implicit def jvalueToJsExp(jvalue: JValue): JsExp = {
    import _root_.net.liftweb.http.js.JE.{JsArray, JsFalse, JsNull, JsObj, JsTrue, Num, Str}
    jvalue match {
      case JArray(vs)  => JsArray(vs.map(jvalueToJsExp): _*)
      case JBool(b)    => if (b) JsTrue else JsFalse
      case JDouble(d)  => Num(d)
      case JField(n,v) => error("no parallel")
      case JInt(i)     => Num(i)
      case JNothing    => error("cannot convert JNothing")
      case JNull       => JsNull
      case JObject(fs) => JsObj(fs.map(f => (f.name, jvalueToJsExp(f.value))): _*)
      case JString(s)  => Str(s)
    }
  }

  /** Remove duplicate fields, preferring the first field seen with a given name */
  def dedupe(fields: List[JField]): List[JField] = {
    var seen = TreeSet.empty[String]
    fields.filter {
      case JField(name, _) if seen contains name => false
      case JField(name, _) => { seen = seen + name; true }
    }
  }
}

import JSONRecordHelpers._

/** Specialized Record that can be encoded and decoded from JSON */
trait JSONRecord[MyType <: JSONRecord[MyType]] extends Record[MyType] {
  self: MyType =>
  
  private var _additionalJFields: List[JField] = Nil

  /** Refines meta to require a JSONMetaRecord */
  def meta: JSONMetaRecord[MyType]

  /** Extra fields to add to the encoded object, such as type. Default is none (Nil) */
  def fixedAdditionalJFields: List[JField] = Nil

  /**
   * Additional fields that are not represented by Record fields, nor are fixed additional fields.
   * Default implementation is for preserving unknown fields across read/write
   */
  def additionalJFields: List[JField] = _additionalJFields

  /**
   * Handle any additional fields that are not represented by Record fields when decoding from a JObject.
   * Default implementation preserves the fields intact and returns them via additionalJFields
   */
  def additionalJFields_= (fields: List[JField]): Unit = _additionalJFields = fields

  /** Encode this record instance as a JObject */
  def asJValue: JObject = meta.asJValue(this)

  /** Set the fields of this record from the given JValue */
  def setFieldsFromJValue(jvalue: JValue): Box[Unit] = meta.setFieldsFromJValue(this, jvalue)
}

object JSONMetaRecord {
  /** Local override to record parsing that can cause extra fields to be ignored, even if they otherwise would cause a failure */
  object overrideIgnoreExtraJSONFields extends ThreadGlobal[Boolean]

  /** Local override to record parsing that can cause missing fields to be ignored, even if they otherwise would cause a failure */
  object overrideNeedAllJSONFields extends ThreadGlobal[Boolean]
}

/** Specialized MetaRecord that deals with JSONRecords */
trait JSONMetaRecord[BaseRecord <: JSONRecord[BaseRecord]] extends MetaRecord[BaseRecord] {
  self: BaseRecord =>

  /**
   * Return the name of the field in the encoded JSON object. If the field implements JSONField and has overridden jsonName then
   * that will be used, otherwise the record field name
   */
  def jsonName(field: Field[_, BaseRecord]): String = field match {
    case (jsonField: JSONField) => jsonField.jsonName openOr field.name
    case _                      =>                           field.name
  }

  /** Whether or not extra fields in a JObject to decode is an error (false) or not (true). The default is true */
  def ignoreExtraJSONFields: Boolean = true

  /** Whether or not missing fields in a JObject to decode is an error (false) or not (true). The default is true */
  def needAllJSONFields: Boolean = true

  override def asJSON(inst: BaseRecord): JsObj = jvalueToJsExp(asJValue).asInstanceOf[JsObj]

  override def setFieldsFromJSON(inst: BaseRecord, json: String): Box[Unit] =
    setFieldsFromJValue(inst, JsonParser.parse(json))

  /** Encode a record instance into a JValue */
  def asJValue(rec: BaseRecord): JObject = {
    val recordJFields = fields(rec).map(f => JField(jsonName(f), f.asJValue))
    JObject(dedupe(recordJFields ++ rec.fixedAdditionalJFields ++ rec.additionalJFields).sort(_.name < _.name))
  }

  /** Create a record by decoding a JValue which must be a JObject */
  def fromJValue(jvalue: JValue): Box[BaseRecord] = {
    val inst = createRecord
    setFieldsFromJValue(inst, jvalue) map (_ => inst)
  }

  /** Attempt to decode a JValue, which must be a JObject, into a record instance */
  def setFieldsFromJValue(rec: BaseRecord, jvalue: JValue): Box[Unit] = {
    def fromJFields(jfields: List[JField]): Box[Unit] = {
      import JSONMetaRecord._

      val flds = fields(rec)
      lazy val recordFieldNames = TreeSet(flds.map(jsonName): _*)
      lazy val jsonFieldNames = TreeSet(jfields.map(_.name): _*)
      lazy val optionalFieldNames = TreeSet(flds.filter(_.optional_?).map(jsonName): _*)
      lazy val recordFieldsNotInJson = recordFieldNames -- jsonFieldNames -- optionalFieldNames
      lazy val jsonFieldsNotInRecord = jsonFieldNames -- recordFieldNames
      
      // If this record type has been configured to be stricter about fields, check those first
      if ((overrideNeedAllJSONFields.box openOr needAllJSONFields) && !recordFieldsNotInJson.isEmpty) {
        Failure("The " + recordFieldsNotInJson.mkString(", ") + " field(s) were not found, but are required.")
      } else if (!(overrideIgnoreExtraJSONFields.box openOr ignoreExtraJSONFields) && !jsonFieldsNotInRecord.isEmpty) {
        Failure("Field(s) " + jsonFieldsNotInRecord.mkString(", ") + " are not recognized.")
      } else {
        for {
          jfield <- jfields
          field  <- flds if jsonName(field) == jfield.name
        } field.setFromJValue(jfield.value)

        rec.additionalJFields = jsonFieldsNotInRecord.toList map {
          name => jfields.find(_.name == name).get
        }

        Full(())
      }
    }

    jvalue match {
      case JObject(jfields) => fromJFields(jfields)
      case other => expectedA("JObject", other)
    }      
  }
}
 
/** Trait for fields with JSON-specific behavior */
trait JSONField {
  /** Return Full(name) to use that name in the encoded JSON object, or Empty to use the same name as in Scala. Defaults to Empty */
  def jsonName: Box[String] = Empty
}


/* ****************************************************************************************************************************************** */


/** Field that contains an entire record represented as an inline object value in the final JSON */
class JSONSubRecordField[OwnerType <: JSONRecord[OwnerType], SubRecordType <: JSONRecord[SubRecordType]]
                        (rec: OwnerType, valueMeta: JSONMetaRecord[SubRecordType])(implicit subRecordType: Manifest[SubRecordType])
  extends Field[SubRecordType, OwnerType] with MandatoryTypedField[SubRecordType]
{
  def this(rec: OwnerType, value: SubRecordType)(implicit subRecordType: Manifest[SubRecordType]) = {
      this(rec, value.meta)
      set(value)
  }

  def this(rec: OwnerType, valueMeta: JSONMetaRecord[SubRecordType], value: Box[SubRecordType])
          (implicit subRecordType: Manifest[SubRecordType]) = {
      this(rec, valueMeta)
      setBox(value)
  }

  def owner = rec
  def asJs = asJValue
  def toForm: Box[NodeSeq] = Empty // FIXME
  def defaultValue = valueMeta.createRecord

  def setFromString(s: String): Box[SubRecordType] = valueMeta.fromJSON(s)

  def setFromAny(in: Any): Box[SubRecordType] = genericSetFromAny(in)

  def asJValue: JValue = valueBox.map(_.asJValue) openOr (JNothing: JValue)
  def setFromJValue(jvalue: JValue): Box[SubRecordType] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case _                            => setBox(valueMeta.fromJValue(jvalue))
  }
}

/** Field that contains an array of some basic JSON type */
class JSONBasicArrayField[OwnerType <: JSONRecord[OwnerType], ElemType <: JValue](rec: OwnerType)(implicit elemType: Manifest[ElemType])
  extends Field[List[ElemType], OwnerType] with MandatoryTypedField[List[ElemType]]
{
  def this(rec: OwnerType, value: List[ElemType])(implicit elemType: Manifest[ElemType]) = { this(rec); set(value) }
  def this(rec: OwnerType, value: Box[List[ElemType]])(implicit elemType: Manifest[ElemType]) = { this(rec); setBox(value) }

  def owner = rec
  def asJs = asJValue
  def toForm: Box[NodeSeq] = Empty // FIXME
  def defaultValue = Nil

  def setFromString(s: String): Box[List[ElemType]] =
    setBox(tryo(JsonParser.parse(s)) flatMap {
      case JArray(values) => checkValueTypes(values)
      case other          => expectedA("JSON string with an array of " + elemType, other)
    })

  def setFromAny(in: Any): Box[List[ElemType]] = genericSetFromAny(in)

  def checkValueTypes(in: List[JValue]): Box[List[ElemType]] =
    in.find(!_.isInstanceOf[ElemType]) match {
      case Some(erroneousValue) if erroneousValue != null =>
          Failure("Value in input array is a " + value.getClass.getName + ", should be a " + elemType.toString)

      case Some(erroneousValue) => Failure("Value in input array is null, should be a " + elemType.toString)
      case None                 => Full(in.map(_.asInstanceOf[ElemType]))
    }

  def asJValue: JValue = valueBox.map(JArray) openOr (JNothing: JValue)
  def setFromJValue(jvalue: JValue): Box[List[ElemType]] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JArray(values)               => setBox(checkValueTypes(values))
    case other                        => setBox(expectedA("JArray containing " + elemType.toString, other))
  }
}

/** Field that contains a homogeneous array of subrecords */
class JSONSubRecordArrayField[OwnerType <: JSONRecord[OwnerType], SubRecordType <: JSONRecord[SubRecordType]]
                             (rec: OwnerType, valueMeta: JSONMetaRecord[SubRecordType])(implicit valueType: Manifest[SubRecordType])
  extends Field[List[SubRecordType], OwnerType] with MandatoryTypedField[List[SubRecordType]]
{
  def this(rec: OwnerType, value: List[SubRecordType])(implicit subRecordType: Manifest[SubRecordType]) = {
      this(rec, value.first.meta)
      set(value)
  }

  def this(rec: OwnerType, valueMeta: JSONMetaRecord[SubRecordType], value: Box[List[SubRecordType]])
          (implicit subRecordType: Manifest[SubRecordType]) = {
      this(rec, valueMeta)
      setBox(value)
  }

  def owner = rec
  def asJs = asJValue
  def toForm: Box[NodeSeq] = Empty // FIXME
  def defaultValue = Nil

  def setFromString(s: String): Box[List[SubRecordType]] =
    setBox(tryo(JsonParser.parse(s)) flatMap {
      case JArray(values) => fromJValues(values)
      case other          => expectedA("JSON string containing an array of " + valueMeta.getClass.getSuperclass.getName, other)
    })

  def setFromAny(in: Any): Box[List[SubRecordType]] = genericSetFromAny(in)

  private def fromJValues(jvalues: List[JValue]): Box[List[SubRecordType]] =
    jvalues
      .foldLeft[Box[List[SubRecordType]]](Full(Nil)) {
        (prev, cur) => prev.flatMap {
          rest => valueMeta.fromJValue(cur).map(_::rest)
        }
      }
      .map(_.reverse)

  def asJValue: JArray = JArray(value.map(_.asJValue))
  def setFromJValue(jvalue: JValue): Box[List[SubRecordType]] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JArray(jvalues)              => setBox(fromJValues(jvalues))
    case other                        => setBox(expectedA("JArray containing " + valueMeta.getClass.getSuperclass.getName, other))
  }
}

}
}
