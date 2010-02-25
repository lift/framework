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
import Box.box2Iterable
import _root_.net.liftweb.http.js.{JsExp, JsObj}
import _root_.net.liftweb.json.JsonParser
import _root_.net.liftweb.json.JsonAST.{JArray, JBool, JInt, JDouble, JField, JNothing, JNull, JObject, JString, JValue}
import _root_.net.liftweb.record.{Field, MetaRecord, OwnedField, Record}
import _root_.net.liftweb.record.field._
import _root_.net.liftweb.util.ThreadGlobal
import _root_.net.liftweb.util.BasicTypesHelpers.{toBoolean, toInt}
import _root_.net.liftweb.util.ControlHelpers.tryo
import _root_.net.liftweb.util.Helpers.{base64Decode, base64Encode}
import _root_.net.liftweb.util.TimeHelpers.{boxParseInternetDate, toInternetDate}

private[couchdb] object JSONRecordHelpers {
  def expectedA(what: String, notA: AnyRef): Failure = Failure("Expected a " + what + ", not a " + (if (notA == null) "null" else notA.getClass.getName))

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

  /* Verify that all the fields are JSONFields, since we will later be blindly casting them that way. FIXME? instead of this, just ignore non-JSONFields? */
  for (f <- metaFields) {
    f match {
      case (_: JSONField) => ()
      case other => error("Non-JSONField found in JSONRecord")
    }
  }

  /** Return the name of the field in the encoded JSON object. If the JSONField has overridden jsonName that will be used, otherwise the record field name */
  def jsonName(field: OwnedField[BaseRecord]): String =
    field.asInstanceOf[JSONField].jsonName openOr field.name

  /** Whether or not extra fields in a JObject to decode is an error (false) or not (true). The default is true */
  def ignoreExtraJSONFields: Boolean = true

  /** Whether or not missing fields in a JObject to decode is an error (false) or not (true). The default is true */
  def needAllJSONFields: Boolean = true

  override def asJSON(inst: BaseRecord): JsObj = jvalueToJsExp(asJValue).asInstanceOf[JsObj]

  override def setFieldsFromJSON(inst: BaseRecord, json: String): Box[Unit] =
    setFieldsFromJValue(inst, JsonParser.parse(json))

  /** Encode a record instance into a JValue */
  def asJValue(rec: BaseRecord): JObject = {
    val recordJFields = fields(rec).map(f => JField(jsonName(f), f.asInstanceOf[JSONField].asJValue))
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
      } else Box {
        for {
          jfield <- jfields
          field  <- flds if jsonName(field) == jfield.name
          setOk  <- field.asInstanceOf[JSONField].fromJValue(jfield.value)
        } yield ()
      } map { _ => rec.additionalJFields = jsonFieldsNotInRecord.toList.map(name => jfields.find(_.name == name).get); () }
    }

    jvalue match {
      case JObject(jfields) => fromJFields(jfields)
      case other => expectedA("JObject", other)
    }      
  }
}
 
/** Trait for JSON fields which provides the generic interface for JSON encoding and decoding */
trait JSONField {
  self: OwnedField[_] =>

  /** Return Full(name) to use that name in the encoded JSON object, or Empty to use the same name as in Scala. Defaults to Empty */
  def jsonName: Box[String] = Empty

  /** Encode the field value into a JValue */
  def asJValue: JValue

  /** Decode the JValue and set the field to the decoded value. Returns Empty or Failure if the value could not be set */
  def fromJValue(jvalue: JValue): Box[MyType]
}


/* ****************************************************************************************************************************************** */


/** Field that contains an entire record represented as an inline object value in the final JSON */
class JSONSubRecordField[OwnerType <: JSONRecord[OwnerType], SubRecordType <: JSONRecord[SubRecordType]]
                        (rec: OwnerType, valueMeta: JSONMetaRecord[SubRecordType])(implicit subRecordType: Manifest[SubRecordType])
  extends Field[SubRecordType, OwnerType] with JSONField
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
  def toForm = NodeSeq.Empty // FIXME
  def asXHtml = NodeSeq.Empty // FIXME
  def defaultValue = valueMeta.createRecord

  def setFromString(s: String): Box[SubRecordType] = valueMeta.fromJSON(s)

  def setFromAny(in: Any): Box[SubRecordType] = genericSetFromAny(in)

  def asJValue: JValue = valueBox.map(_.asJValue) openOr (JNothing: JValue)
  def fromJValue(jvalue: JValue): Box[SubRecordType] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case _                            => setBox(fromJValue(jvalue))
  }
}

/** Field that contains an array of some basic JSON type */
class JSONBasicArrayField[OwnerType <: JSONRecord[OwnerType], ValueType <: JValue](rec: OwnerType)(implicit valueType: Manifest[ValueType])
  extends Field[List[ValueType], OwnerType] with JSONField
{
  def this(rec: OwnerType, value: List[ValueType])(implicit valueType: Manifest[ValueType]) = { this(rec); set(value) }
  def this(rec: OwnerType, value: Box[List[ValueType]])(implicit valueType: Manifest[ValueType]) = { this(rec); setBox(value) }

  def owner = rec
  def asJs = asJValue
  def toForm = NodeSeq.Empty // FIXME
  def asXHtml = NodeSeq.Empty // FIXME
  def defaultValue = Nil

  def setFromString(s: String): Box[List[ValueType]] =
    setBox(tryo(JsonParser.parse(s)) flatMap {
      case JArray(values) => checkValueTypes(values)
      case other          => expectedA("JSON string with an array of " + valueType, other)
    })

  def setFromAny(in: Any): Box[List[ValueType]] = genericSetFromAny(in)

  def checkValueTypes(in: List[JValue]): Box[List[ValueType]] =
    in.find(!_.isInstanceOf[ValueType]) match {
      case Some(erroneousValue) if erroneousValue != null =>
          Failure("Value in input array is a " + value.getClass.getName + ", should be a " + valueType.toString)

      case Some(erroneousValue) => Failure("Value in input array is null, should be a " + valueType.toString)
      case None                 => Full(in.map(_.asInstanceOf[ValueType]))
    }

  def asJValue: JValue = valueBox.map(JArray) openOr (JNothing: JValue)
  def fromJValue(jvalue: JValue): Box[List[ValueType]] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JArray(values)               => setBox(checkValueTypes(values))
    case other                        => setBox(expectedA("JArray containing " + valueType.toString, other))
  }
}

/** Field that contains a homogeneous array of subrecords */
class JSONSubRecordArrayField[OwnerType <: JSONRecord[OwnerType], SubRecordType <: JSONRecord[SubRecordType]]
                             (rec: OwnerType, valueMeta: JSONMetaRecord[SubRecordType])(implicit valueType: Manifest[SubRecordType])
  extends Field[List[SubRecordType], OwnerType] with JSONField
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
  def toForm = NodeSeq.Empty // FIXME
  def asXHtml = NodeSeq.Empty // FIXME
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
  def fromJValue(jvalue: JValue): Box[List[SubRecordType]] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JArray(jvalues)              => setBox(fromJValues(jvalues))
    case other                        => setBox(expectedA("JArray containing " + valueMeta.getClass.getSuperclass.getName, other))
  }
}


/* ****************************************************************************************************************************************** */


/** Specialization of JSONField for field types that use some kind of encoded string as the JSON type (e.g. binary data, datetimes) */
private[couchdb] trait JSONEncodedStringFieldMixin extends JSONField {
  self: Field[_, _] =>

  /** Encode the current value of the field as a JValue */
  def encode(value: MyType): String

  /** Decode a JValue, with potential failure represented by Empty or Failure */
  def decode(value: String): Box[MyType]

  def asJValue: JValue = valueBox.map(v => JString(encode(v)): JValue) openOr JNothing
  def fromJValue(jvalue: JValue): Box[MyType] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JString(s)                   => setBox(decode(s))
    case other                        => setBox(expectedA("JString", other))
  }
}

/** Specialization of JSONField for field types that are some derivative of StringField and use JString as the JSON type */
private[couchdb] trait JSONStringFieldMixin extends JSONField {
  self: Field[String, _] =>

  def asJValue: JValue = valueBox.map(v => JString(v): JValue) openOr (JNothing: JValue)
  def fromJValue(jvalue: JValue): Box[MyType] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JString(s)                   => setFromString(s)
    case other                        => expectedA("JString", other)
  }
}

/* ****************************************************************************************************************************************** */

/** Binary data field for JSON records. Encodes as JString containing base64 conversion of binary data. */
class JSONBinaryField[OwnerType <: JSONRecord[OwnerType]](rec: OwnerType)
  extends BinaryField[OwnerType](rec) with JSONEncodedStringFieldMixin
{
  def this(rec: OwnerType, value: Array[Byte])      = { this(rec); set(value) }
  def this(rec: OwnerType, value: Box[Array[Byte]]) = { this(rec); setBox(value) }

  def encode(value: Array[Byte]): String = base64Encode(value)
  def decode(value: String): Box[Array[Byte]] = tryo(base64Decode(value))
}

/** Boolean data field for JSON records. Encodes as JBool. */
class JSONBooleanField[OwnerType <: JSONRecord[OwnerType]](rec: OwnerType)
  extends BooleanField[OwnerType](rec) with JSONField
{
  def this(rec: OwnerType, value: Boolean)      = { this(rec); set(value) }
  def this(rec: OwnerType, value: Box[Boolean]) = { this(rec); setBox(value) }

  def asJValue: JValue = valueBox.map(JBool) openOr (JNothing: JValue)
  def fromJValue(jvalue: JValue): Box[Boolean] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JBool(b)                     => setBox(Full(b))
    case other                        => setBox(expectedA("JBool", other))
  }
}

/** Country data field for JSON records. Encodes as JInt (like JSONEnumField)  */
class JSONCountryField[OwnerType <: JSONRecord[OwnerType]](rec: OwnerType)
  extends CountryField(rec) with JSONField
{
  def this(rec: OwnerType, value: Countries.Value)      = { this(rec); set(value) }
  def this(rec: OwnerType, value: Box[Countries.Value]) = { this(rec); setBox(value) }

  def asJValue: JValue = toInt.map(i => JInt(BigInt(i))) openOr (JNothing: JValue)
  def fromJValue(jvalue: JValue): Box[MyType] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JInt(i)                      => setBox(fromInt(i.intValue))
    case other                        => setBox(expectedA("JInt", other))
  }
}

/** Date/time data field for JSON records. Encodes as JString containing the internet formatted datetime */
class JSONDateTimeField[OwnerType <: JSONRecord[OwnerType]](rec: OwnerType)
  extends DateTimeField[OwnerType](rec) with JSONEncodedStringFieldMixin
{
  def this(rec: OwnerType, value: Calendar)      = { this(rec); set(value) }
  def this(rec: OwnerType, value: Box[Calendar]) = { this(rec); setBox(value) }

  def encode(value: Calendar): String = toInternetDate(value.getTime)
  def decode(value: String): Box[Calendar] = boxParseInternetDate(value).map(d => {
    val cal = Calendar.getInstance
    cal.setTime(d)
    cal
  })
}

/** Decimal data field for JSON records. Encodes as a JString, to preserve decimal points (JDouble being lossy) */
class JSONDecimalField[OwnerType <: JSONRecord[OwnerType]](rec: OwnerType, context: MathContext, scale: Int)
  extends DecimalField[OwnerType](rec, context, scale) with JSONEncodedStringFieldMixin
{
  def this(rec: OwnerType, value:     BigDecimal)              = { this(rec, MathContext.UNLIMITED, value.scale); set(value) }
  def this(rec: OwnerType, value: Box[BigDecimal], scale: Int) = { this(rec, MathContext.UNLIMITED, scale); setBox(value) }
  def this(rec: OwnerType, value:     BigDecimal,              context: MathContext) = { this(rec, context, value.scale); set(value) }
  def this(rec: OwnerType, value: Box[BigDecimal], scale: Int, context: MathContext) = { this(rec, context, scale); setBox(value) }

  def encode(value: BigDecimal): String = value.toString
  def decode(value: String): Box[BigDecimal] = tryo(BigDecimal(value))
}

/** Double data field for JSON records. Encodes as JDouble */
class JSONDoubleField[OwnerType <: JSONRecord[OwnerType]](rec: OwnerType)
  extends DoubleField[OwnerType](rec) with JSONField
{
  def this(rec: OwnerType, value: Double)      = { this(rec); set(value) }
  def this(rec: OwnerType, value: Box[Double]) = { this(rec); setBox(value) }

  def asJValue: JValue = valueBox.map(JDouble) openOr (JNothing: JValue)
  def fromJValue(jvalue: JValue): Box[Double] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JDouble(d)                   => setBox(Full(d))
    case other                        => setBox(expectedA("JDouble", other))
  }
}

/** Email data field for JSON records. Encodes as JString */
class JSONEmailField[OwnerType <: JSONRecord[OwnerType]](rec: OwnerType, maxLength: Int)
  extends EmailField[OwnerType](rec, maxLength) with JSONStringFieldMixin
{
  def this(rec: OwnerType, maxLength: Int, value: String)      = { this(rec, maxLength); set(value) }
  def this(rec: OwnerType, maxLength: Int, value: Box[String]) = { this(rec, maxLength); setBox(value) }
  def this(rec: OwnerType, value: String)      = this(rec, 100, value)
  def this(rec: OwnerType, value: Box[String]) = this(rec, 100, value)
}

/** Enum data field for JSON records. Encodes as JInt */
class JSONEnumField[OwnerType <: JSONRecord[OwnerType], EnumType <: Enumeration]
                   (rec: OwnerType, enum: EnumType)(implicit enumValueType: Manifest[EnumType#Value])
  extends EnumField[OwnerType, EnumType](rec, enum) with JSONField
{
  def this(rec: OwnerType, enum: EnumType, value: EnumType#Value)(implicit enumValueType: Manifest[EnumType#Value]) = {
      this(rec, enum)
      set(value)
  }

  def this(rec: OwnerType, enum: EnumType, value: Box[EnumType#Value])(implicit enumValueType: Manifest[EnumType#Value]) = {
      this(rec, enum)
      setBox(value)
  }

  def asJValue: JValue = toInt.map(i => JInt(BigInt(i))) openOr (JNothing: JValue)
  def fromJValue(jvalue: JValue): Box[EnumType#Value] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JInt(i)                      => setBox(fromInt(i.intValue))
    case other                        => setBox(expectedA("JInt", other))
  }
}

/** Integer data field for JSON records. Encodes as JInt */
class JSONIntField[OwnerType <: JSONRecord[OwnerType]](rec: OwnerType)
  extends IntField[OwnerType](rec) with JSONField
{
  def this(rec: OwnerType, value: Int)      = { this(rec); set(value) }
  def this(rec: OwnerType, value: Box[Int]) = { this(rec); setBox(value) }

  def asJValue: JValue = valueBox.map(i => JInt(BigInt(i))) openOr (JNothing: JValue)
  def fromJValue(jvalue: JValue): Box[Int] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JInt(i)                      => setBox(Full(i.intValue))
    case other                        => setBox(expectedA("JInt", other))
  }
}

/** Locale data field for JSON records. Encodes as JString */
class JSONLocaleField[OwnerType <: JSONRecord[OwnerType]](rec: OwnerType)
  extends LocaleField[OwnerType](rec) with JSONStringFieldMixin
{
  def this(rec: OwnerType, value: String)      = { this(rec); set(value) }
  def this(rec: OwnerType, value: Box[String]) = { this(rec); setBox(value) }
}

/** Long data field for JSON records. Encodes as JInt */
class JSONLongField[OwnerType <: JSONRecord[OwnerType]](rec: OwnerType)
  extends LongField[OwnerType](rec) with JSONField
{
  def this(rec: OwnerType, value: Long)      = { this(rec); set(value) }
  def this(rec: OwnerType, value: Box[Long]) = { this(rec); setBox(value) }

  def asJValue: JValue = valueBox.map(l => JInt(BigInt(l))) openOr (JNothing: JValue)
  def fromJValue(jvalue: JValue): Box[Long] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JInt(i)                      => setBox(Full(i.longValue))
    case other                        => setBox(expectedA("JLong", other))
  }
}

/** Password data field for JSON records. Encodes as JString */
class JSONPasswordField[OwnerType <: JSONRecord[OwnerType]](rec: OwnerType)
  extends PasswordField[OwnerType](rec) with JSONStringFieldMixin
{
  def this(rec: OwnerType, value: String)      = { this(rec); set(value) }
  def this(rec: OwnerType, value: Box[String]) = { this(rec); setBox(value) }
}

/** Postal code data field for JSON records. Encodes as JString */
class JSONPostalCodeField[OwnerType <: JSONRecord[OwnerType]](rec: OwnerType, country: JSONCountryField[OwnerType])
  extends PostalCodeField[OwnerType](rec, country) with JSONStringFieldMixin
{
  def this(rec: OwnerType, country: JSONCountryField[OwnerType], value: String)      = { this(rec, country); set(value) }
  def this(rec: OwnerType, country: JSONCountryField[OwnerType], value: Box[String]) = { this(rec, country); setBox(value) }
}

/** String data field for JSON records. Encodes as JString */
class JSONStringField[OwnerType <: JSONRecord[OwnerType]](rec: OwnerType, maxLength: Int)
  extends StringField[OwnerType](rec, maxLength) with JSONStringFieldMixin
{
  def this(rec: OwnerType, maxLength: Int, value: String)      = { this(rec, maxLength); set(value) }
  def this(rec: OwnerType, maxLength: Int, value: Box[String]) = { this(rec, maxLength); setBox(value) }
}

/** String data field for JSON records using a text area input. Encodes as JString */
class JSONTextareaField[OwnerType <: JSONRecord[OwnerType]](rec: OwnerType, maxLength: Int)
  extends TextareaField[OwnerType](rec, maxLength) with JSONStringFieldMixin
{
  def this(rec: OwnerType, maxLength: Int, value: String)      = { this(rec, maxLength); set(value) }
  def this(rec: OwnerType, maxLength: Int, value: Box[String]) = { this(rec, maxLength); setBox(value) }
}

/** Time zone data field for JSON records. Encodes as JString */
class JSONTimeZoneField[OwnerType <: JSONRecord[OwnerType]](rec: OwnerType)
  extends TimeZoneField[OwnerType](rec) with JSONStringFieldMixin
{
  def this(rec: OwnerType, value: String)      = { this(rec); set(value) }
  def this(rec: OwnerType, value: Box[String]) = { this(rec); setBox(value) }
}

}
}
