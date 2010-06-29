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
package mongodb {
package record {
package field {

import scala.xml.{NodeSeq, Text}

import _root_.net.liftweb.common.{Box, Empty, Failure, Full}

import _root_.net.liftweb.http.js.JE.{JsNull, Str}
import _root_.net.liftweb.json.JsonAST._
import _root_.net.liftweb.json.{JsonParser, Printer}
import _root_.net.liftweb.record.{Field, FieldHelpers, MandatoryTypedField, Record}
import _root_.net.liftweb.util.Helpers.tryo

import com.mongodb.DBObject

abstract class JsonObjectField[OwnerType <: MongoRecord[OwnerType], JObjectType <: JsonObject[JObjectType]]
  (rec: OwnerType, valueMeta: JsonObjectMeta[JObjectType])
  extends Field[JObjectType, OwnerType] with MandatoryTypedField[JObjectType] with MongoFieldFlavor[JObjectType] {

  def owner = rec

  implicit val formats = owner.meta.formats

  /**
   * Convert the field value to an XHTML representation
   */
  override def toForm: Box[NodeSeq] = Empty // FIXME

  /** Encode the field value into a JValue */
  def asJValue: JValue = value.asJObject

  /*
  * Decode the JValue and set the field to the decoded value.
  * Returns Empty or Failure if the value could not be set
  */
  def setFromJValue(jvalue: JValue): Box[JObjectType] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case o: JObject => setBox(tryo(valueMeta.create(o)))
    case other => setBox(FieldHelpers.expectedA("JObject", other))
  }

  def setFromAny(in: Any): Box[JObjectType] = in match {
    case dbo: DBObject => setFromDBObject(dbo)
    case value: JObjectType => setBox(Full(value))
    case Some(value: JObjectType) => setBox(Full(value))
    case Full(value: JObjectType) => setBox(Full(value))
    case (value: JObjectType) :: _ => setBox(Full(value))
    case s: String => setFromString(s)
    case Some(s: String) => setFromString(s)
    case Full(s: String) => setFromString(s)
    case null|None|Empty => setBox(defaultValueBox)
    case f: Failure => setBox(f)
    case o => setFromString(o.toString)
  }

  // parse String into a JObject
  def setFromString(in: String): Box[JObjectType] = tryo(JsonParser.parse(in)) match {
    case Full(jv: JValue) => setFromJValue(jv)
    case f: Failure => setBox(f)
    case other => setBox(Failure("Error parsing String into a JValue: "+in))
  }

  /*
  * Convert this field's value into a DBObject so it can be stored in Mongo.
  */
  def asDBObject: DBObject = JObjectParser.parse(asJValue.asInstanceOf[JObject])

  // set this field's value using a DBObject returned from Mongo.
  def setFromDBObject(dbo: DBObject): Box[JObjectType] =
    setFromJValue(JObjectParser.serialize(dbo).asInstanceOf[JObject])
}

}
}
}
}
