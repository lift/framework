/*
 * Copyright 2010-2012 WorldWide Conferencing, LLC
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

package net.liftweb
package mongodb
package record
package field

import common._
import http.js.JE._
import json._
import util.Helpers.tryo
import net.liftweb.record.{Field, FieldHelpers, MandatoryTypedField}

import scala.xml.NodeSeq

import com.mongodb._

class JObjectField[OwnerType <: BsonRecord[OwnerType]](rec: OwnerType)
extends Field[JObject, OwnerType]
with MandatoryTypedField[JObject]
with MongoFieldFlavor[JObject] {

  def owner = rec

  def asJValue = valueBox openOr (JNothing: JValue)

  def setFromJValue(jvalue: JValue): Box[JObject] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case jo: JObject => setBox(Full(jo))
    case other => setBox(FieldHelpers.expectedA("JObject", other))
  }

  def defaultValue = JObject(List())

  def setFromAny(in: Any): Box[JObject] = in match {
    case dbo: DBObject => setBox(setFromDBObject(dbo))
    case jv: JObject => setBox(Full(jv))
    case Some(jv: JObject) => setBox(Full(jv))
    case Full(jv: JObject) => setBox(Full(jv))
    case seq: Seq[_] if !seq.isEmpty => seq.map(setFromAny).apply(0)
    case (s: String) :: _ => setFromString(s)
    case null => setBox(Full(null))
    case s: String => setFromString(s)
    case None | Empty | Failure(_, _, _) => setBox(Full(null))
    case o => setFromString(o.toString)
  }

  // assume string is json
  def setFromString(in: String): Box[JObject] = {
    // use lift-json to parse string into a JObject
    setBox(tryo(JsonParser.parse(in).asInstanceOf[JObject]))
  }

  def toForm: Box[NodeSeq] = Empty

  def asDBObject: DBObject = valueBox
    .map { v => JObjectParser.parse(v)(owner.meta.formats) }
    .openOr(new BasicDBObject)

  def setFromDBObject(obj: DBObject): Box[JObject] =
    Full(JObjectParser.serialize(obj)(owner.meta.formats).asInstanceOf[JObject])
}
