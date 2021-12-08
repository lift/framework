/*
 * Copyright 2010-2020 WorldWide Conferencing, LLC
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

import com.mongodb._
import net.liftweb.common._
import net.liftweb.json._
import net.liftweb.record._
import net.liftweb.util.Helpers.tryo

import org.bson._
import org.bson.codecs.{BsonDocumentCodec, BsonTypeCodecMap, Codec, DecoderContext, EncoderContext}
import org.bson.codecs.configuration.CodecRegistry

import scala.xml.NodeSeq

trait JObjectTypedField[OwnerType <: BsonRecord[OwnerType]] extends TypedField[JObject]
  with Field[JObject, OwnerType]
  with MongoFieldFlavor[JObject]
  with BsonDocumentJObjectField[JObject]
{

  override implicit val formats = owner.meta.formats

  def setFromJValue(jvalue: JValue): Box[JObject] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case jo: JObject => setBox(Full(jo))
    case other => setBox(FieldHelpers.expectedA("JObject", other))
  }

  def setFromAny(in: Any): Box[JObject] = in match {
    case dbo: DBObject => setBox(setFromDBObject(dbo))
    case doc: Document => setBox(setFromDocument(doc))
    case jv: JObject => setBox(Full(jv))
    case Some(jv: JObject) => setBox(Full(jv))
    case Full(jv: JObject) => setBox(Full(jv))
    case seq: Seq[_] if seq.nonEmpty => seq.map(setFromAny).head
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

  @deprecated("This was replaced with the functions from 'BsonableField'.", "3.4.3")
  def asDBObject: DBObject = valueBox
    .map { v => JObjectParser.parse(v)(owner.meta.formats) }
    .openOr(new BasicDBObject)

  @deprecated("This was replaced with the functions from 'BsonableField'.", "3.4.3")
  def setFromDBObject(obj: DBObject): Box[JObject] =
    Full(JObjectParser.serialize(obj)(owner.meta.formats).asInstanceOf[JObject])

  def setFromDocument(obj: Document): Box[JObject] =
    Full(JObjectParser.serialize(obj)(owner.meta.formats).asInstanceOf[JObject])

  def asJValue: JValue = valueBox openOr (JNothing: JValue)
}

class JObjectField[OwnerType <: BsonRecord[OwnerType]](@deprecatedName(Symbol("rec")) val owner: OwnerType)
  extends JObjectTypedField[OwnerType] with MandatoryTypedField[JObject] {

  def this(owner: OwnerType, value: JObject) = {
    this(owner)
    setBox(Full(value))
  }

  def defaultValue = JObject(List())

}

class OptionalJObjectField[OwnerType <: BsonRecord[OwnerType]](val owner: OwnerType)
  extends JObjectTypedField[OwnerType] with OptionalTypedField[JObject] {

  def this(owner: OwnerType, value: Box[JObject]) = {
    this(owner)
    setBox(value)
  }

}
