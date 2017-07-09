/*
 * Copyright 2010-2017 WorldWide Conferencing, LLC
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
import org.bson.Document

import scala.xml.NodeSeq

trait JObjectTypedField[OwnerType <: BsonRecord[OwnerType]] extends TypedField[JObject]
  with Field[JObject, OwnerType] with MongoFieldFlavor[JObject] {

  override def setFromJValue(jvalue: JValue): Box[JObject] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case jo: JObject => setBox(Full(jo))
    case other => setBox(FieldHelpers.expectedA("JObject", other))
  }

  override def setFromAny(in: Any): Box[JObject] = in match {
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
  override def setFromString(in: String): Box[JObject] = {
    // use lift-json to parse string into a JObject
    setBox(tryo(JsonParser.parse(in).asInstanceOf[JObject]))
  }

  override def toForm: Box[NodeSeq] = Empty

  override def asDBObject: DBObject = valueBox
    .map { v => JObjectParser.parse(v)(owner.meta.formats) }
    .openOr(new BasicDBObject)

  override def setFromDBObject(obj: DBObject): Box[JObject] =
    Full(JObjectParser.serialize(obj)(owner.meta.formats).asInstanceOf[JObject])

  def setFromDocument(obj: Document): Box[JObject] =
    Full(JObjectParser.serialize(obj)(owner.meta.formats).asInstanceOf[JObject])

  override def asJValue: JValue = valueBox openOr (JNothing: JValue)
}

class JObjectField[OwnerType <: BsonRecord[OwnerType]](@deprecatedName('rec, "Lift 3.2") override val owner: OwnerType)
  extends JObjectTypedField[OwnerType] with MandatoryTypedField[JObject] {

  def this(owner: OwnerType, value: JObject) = {
    this(owner)
    setBox(Full(value))
  }

  override def defaultValue = JObject(List())

}

class OptionalJObjectField[OwnerType <: BsonRecord[OwnerType]](override val owner: OwnerType)
  extends JObjectTypedField[OwnerType] with OptionalTypedField[JObject] {

  def this(owner: OwnerType, value: Box[JObject]) = {
    this(owner)
    setBox(value)
  }

}
