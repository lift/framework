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

import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.xml.NodeSeq

import net.liftweb.common.{Box, Empty, Failure, Full}
import net.liftweb.http.js.JsExp
import net.liftweb.http.js.JE.{JsNull, JsRaw}
import net.liftweb.json._
import net.liftweb.record._
import net.liftweb.util.Helpers.tryo

import com.mongodb._
import org.bson._
import org.bson.codecs.{BsonDocumentCodec, BsonTypeCodecMap, Codec, DecoderContext, Encoder, EncoderContext}
import org.bson.codecs.configuration.CodecRegistry

/**
  * Note: setting optional_? = false will result in incorrect equals behavior when using setFromJValue
  */
class MongoMapField[OwnerType <: BsonRecord[OwnerType], MapValueType](rec: OwnerType)
  extends Field[Map[String, MapValueType], OwnerType] with MandatoryTypedField[Map[String, MapValueType]]
  with MongoFieldFlavor[Map[String, MapValueType]]
  with BsonableField[Map[String, MapValueType]]
{

  import mongodb.Meta.Reflection._

  def owner = rec

  def defaultValue = Map[String, MapValueType]()

  def setFromAny(in: Any): Box[Map[String, MapValueType]] = {
    in match {
      case dbo: DBObject => setFromDBObject(dbo)
      case doc: Document => setFromDocument(doc)
      case map: Map[_, _] => setBox(Full(map.asInstanceOf[Map[String, MapValueType]]))
      case Some(map: Map[_, _]) => setBox(Full(map.asInstanceOf[Map[String, MapValueType]]))
      case Full(map: Map[_, _]) => setBox(Full(map.asInstanceOf[Map[String, MapValueType]]))
      case (map: Map[_, _]) :: _ => setBox(Full(map.asInstanceOf[Map[String, MapValueType]]))
      case s: String => setFromString(s)
      case Some(s: String) => setFromString(s)
      case Full(s: String) => setFromString(s)
      case null|None|Empty => setBox(defaultValueBox)
      case f: Failure => setBox(f)
      case o => setFromString(o.toString)
    }
  }

  def setFromJValue(jvalue: JValue) = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JObject(obj) => setBox(Full(
      Map() ++ obj.map(jf => (jf.name, jf.value.values.asInstanceOf[MapValueType]))
    ))
    case other => setBox(FieldHelpers.expectedA("JObject", other))
  }

  def setFromString(in: String): Box[Map[String, MapValueType]] = tryo(JsonParser.parse(in)) match {
    case Full(jv: JValue) => setFromJValue(jv)
    case f: Failure => setBox(f)
    case other => setBox(Failure("Error parsing String into a JValue: "+in))
  }

  def toForm: Box[NodeSeq] = Empty

  def asJValue: JValue = JObject(value.keys.map {
    k =>
      JField(k, value(k).asInstanceOf[AnyRef] match {
        case x if primitive_?(x.getClass) => primitive2jvalue(x)
        case x if mongotype_?(x.getClass) => mongotype2jvalue(x)(owner.meta.formats)
        case x if datetype_?(x.getClass) => datetype2jvalue(x)(owner.meta.formats)
        case _ => JNothing
      })
  }.toList)

  /*
  * Convert this field's value into a DBObject so it can be stored in Mongo.
  */
  @deprecated("This was replaced with the functions from 'BsonableField'.", "3.4.2")
  def asDBObject: DBObject = {
    val dbo = new BasicDBObject
    value.keys.foreach { key =>
      value.get(key).foreach { innerValue =>
        dbo.put(key.toString, innerValue.asInstanceOf[Object])
      }
    }
    dbo
  }

  // set this field's value using a DBObject returned from Mongo.
  @deprecated("This was replaced with the functions from 'BsonableField'.", "3.4.2")
  def setFromDBObject(dbo: DBObject): Box[Map[String, MapValueType]] = {
    setBox(Full(
      Map() ++ dbo.keySet.asScala.map {
        k => (k.toString, dbo.get(k).asInstanceOf[MapValueType])
      }
    ))
  }

  // set this field's value using a bson.Document returned from Mongo.
  @deprecated("This was replaced with the functions from 'BsonableField'.", "3.4.2")
  def setFromDocument(doc: Document): Box[Map[String, MapValueType]] = {
    val map = scala.collection.mutable.Map[String, MapValueType]()

    doc.keySet.asScala.foreach { k =>
      map += k -> doc.get(k).asInstanceOf[MapValueType]
    }

    setBox {
      Full(map.toMap)
    }
  }

  @deprecated("This was replaced with the functions from 'BsonableField'.", "3.4.2")
  def asDocument: Document = {
    val doc = new Document()

    value.keys.view.foreach { k =>
      doc.append(k, value.getOrElse(k, "").asInstanceOf[AnyRef])
    }

    doc
  }

  def setFromBsonReader(reader: BsonReader, context: DecoderContext, registry: CodecRegistry, bsonTypeCodecMap: BsonTypeCodecMap): Box[Map[String, MapValueType]] = {
    reader.getCurrentBsonType match {
      case BsonType.NULL =>
        reader.readNull()
        Empty
      case BsonType.DOCUMENT =>
        setBox(tryo(readMap(reader, context, registry, bsonTypeCodecMap).asInstanceOf[Map[String, MapValueType]]))
      case bsonType =>
        Failure(s"Invalid BsonType for field ${name}: ${bsonType}")
    }
  }

  def writeToBsonWriter(writer: BsonWriter, context: EncoderContext, registry: CodecRegistry, bsonTypeCodecMap: BsonTypeCodecMap): Unit = {
    writer.writeName(name)
    writeMap(writer, value.asInstanceOf[Map[String, Any]], context.getChildContext, registry)
  }
}

