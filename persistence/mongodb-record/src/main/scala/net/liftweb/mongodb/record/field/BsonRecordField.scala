/*
 * Copyright 2011-2020 WorldWide Conferencing, LLC
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

import net.liftweb.common._
import net.liftweb.http.js.JsExp
import net.liftweb.http.js.JE.JsNull
import net.liftweb.json._
import net.liftweb.record._
import net.liftweb.util.Helpers.tryo

import com.mongodb._

import org.bson._
import org.bson.codecs.{BsonDocumentCodec, BsonTypeCodecMap, Codec, DecoderContext, Encoder, EncoderContext, StringCodec}
import org.bson.codecs.configuration.CodecRegistry

import scala.collection.mutable
import scala.reflect.Manifest
import scala.xml.NodeSeq

/** Field that contains an entire record represented as an inline object value. Inspired by JSONSubRecordField */
abstract class BsonRecordTypedField[OwnerType <: BsonRecord[OwnerType], SubRecordType <: BsonRecord[SubRecordType]]
(val owner: OwnerType, val valueMeta: BsonMetaRecord[SubRecordType])(implicit subRecordType: Manifest[SubRecordType])
  extends Field[SubRecordType, OwnerType]
  with BsonableField[SubRecordType]
{

  def this(owner: OwnerType, valueMeta: BsonMetaRecord[SubRecordType], value: Box[SubRecordType])
    (implicit subRecordType: Manifest[SubRecordType]) = {
    this(owner, valueMeta)
    setBox(value)
  }

  def setFromBsonReader(reader: BsonReader, context: DecoderContext, registry: CodecRegistry, bsonTypeCodecMap: BsonTypeCodecMap): Box[MyType] = {
    reader.getCurrentBsonType match {
      case BsonType.DOCUMENT =>
        setBox(tryo(valueMeta.codec.decode(reader, context)))
      case BsonType.NULL =>
        reader.readNull()
        Empty
      case bsonType =>
        Failure(s"Invalid BsonType for field ${name}: ${bsonType}")
    }
  }

  def writeToBsonWriter(writer: BsonWriter, context: EncoderContext, registry: CodecRegistry, bsonTypeCodecMap: BsonTypeCodecMap): Unit = {
    valueBox match {
      case Empty if optional_? =>
      case Empty =>
        writer.writeName(name)
        val codec = new StringCodec()
        context.encodeWithChildContext(codec, writer, "Empty")
      case Full(v) =>
        writer.writeName(name)
        val codec = registry.get(v.getClass).asInstanceOf[Encoder[SubRecordType]]
        context.encodeWithChildContext(codec, writer, v)
      case Failure(msg, _, _) =>
        writer.writeName(name)
        val codec = new StringCodec()
        context.encodeWithChildContext(codec, writer, s"Failure: ${msg}")
    }
  }

  def asJs = asJValue match {
    case JNothing => JsNull
    case jv => new JsExp {
      lazy val toJsCmd = compactRender(jv)
    }
  }

  def toForm: Box[NodeSeq] = Empty

  def setFromString(s: String): Box[SubRecordType] = valueMeta.fromJsonString(s)

  def setFromAny(in: Any): Box[SubRecordType] = in match {
    case dbo: DBObject => setBox(Full(valueMeta.fromDBObject(dbo)))
    case dbo: Document => setBox(Full(valueMeta.fromDocument(dbo)))
    case _ => genericSetFromAny(in)
  }

  def asJValue: JValue = valueBox.map(_.asJValue) openOr (JNothing: JValue)

  def setFromJValue(jvalue: JValue): Box[SubRecordType] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case _ => setBox(valueMeta.fromJValue(jvalue))
  }
}

class BsonRecordField[OwnerType <: BsonRecord[OwnerType], SubRecordType <: BsonRecord[SubRecordType]]
(@deprecatedName('rec) owner: OwnerType, valueMeta: BsonMetaRecord[SubRecordType])(implicit subRecordType: Manifest[SubRecordType])
  extends BsonRecordTypedField(owner, valueMeta) with MandatoryTypedField[SubRecordType] {

  def this(@deprecatedName('rec) owner: OwnerType, valueMeta: BsonMetaRecord[SubRecordType], value: SubRecordType)(implicit subRecordType: Manifest[SubRecordType]) = {
    this(owner, value.meta)
    set(value)
  }

  def defaultValue = valueMeta.createRecord
}

class OptionalBsonRecordField[OwnerType <: BsonRecord[OwnerType], SubRecordType <: BsonRecord[SubRecordType]]
(owner: OwnerType, valueMeta: BsonMetaRecord[SubRecordType])(implicit subRecordType: Manifest[SubRecordType])
  extends BsonRecordTypedField(owner, valueMeta) with OptionalTypedField[SubRecordType]


/**
 * List of BsonRecords
 */
class BsonRecordListField[OwnerType <: BsonRecord[OwnerType], SubRecordType <: BsonRecord[SubRecordType]]
  (@deprecatedName('rec) owner: OwnerType, val valueMeta: BsonMetaRecord[SubRecordType])(implicit mf: Manifest[SubRecordType])
  extends MongoListField[OwnerType, SubRecordType](owner: OwnerType) {

  import scala.jdk.CollectionConverters._

  override def validations = ((elems: ValueType) => elems.flatMap(_.validate)) :: super.validations

  @deprecated("This was replaced with the functions from 'BsonableField'.", "3.4.3")
  override def asDBObject: DBObject = {
    val dbl = new BasicDBList
    value.foreach { v => dbl.add(v.asDBObject) }
    dbl
  }

  @deprecated("This was replaced with the functions from 'BsonableField'.", "3.4.3")
  override def setFromDBObject(dbo: DBObject): Box[List[SubRecordType]] = {
    setBox(Full(dbo.keySet.asScala.toList.map { k =>
      valueMeta.fromDBObject(dbo.get(k).asInstanceOf[DBObject])
    }))
  }

  override def asJValue: JValue = JArray(value.map(_.asJValue))

  override def setFromJValue(jvalue: JValue) = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JArray(arr) => setBox(Full(arr.map { jv =>
      valueMeta.fromJValue(jv) openOr valueMeta.createRecord
    }))
    case other => setBox(FieldHelpers.expectedA("JArray", other))
  }

  @deprecated("This was replaced with the functions from 'BsonableField'.", "3.4.3")
  override def setFromDocumentList(list: java.util.List[Document]): Box[List[SubRecordType]] = {
    setBox(Full(
      list.asScala.toList.map { valueMeta.fromDocument }
    ))
  }

  override def setFromBsonReader(reader: BsonReader, context: DecoderContext, registry: CodecRegistry, bsonTypeCodecMap: BsonTypeCodecMap): Box[List[SubRecordType]] = {
    reader.getCurrentBsonType match {
      case BsonType.ARRAY =>
        setBox(tryo {
          reader.readStartArray()
          val list = mutable.ListBuffer[SubRecordType]()
          while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
            list.append(valueMeta.codec.decode(reader, context))
          }
          reader.readEndArray()
          list.toList
        })
      case BsonType.NULL =>
        reader.readNull()
        Empty
      case bsonType =>
        Failure(s"Invalid BsonType for field ${name}: ${bsonType}")
    }
  }

  override def writeToBsonWriter(writer: BsonWriter, context: EncoderContext, registry: CodecRegistry, bsonTypeCodecMap: BsonTypeCodecMap): Unit = {
    writer.writeName(name)

    writer.writeStartArray()
    value.foreach { v =>
      val codec = registry.get(v.getClass).asInstanceOf[Encoder[SubRecordType]]
      context.encodeWithChildContext(codec, writer, v)
    }
    writer.writeEndArray()
  }
}

/**
 * Map of BsonRecords
 */
class BsonRecordMapField[OwnerType <: BsonRecord[OwnerType], SubRecordType <: BsonRecord[SubRecordType]]
  (owner: OwnerType, val valueMeta: BsonMetaRecord[SubRecordType])(implicit mf: Manifest[SubRecordType])
  extends MongoMapField[OwnerType, SubRecordType](owner: OwnerType)
{
  override def validations = ((elems: ValueType) => elems.values.toList.flatMap(_.validate)) :: super.validations

  override def setFromBsonReader(reader: BsonReader, context: DecoderContext, registry: CodecRegistry, bsonTypeCodecMap: BsonTypeCodecMap): Box[Map[String, SubRecordType]] = {
    reader.getCurrentBsonType match {
      case BsonType.NULL =>
        reader.readNull()
        Empty
      case BsonType.DOCUMENT =>
        setBox(tryo {
          val map = mutable.Map[String, SubRecordType]()
          reader.readStartDocument()
          while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
            map += (reader.readName -> valueMeta.codec.decode(reader, context))
          }
          reader.readEndDocument()
          map.toMap
        })
      case bsonType =>
        Failure(s"Invalid BsonType for field ${name}: ${bsonType}")
    }
  }

  override def writeToBsonWriter(writer: BsonWriter, context: EncoderContext, registry: CodecRegistry, bsonTypeCodecMap: BsonTypeCodecMap): Unit = {
    writer.writeName(name)
    writer.writeStartDocument()
    value.foreach(kv => {
      writer.writeName(kv._1)
      val codec = registry.get(kv._2.getClass).asInstanceOf[Encoder[SubRecordType]]
      context.encodeWithChildContext(codec, writer, kv._2)
    })
    writer.writeEndDocument()
  }
}
