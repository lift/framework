/*
 * Copyright 2020 WorldWide Conferencing, LLC
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

import java.util.UUID

import net.liftweb.common._
import net.liftweb.json._
import net.liftweb.record.Field

import org.bson._
import org.bson.codecs._
import org.bson.codecs.configuration.CodecRegistry

/**
 * A trait for creating custom Fields. Allows writing your own
 * functions for encoding to and decoding from Bson.
 */
trait BsonableField[T] {
  this: Field[T, _] =>

  private val uuidClass = classOf[UUID]

  /**
   * Set this field's value from a BsonReader.
   */
  def setFromBsonReader(reader: BsonReader, context: DecoderContext, registry: CodecRegistry, bsonTypeCodecMap: BsonTypeCodecMap): Box[T]

  /**
   * Write this field's value to a BsonWriter.
   */
  def writeToBsonWriter(writer: BsonWriter, context: EncoderContext, registry: CodecRegistry, bsonTypeCodecMap: BsonTypeCodecMap): Unit

  /**
   * Helper function to read a value to a BsonDocument.
   */
  protected def readValueToBsonDocument(reader: BsonReader, context: DecoderContext, registry: CodecRegistry): BsonDocument = {
    val codec = new BsonDocumentCodec(registry)
    val result = codec.decode(reader, context).asInstanceOf[BsonDocument]
    result
  }

  /**
   * Helper function to read an array to a List of BsonDocuments.
   */
  protected def readArrayToBsonDocument(reader: BsonReader, context: DecoderContext, registry: CodecRegistry): List[BsonDocument] = {
    reader.readStartArray()
    val list = mutable.ListBuffer[BsonDocument]()
    while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
      val codec = new BsonDocumentCodec(registry)
      list.append(codec.decode(reader, context).asInstanceOf[BsonDocument])
    }
    reader.readEndArray()
    list.toList
  }

  /**
   * Helper function to write a value to a BsonWriter.
   */
  protected def writeValue[T](writer: BsonWriter, encoderContext: EncoderContext, value: T, codecRegistry: CodecRegistry): Unit = {
    value match {
      case isNull if value == null =>
        writer.writeNull()
      case map: Map[_, _] =>
        writeMap(writer, map.asInstanceOf[Map[String, Any]], encoderContext.getChildContext, codecRegistry)
      case list: Iterable[_] =>
        writeIterable(writer, list, encoderContext.getChildContext, codecRegistry)
      case _ =>
        val codec = codecRegistry.get(value.getClass).asInstanceOf[Encoder[T]]
        encoderContext.encodeWithChildContext(codec, writer, value)
    }
  }

  /**
   * Helper function to write a Map to a BsonWriter.
   */
  protected def writeMap(writer: BsonWriter, map: Map[String, Any], encoderContext: EncoderContext, codecRegistry: CodecRegistry): Unit = {
    writer.writeStartDocument()
    map.foreach(kv => {
      writer.writeName(kv._1)
      writeValue(writer, encoderContext, kv._2, codecRegistry)
    })
    writer.writeEndDocument()
  }

  /**
   * Helper function to write an Iterable to a BsonWriter.
   */
  protected def writeIterable(writer: BsonWriter, list: Iterable[_], encoderContext: EncoderContext, codecRegistry: CodecRegistry): Unit = {
    writer.writeStartArray()
    list.foreach(value => writeValue(writer, encoderContext, value, codecRegistry))
    writer.writeEndArray()
  }

  /**
   * Helper function to read a value from a BsonReader.
   */
  protected def readValue(reader: BsonReader, decoderContext: DecoderContext, codecRegistry: CodecRegistry, bsonTypeCodecMap: BsonTypeCodecMap): Any = {
    reader.getCurrentBsonType match {
      case BsonType.NULL =>
        reader.readNull()
        null
      case BsonType.ARRAY =>
        readList(reader, decoderContext, codecRegistry, bsonTypeCodecMap)
      case BsonType.DOCUMENT =>
        readMap(reader, decoderContext, codecRegistry, bsonTypeCodecMap)
      case BsonType.BINARY if BsonBinarySubType.isUuid(reader.peekBinarySubType) && reader.peekBinarySize == 16 =>
        codecRegistry.get(uuidClass).decode(reader, decoderContext)
      case bsonType: BsonType =>
        bsonTypeCodecMap.get(bsonType).decode(reader, decoderContext)
    }
  }

  /**
   * Helper function to read a Map from a BsonReader.
   */
  protected def readMap(reader: BsonReader, decoderContext: DecoderContext, codecRegistry: CodecRegistry, bsonTypeCodecMap: BsonTypeCodecMap): Map[String, _] = {
    val map = mutable.Map[String, Any]()
    reader.readStartDocument()
    while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
      map += (reader.readName -> readValue(reader, decoderContext, codecRegistry, bsonTypeCodecMap))
    }
    reader.readEndDocument()
    map.toMap
  }

  /**
   * Helper function to read a List from a BsonReader.
   */
  protected def readList(reader: BsonReader, decoderContext: DecoderContext, codecRegistry: CodecRegistry, bsonTypeCodecMap: BsonTypeCodecMap): List[_] = {
    reader.readStartArray()
    val list = mutable.ListBuffer[Any]()
    while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
      list.append(readValue(reader, decoderContext, codecRegistry, bsonTypeCodecMap))
    }
    reader.readEndArray()
    list.toList
  }
}

/**
 * A trait for creating custom Fields that are based on JObject and are saved as a document (BsonType.DOCUMENT).
 */
trait BsonDocumentJObjectField[T] extends BsonableField[T] {
  this: Field[T, _] =>

  implicit def formats: Formats = DefaultFormats

  def setFromBsonReader(reader: BsonReader, context: DecoderContext, registry: CodecRegistry, bsonTypeCodecMap: BsonTypeCodecMap): Box[MyType] = {
    reader.getCurrentBsonType match {
      case BsonType.DOCUMENT =>
        val doc = readValueToBsonDocument(reader, context, registry)
        setFromJValue(BsonParser.serialize(doc)(formats))
      case BsonType.NULL =>
        reader.readNull()
        Empty
      case bsonType =>
        Failure(s"Invalid BsonType for field ${name}: ${bsonType}")
    }
  }

  def writeToBsonWriter(writer: BsonWriter, context: EncoderContext, registry: CodecRegistry, bsonTypeCodecMap: BsonTypeCodecMap): Unit = {
    asJValue match {
      case jo: JObject =>
        writer.writeName(name)
        val codec = (new BsonDocumentCodec(registry)).asInstanceOf[Codec[Any]]
        context.encodeWithChildContext(codec, writer, BsonParser.parse(jo)(formats))
      case JNothing | JNull if optional_? =>
      case _ =>
        writer.writeName(name)
        writer.writeNull()
    }
  }
}
