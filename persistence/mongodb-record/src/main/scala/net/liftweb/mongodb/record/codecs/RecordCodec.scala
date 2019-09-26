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

package net.liftweb.mongodb
package record
package codecs

import scala.collection.mutable

import java.util.{Calendar, GregorianCalendar, ArrayList, List => JavaList, Map => JavaMap, UUID}
import java.util.regex.Pattern
import java.util.Arrays.asList

import net.liftweb.common._
import net.liftweb.mongodb.codecs._
import net.liftweb.mongodb.record.field._
import net.liftweb.record.{Field, MandatoryTypedField, MetaRecord, Record}
import net.liftweb.record.field._
import net.liftweb.record.field.joda.JodaTimeTypedField
import net.liftweb.util.Helpers.tryo

import org.bson._
import org.bson.codecs.{BsonTypeCodecMap, Codec, Decoder, DecoderContext, Encoder, EncoderContext}
import org.bson.codecs.configuration.{CodecRegistry, CodecRegistries}
import com.mongodb._

import org.joda.time.DateTime

/**
 * A codec for Record instances.
 */
object RecordCodec {

  val defaultLegacyBsonTypeClassMap: BsonTypeClassMap =
    BsonTypeClassMap(
      (BsonType.REGULAR_EXPRESSION -> classOf[Pattern]),
      (BsonType.BINARY -> classOf[Array[Byte]]),
      (BsonType.DOCUMENT, classOf[BsonDocument])
    )

  val defaultLegacyRegistry: CodecRegistry = CodecRegistries.fromRegistries(
    MongoClientSettings.getDefaultCodecRegistry(),
    CodecRegistries.fromCodecs(BigDecimalStringCodec(), CalendarCodec(), JodaDateTimeCodec())
  )

  val defaultBsonTypeClassMap: BsonTypeClassMap =
    BsonTypeClassMap(
      (BsonType.BINARY -> classOf[Array[Byte]]),
      (BsonType.DECIMAL128 -> classOf[BigDecimal]),
      (BsonType.DOCUMENT, classOf[BsonDocument])
    )

  val defaultRegistry: CodecRegistry = CodecRegistries.fromRegistries(
    MongoClientSettings.getDefaultCodecRegistry(),
    CodecRegistries.fromCodecs(BigDecimalCodec())
  )

  val defaultTransformer: Transformer = new Transformer() {
    override def transform(value: Object): Object = value
  }
}

/**
 * A Codec trait for Record typed instances
 */
trait RecordTypedCodec[T <: Record[T]] extends Codec[T] with Loggable {
  def metaRecord: MetaRecord[T]
  def codecRegistry: CodecRegistry
  def bsonTypeClassMap: BsonTypeClassMap
  def valueTransformer: Transformer

  lazy val bsonTypeCodecMap = new BsonTypeCodecMap(bsonTypeClassMap, codecRegistry)

  private val uuidClass = classOf[UUID]

  def decode(reader: BsonReader, decoderContext: DecoderContext): T = {
    val rec: T = metaRecord.createRecord

    reader.readStartDocument()

    while (reader.readBsonType() != BsonType.END_OF_DOCUMENT) {
      val fieldName: String = reader.readName()

      // find the field and set it
      rec.fieldByName(fieldName) match {
        case Full(field) =>
          readAndSetFieldValue(reader, decoderContext, field) match {
            case f: Failure =>
              logger.error(f)
            case _ =>
          }
        case _ =>
          // this field is not in the Record, skip it
          reader.skipValue()
      }
    }

    reader.readEndDocument()

    rec.runSafe {
      rec.fields.foreach(_.resetDirty)
    }

    rec
  }

  /**
   * Read the Bson data and set the Field.
   */
  private def readAndSetFieldValue(reader: BsonReader, decoderContext: DecoderContext, field: Field[_, T]): Box[Any] = {
    val currentBsonType = reader.getCurrentBsonType

    def readNext(): Any = {
      bsonTypeCodecMap.get(currentBsonType).decode(reader, decoderContext)
    }

    field match {
      case f: BsonableField[_] =>
        f.setFromBsonReader(reader, decoderContext, codecRegistry, bsonTypeCodecMap)
      case f: DateTimeTypedField if currentBsonType == BsonType.DATE_TIME => // Calendar
        f.setBox(tryo(CalendarCodec().decode(reader, decoderContext).asInstanceOf[Calendar]))
      case f: JodaTimeTypedField if currentBsonType == BsonType.DATE_TIME => // joda.DateTime
        f.setBox(tryo(JodaDateTimeCodec().decode(reader, decoderContext).asInstanceOf[DateTime]))
      case f: DecimalTypedField if currentBsonType == BsonType.STRING =>
        f.setFromString(readNext().asInstanceOf[String])
      case f: EnumTypedField[_] if currentBsonType == BsonType.INT32 =>
        f.setFromInt(readNext().asInstanceOf[Int])
      case f: EnumNameTypedField[_] if currentBsonType == BsonType.STRING =>
        f.setFromString(readNext().asInstanceOf[String])
      case _ =>
        (currentBsonType match {
          case BsonType.NULL =>
            reader.readNull()
            Empty
          case BsonType.BINARY if BsonBinarySubType.isUuid(reader.peekBinarySubType) && reader.peekBinarySize == 16 =>
            tryo(codecRegistry.get(uuidClass).decode(reader, decoderContext))
          case bsonType: BsonType =>
            tryo(valueTransformer.transform(readNext))
        }) match {
          case Full(v: field.MyType) =>
            field.setBox(Full(v))
          case Empty =>
            field.setBox(Empty)
          case f: Failure =>
            field.setBox(Failure(s"Error reading Bson value: ${reader.getCurrentBsonType}", Empty, f))
        }
    }
  }

  private def readValue(reader: BsonReader, decoderContext: DecoderContext): Any = {
    reader.getCurrentBsonType match {
      case BsonType.NULL =>
        reader.readNull()
        null
      case BsonType.ARRAY =>
        readList(reader, decoderContext)
      case BsonType.DOCUMENT =>
        readMap(reader, decoderContext)
      case BsonType.BINARY if BsonBinarySubType.isUuid(reader.peekBinarySubType) && reader.peekBinarySize == 16 =>
        codecRegistry.get(uuidClass).decode(reader, decoderContext)
      case bsonType: BsonType =>
        valueTransformer.transform(bsonTypeCodecMap.get(bsonType).decode(reader, decoderContext))
    }
  }

  private def readMap(reader: BsonReader, decoderContext: DecoderContext): Map[String, _] = {
    val map = mutable.Map[String, Any]()
    reader.readStartDocument()
    while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
      map += (reader.readName -> readValue(reader, decoderContext))
    }
    reader.readEndDocument()
    map.toMap
  }

  private def readList(reader: BsonReader, decoderContext: DecoderContext): List[_] = {
    reader.readStartArray()
    val list = mutable.ListBuffer[Any]()
    while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
      list.append(readValue(reader, decoderContext))
    }
    reader.readEndArray()
    list.toList
  }

  private def readListToDBObject(reader: BsonReader, decoderContext: DecoderContext): DBObject = {
    reader.readStartArray()
    val list = new BasicDBList
    while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
      list.add(readValue(reader, decoderContext).asInstanceOf[Object])
    }
    reader.readEndArray()
    list
  }

  def encode(writer: BsonWriter, record: T, encoderContext: EncoderContext): Unit = {

    writer.writeStartDocument()

    record.fields().foreach { _ match {
      case field if (field.optional_? && field.valueBox.isEmpty) => // don't write anything
      case field: BsonableField[_] =>
        Option(field.asInstanceOf[BsonableField[Any]]).foreach { bf =>
          bf.writeToBsonWriter(writer, encoderContext, codecRegistry, bsonTypeCodecMap)
        }
      case field: EnumTypedField[_] =>
        field.asInstanceOf[EnumTypedField[Enumeration]].valueBox
          .map(_.id)
          .foreach { id =>
            writer.writeName(field.name)
            writer.writeInt32(id)
          }
      case field: EnumNameTypedField[_] =>
        field.asInstanceOf[EnumNameTypedField[Enumeration]].valueBox
          .map(_.toString)
          .foreach { s =>
            writer.writeName(field.name)
            writer.writeString(s)
          }
      case field => field.valueBox match {
        case Empty if field.optional_? => // don't write anything
        case Empty =>
          sys.error(s"Field value is Empty. Field name: ${field.name}.")
        case Failure(msg, _, _) =>
          sys.error(s"Error reading value. Field name: ${field.name}. Error message: ${msg}")
        case Full(v: Array[Byte]) =>
          writer.writeName(field.name)
          writer.writeBinaryData(new BsonBinary(v))
        case Full(v) =>
          writer.writeName(field.name)
          writeValue(writer, encoderContext, v)
      }
    }}

    writer.writeEndDocument()
  }

  protected def writeValue[T](writer: BsonWriter, encoderContext: EncoderContext, value: T): Unit = {
    value match {
      case isNull if value == null =>
        writer.writeNull()
      case map: Map[_, _] =>
        writeMap(writer, map.asInstanceOf[Map[String, Any]], encoderContext.getChildContext)
      case list: Iterable[_] =>
        writeIterable(writer, list, encoderContext.getChildContext)
      case _ =>
        val codec = codecRegistry.get(value.getClass).asInstanceOf[Encoder[T]]
        encoderContext.encodeWithChildContext(codec, writer, value)
    }
  }

  protected def writeMap(writer: BsonWriter, map: Map[String, Any], encoderContext: EncoderContext): Unit = {
    writer.writeStartDocument()
    map.foreach(kv => {
      writer.writeName(kv._1)
      writeValue(writer, encoderContext, kv._2)
    })
    writer.writeEndDocument()
  }

  protected def writeIterable(writer: BsonWriter, list: Iterable[_], encoderContext: EncoderContext): Unit = {
    writer.writeStartArray()
    list.foreach(value => writeValue(writer, encoderContext, value))
    writer.writeEndArray()
  }
}

/**
 * A Codec for Record instances
 */
case class RecordCodec[T <: Record[T]](
  metaRecord: MetaRecord[T],
  codecRegistry: CodecRegistry = RecordCodec.defaultLegacyRegistry,
  bsonTypeClassMap: BsonTypeClassMap = RecordCodec.defaultLegacyBsonTypeClassMap,
  valueTransformer: Transformer = RecordCodec.defaultTransformer
) extends RecordTypedCodec[T] {
  def getEncoderClass(): Class[T] = metaRecord.createRecord.getClass.asInstanceOf[Class[T]]
}
