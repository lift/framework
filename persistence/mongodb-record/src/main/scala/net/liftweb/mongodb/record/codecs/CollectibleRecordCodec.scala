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

import net.liftweb.mongodb.codecs._
import net.liftweb.record.{Field, MandatoryTypedField, MetaRecord, Record}

import org.bson._
import org.bson.codecs._
import org.bson.codecs.configuration.CodecRegistry

/**
 * A Collectible (requires an _id field) codec for Record instances.
 */
object CollectibleRecordCodec {
  private val idFieldName: String = "_id"
}

case class CollectibleRecordCodec[T <: Record[T]](
  metaRecord: MetaRecord[T],
  codecRegistry: CodecRegistry = RecordCodec.defaultLegacyRegistry,
  bsonTypeClassMap: BsonTypeClassMap = RecordCodec.defaultLegacyBsonTypeClassMap,
  valueTransformer: Transformer = RecordCodec.defaultTransformer
)
  extends RecordTypedCodec[T]
  with CollectibleCodec[T]
{
  override def getEncoderClass(): Class[T] = metaRecord.createRecord.getClass.asInstanceOf[Class[T]]

  /**
   * Fields must be predefined on Records so there's no way to add one if missing.
   */
  def generateIdIfAbsentFromDocument(rec: T): T = {
    rec
  }

  private def findIdField(rec: T): Option[Field[_, T]] = {
    rec.fieldByName(CollectibleRecordCodec.idFieldName).toOption
  }

  def documentHasId(rec: T): Boolean = {
    findIdField(rec).nonEmpty
  }

  def getDocumentId(rec: T): BsonValue = {
    if (!documentHasId(rec)) {
      throw new IllegalStateException("The rec does not contain an _id")
    }

    findIdField(rec) match {
      case Some(field: Field[_, T] with MandatoryTypedField[_]) =>
        val idHoldingDocument = new BsonDocument()
        val writer = new BsonDocumentWriter(idHoldingDocument)
        writer.writeStartDocument()
        writer.writeName(CollectibleRecordCodec.idFieldName)

        writeValue(writer, EncoderContext.builder().build(), field.value.asInstanceOf[Object])

        writer.writeEndDocument()
        idHoldingDocument.get(CollectibleRecordCodec.idFieldName)
      case _ =>
        throw new IllegalStateException("The _id field could not be found")
    }
  }
}
