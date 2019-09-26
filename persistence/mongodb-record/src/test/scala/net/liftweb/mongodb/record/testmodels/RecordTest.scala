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
package testmodels

import scala.util.Random

import net.liftweb.common._
import net.liftweb.record._
import net.liftweb.record.field._

import org.bson._
import org.bson.codecs.{DecoderContext, EncoderContext}
import org.bson.codecs.configuration.{CodecRegistry, CodecRegistries}

import com.mongodb._
import com.mongodb.client.{MongoClients, MongoCollection}
import com.mongodb.client.model.Filters.{eq => eqs}

import net.liftweb.mongodb.codecs.{BigDecimalCodec, BsonTypeClassMap}
import net.liftweb.mongodb.record.codecs.CollectibleRecordCodec
import net.liftweb.mongodb.record.testmodels._
import net.liftweb.record._

class RecordTest private () extends Record[RecordTest] {

  def meta = RecordTest

  object id extends IntField(this) {
    override def name = "_id"
    override def defaultValue = Random.nextInt
  }

  object stringfield extends StringField(this, 100)
}

object RecordTest extends RecordTest with MetaRecord[RecordTest]

object MongoConfig {
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

  private val mongoClient = MongoClients.create()

  val main = mongoClient.getDatabase("record_test_db")
}

object RecordTestStore {
  val codec = CollectibleRecordCodec(RecordTest, MongoConfig.defaultRegistry, MongoConfig.defaultBsonTypeClassMap)

  private val registry = CodecRegistries.fromRegistries(
    CodecRegistries.fromCodecs(codec),
    MongoConfig.defaultRegistry
  )

  val collection: MongoCollection[RecordTest] = MongoConfig.main.getCollection("record_test", classOf[RecordTest]).withCodecRegistry(registry)
}