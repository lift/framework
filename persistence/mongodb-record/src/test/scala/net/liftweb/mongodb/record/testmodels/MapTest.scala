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

import fixtures._

import java.util.{Date, UUID}
import java.util.regex.Pattern

import net.liftweb.common._
import net.liftweb.json._
import net.liftweb.json.ext.EnumSerializer
import net.liftweb.mongodb.codecs.{BsonTypeClassMap, JodaDateTimeCodec}
import net.liftweb.mongodb.record.codecs.RecordCodec
import net.liftweb.mongodb.record.field._

import org.bson.{BsonDocument, BsonType}
import org.bson.codecs.configuration.CodecRegistries
import org.bson.types.ObjectId
import org.joda.time.DateTime

import com.mongodb._

class MapTest private () extends MongoRecord[MapTest] with StringPk[MapTest] {
  def meta = MapTest

  object mandatoryStringMapField extends MongoMapField[MapTest, String](this)
  object mandatoryIntMapField extends MongoMapField[MapTest, Int](this)

  object binaryMapField extends MongoMapField[MapTest, Array[Byte]](this)
  object booleanMapField extends MongoMapField[MapTest, Boolean](this)
  object dateMapField extends MongoMapField[MapTest, Date](this)
  object decimalMapField extends MongoMapField[MapTest, BigDecimal](this)
  object doubleMapField extends MongoMapField[MapTest, Double](this)
  object longMapField extends MongoMapField[MapTest, Long](this)

  object patternMapField extends MongoMapField[MapTest, Pattern](this) {
    override def equals(other: Any): Boolean = {
      other match {
        case that: MongoMapField[MapTest, Pattern] =>
          that.value.toSeq.corresponds(this.value.toSeq) { (a,b) =>
            a._1 == b._1 && // keys
            a._2.pattern == b._2.pattern && a._2.flags == b._2.flags
          }
        case _ =>
          false
      }
    }
  }

  object stringListMapField extends MongoMapField[MapTest, List[String]](this)
  object stringMapMapField extends MongoMapField[MapTest, Map[String, String]](this)
  object uuidMapField extends MongoMapField[MapTest, UUID](this)
}

object MapTest extends MapTest with MongoMetaRecord[MapTest] {
  override def formats = allFormats

  override def codecRegistry = RecordCodec.defaultRegistry
  override def bsonTypeClassMap = BsonTypeClassMap(
    (BsonType.REGULAR_EXPRESSION -> classOf[Pattern]),
    (BsonType.BINARY -> classOf[Array[Byte]]),
    (BsonType.DECIMAL128 -> classOf[BigDecimal]),
    (BsonType.DOCUMENT, classOf[BsonDocument])
  )
}

class MapTestRecord private () extends MongoRecord[MapTestRecord] with StringPk[MapTestRecord] {
  def meta = MapTestRecord

  object mandatoryStringMapField extends MongoMapField[MapTestRecord, String](this)
  object mandatoryIntMapField extends MongoMapField[MapTestRecord, Int](this)
}

object MapTestRecord extends MapTestRecord with MongoMetaRecord[MapTestRecord] {
  override def formats = allFormats
}

class JodaTimeMapTest private () extends MongoRecord[JodaTimeMapTest] with ObjectIdPk[JodaTimeMapTest] {

  def meta = JodaTimeMapTest

  object jodatimeMapField extends MongoMapField[JodaTimeMapTest, DateTime](this)
}

object JodaTimeMapTest extends JodaTimeMapTest with MongoMetaRecord[JodaTimeMapTest] {
  override def bsonTypeClassMap: BsonTypeClassMap = BsonTypeClassMap((BsonType.DATE_TIME -> classOf[DateTime]))
}
