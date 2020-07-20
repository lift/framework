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
import net.liftweb.mongodb.codecs.{BigIntLongCodec, BsonTypeClassMap, JodaDateTimeCodec}
import net.liftweb.mongodb.record.codecs.{RecordCodec}
import net.liftweb.mongodb.record.field._
import net.liftweb.record.field.IntField

import org.bson.BsonType
import org.bson.codecs.configuration.CodecRegistries
import org.bson.types.ObjectId
import org.joda.time.DateTime

import com.mongodb._

class BasicListTestRecord private () extends MongoRecord[BasicListTestRecord] with UUIDPk[BasicListTestRecord] {
  def meta = BasicListTestRecord

  object bigIntListField extends MongoListField[BasicListTestRecord, BigInt](this)
  object binaryListField extends MongoListField[BasicListTestRecord, Array[Byte]](this)
  object booleanListField extends MongoListField[BasicListTestRecord, Boolean](this)
  object decimalListField extends MongoListField[BasicListTestRecord, BigDecimal](this)
  object doubleListField extends MongoListField[BasicListTestRecord, Double](this)
  object longListField extends MongoListField[BasicListTestRecord, Long](this)
  object stringListListField extends MongoListField[BasicListTestRecord, List[String]](this)
  object stringMapListField extends MongoListField[BasicListTestRecord, Map[String, String]](this)
}

object BasicListTestRecord extends BasicListTestRecord with MongoMetaRecord[BasicListTestRecord] {
  override def formats = allFormats

  override def codecRegistry = CodecRegistries.fromRegistries(
    CodecRegistries.fromCodecs(BigIntLongCodec()),
    RecordCodec.defaultRegistry
  )
  override def bsonTypeClassMap = RecordCodec.defaultBsonTypeClassMap
}

class ListTestRecord private () extends MongoRecord[ListTestRecord] with UUIDPk[ListTestRecord] {
  def meta = ListTestRecord

  object mandatoryStringListField extends MongoListField[ListTestRecord, String](this)
  object mandatoryMongoRefListField extends ObjectIdRefListField(this, FieldTypeTestRecord)
  object mandatoryIntListField extends MongoListField[ListTestRecord, Int](this)
  object mandatoryJsonObjectListField extends JsonObjectListField(this, TypeTestJsonObject)
  object caseClassListField extends CaseClassListField[ListTestRecord, CaseClassTestObject](this) {
    override def formats = owner.meta.formats
  }
}

object ListTestRecord extends ListTestRecord with MongoMetaRecord[ListTestRecord] {
  override def formats = allFormats + new EnumSerializer(MyTestEnum)
}


class MongoListTestRecord private () extends MongoRecord[MongoListTestRecord] with UUIDPk[MongoListTestRecord] {
  def meta = MongoListTestRecord

  object objectIdRefListField extends ObjectIdRefListField(this, FieldTypeTestRecord)

  object patternListField extends MongoListField[MongoListTestRecord, Pattern](this) {
    override def equals(other: Any): Boolean = {
      other match {
        case that: MongoListField[MongoListTestRecord, Pattern] =>
          that.value.corresponds(this.value) { (a,b) =>
            a.pattern == b.pattern && a.flags == b.flags
          }
        case _ =>
          false
      }
    }
  }

  object dateListField extends MongoListField[MongoListTestRecord, Date](this)
  object uuidListField extends MongoListField[MongoListTestRecord, UUID](this)
}

object MongoListTestRecord extends MongoListTestRecord with MongoMetaRecord[MongoListTestRecord] {
  override def formats = DefaultFormats.lossless + new ObjectIdSerializer + new PatternSerializer + new DateSerializer
}


class MongoJodaListTestRecord private () extends MongoRecord[MongoJodaListTestRecord] with UUIDPk[MongoJodaListTestRecord] {
  def meta = MongoJodaListTestRecord

  object dateTimeListField extends MongoListField[MongoJodaListTestRecord, DateTime](this)
}

object MongoJodaListTestRecord extends MongoJodaListTestRecord with MongoMetaRecord[MongoJodaListTestRecord] {
  override def formats = DefaultFormats.lossless + new DateTimeSerializer
  override def bsonTypeClassMap: BsonTypeClassMap = BsonTypeClassMap((BsonType.DATE_TIME -> classOf[DateTime]))
}
