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

import net.liftweb.common._
import net.liftweb.mongodb.record.field._
import net.liftweb.record.field.StringField

import com.mongodb._

import org.bson.codecs.Codec
import org.bson.codecs.configuration.CodecRegistries

class TestSubRecord private () extends BsonRecord[TestSubRecord] {
  def meta = TestSubRecord

  object name extends StringField(this, 12)
}
object TestSubRecord extends TestSubRecord with BsonMetaRecord[TestSubRecord] {
  override def formats = allFormats
}


class BsonRecordTest private () extends MongoRecord[BsonRecordTest] with ObjectIdPk[BsonRecordTest] {

  def meta = BsonRecordTest

  object bsonrecordfield extends BsonRecordField(this, TestSubRecord)
  object bsonrecordlistfield extends BsonRecordListField(this, TestSubRecord)
  object bsonrecordmapfield extends BsonRecordMapField(this, TestSubRecord)
}

object BsonRecordTest extends BsonRecordTest with MongoMetaRecord[BsonRecordTest]

/*
 * SubRecord fields
 */
class SubRecord private () extends BsonRecord[SubRecord] {
  def meta = SubRecord

  object name extends StringField(this, 12)
  object subsub extends BsonRecordField(this, SubSubRecord)
  object subsublist extends BsonRecordListField(this, SubSubRecord)
  object when extends DateField(this)
  object slist extends MongoListField[SubRecord, String](this)
  object smap extends MongoMapField[SubRecord, String](this)
  object oid extends ObjectIdField(this)
  object pattern extends PatternField(this)
  object uuid extends UUIDField(this)
}
object SubRecord extends SubRecord with BsonMetaRecord[SubRecord] {
  override def formats = allFormats
}

class SubSubRecord private () extends BsonRecord[SubSubRecord] {
  def meta = SubSubRecord

  object name extends StringField(this, 12)
}
object SubSubRecord extends SubSubRecord with BsonMetaRecord[SubSubRecord] {
  override def formats = allFormats
}

class SubRecordTestRecord private () extends MongoRecord[SubRecordTestRecord] with ObjectIdPk[SubRecordTestRecord] {
  def meta = SubRecordTestRecord

  object mandatoryBsonRecordField extends BsonRecordField(this, SubRecord)
  object optioalBsonRecordField extends OptionalBsonRecordField(this, SubRecord)
  object legacyOptionalBsonRecordField extends BsonRecordField(this, SubRecord) {
    override def optional_? = true
  }

  object mandatoryBsonRecordListField extends BsonRecordListField(this, SubRecord)
  object legacyOptionalBsonRecordListField extends BsonRecordListField(this, SubRecord) {
    override def optional_? = true
  }
}
object SubRecordTestRecord extends SubRecordTestRecord with MongoMetaRecord[SubRecordTestRecord] {
  override def formats = allFormats
}

class BsonRecordMapTest private () extends MongoRecord[BsonRecordMapTest] with ObjectIdPk[BsonRecordMapTest] {

  def meta = BsonRecordMapTest

  object bsonrecordmapfield extends BsonRecordMapField(this, TestSubRecord)
}

object BsonRecordMapTest extends BsonRecordMapTest with MongoMetaRecord[BsonRecordMapTest]

class BsonRecordListTest private () extends MongoRecord[BsonRecordListTest] with ObjectIdPk[BsonRecordListTest] {

  def meta = BsonRecordListTest

  object bsonrecordlistfield extends BsonRecordListField(this, TestSubRecord)
}

object BsonRecordListTest extends BsonRecordListTest with MongoMetaRecord[BsonRecordListTest]
