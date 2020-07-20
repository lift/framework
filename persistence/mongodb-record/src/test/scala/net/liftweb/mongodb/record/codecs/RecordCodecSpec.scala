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
package codecs

import java.util.{Calendar, Date, UUID}
import java.util.regex.Pattern

import org.bson.types.ObjectId
import org.specs2.mutable.Specification

import net.liftweb.common._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import net.liftweb.mongodb.record.fixtures._
import net.liftweb.mongodb.record.testmodels._
import net.liftweb.record.{MetaRecord, Record}
import net.liftweb.util.Helpers._

import com.mongodb._
import org.bson._
import org.bson.codecs.{BsonTypeClassMap, DecoderContext, EncoderContext}

import org.joda.time.DateTime

/**
 * Systems under specification for RecordCodec.
 */
object RecordCodecSpec extends Specification {
  "RecordCodec Specification".title

  /**
   * Encodes then decodes a BsonRecord instance to/from Bson and asserts they are equal.
   */
  private def testEncodeDecode[T <: BsonRecord[T]](metaRecord: BsonMetaRecord[T], record: T) = {
    val bson = new BsonDocument()
    val writer = new BsonDocumentWriter(bson)

    metaRecord.codec.encode(writer, record, EncoderContext.builder.build)

    val reader = new BsonDocumentReader(bson)
    val result: T = metaRecord.codec.decode(reader, DecoderContext.builder.build)

    result must_== record
  }

  "RecordCodec" should {

    "support Binary fields" in {
      val binData: Array[Byte] = Array(18, 19, 20)
      val rec0 = BinaryTest.createRecord
      val rec1 = BinaryTest.createRecord.binaryfield(binData)

      testEncodeDecode(BinaryTest, rec0)
      testEncodeDecode(BinaryTest, rec1)
    }

    "support Boolean fields" in {
      val rec0 = BooleanTest.createRecord
      val rec1 = BooleanTest.createRecord.booleanfield(true)
      val rec2 = BooleanTest.createRecord.booleanfield(false)

      testEncodeDecode(BooleanTest, rec0)
      testEncodeDecode(BooleanTest, rec1)
      testEncodeDecode(BooleanTest, rec2)
    }

    "support Calendar fields (DateTimeField)" in {
      val rec0 = CalendarTest.createRecord
      val rec1 = CalendarTest.createRecord.calendarfield(Calendar.getInstance)

      testEncodeDecode(CalendarTest, rec0)
      testEncodeDecode(CalendarTest, rec1)
    }

    "support Case Class fields" in {
      val rec0 = CaseClassTest.createRecord
      val tcc = TestCaseClass("hi", 9)
      val rec1 = CaseClassTest.createRecord.caseclassfield(tcc)

      testEncodeDecode(CaseClassTest, rec0)
      testEncodeDecode(CaseClassTest, rec1)
    }

    "support Date fields" in {
      val rec0 = DateTest.createRecord
      val rec1 = DateTest.createRecord.datefield(new Date)

      testEncodeDecode(DateTest, rec0)
      testEncodeDecode(DateTest, rec1)
    }

    "support Decimal fields (legacy)" in {
      val rec0 = LegacyDecimalTest.createRecord
      val rec1 = LegacyDecimalTest.createRecord.decimalfield(BigDecimal("1234.25"))

      testEncodeDecode(LegacyDecimalTest, rec0)
      testEncodeDecode(LegacyDecimalTest, rec1)
    }

    "support Decimal fields" in {
      val rec0 = DecimalTest.createRecord
      val rec1 = DecimalTest.createRecord.decimalfield(BigDecimal("1234.25"))

      testEncodeDecode(DecimalTest, rec0)
      testEncodeDecode(DecimalTest, rec1)
    }

    "support Double fields" in {
      val rec0 = DoubleTest.createRecord
      val rec1 = DoubleTest.createRecord.doublefield(1234)

      testEncodeDecode(DoubleTest, rec0)
      testEncodeDecode(DoubleTest, rec1)
    }

    "support Enum fields" in {
      val rec0 = EnumTest.createRecord
      val rec1 = EnumTest.createRecord.enumfield(TestEnum.Three)

      testEncodeDecode(EnumTest, rec0)
      testEncodeDecode(EnumTest, rec1)
    }

    "support Int fields" in {
      val rec0 = IntTest.createRecord
      val rec1 = IntTest.createRecord.intfield(1234)

      testEncodeDecode(IntTest, rec0)
      testEncodeDecode(IntTest, rec1)
    }

    "support Long fields" in {
      val rec0 = LongTest.createRecord
      val rec1 = LongTest.createRecord.longfield(1234L)

      testEncodeDecode(LongTest, rec0)
      testEncodeDecode(LongTest, rec1)
    }

    "support String fields" in {
      val rec0 = StringTest.createRecord
      val rec1 = StringTest.createRecord.stringfield("abc")
      val rec2 = StringTest.createRecord.optstringfield("def")
      val rec3 = StringTest.createRecord.stringfield("abc").optstringfield("def")
      val rec4 = StringTest.createRecord.stringfieldopt("abc")

      testEncodeDecode(StringTest, rec0)
      testEncodeDecode(StringTest, rec1)
      testEncodeDecode(StringTest, rec2)
      testEncodeDecode(StringTest, rec3)
      testEncodeDecode(StringTest, rec4)
    }

    // joda
    "support Joda DateTime fields (JodaTimeField)" in {
      val rec0 = JodaTimeTest.createRecord
      val rec1 = JodaTimeTest.createRecord.jodatimefield(DateTime.now)

      testEncodeDecode(JodaTimeTest, rec0)
      testEncodeDecode(JodaTimeTest, rec1)
    }

    // mongodb.record.field
    "support BsonRecord fields" in {
      val sub = TestSubRecord.createRecord.name("mrx")

      val rec0 = BsonRecordTest.createRecord
      val rec1 = BsonRecordTest.createRecord.bsonrecordfield(sub)
      val rec2 = BsonRecordTest.createRecord.bsonrecordlistfield(List(sub))
      val rec3 = BsonRecordTest.createRecord.bsonrecordmapfield(Map("a" -> sub))

      testEncodeDecode(BsonRecordTest, rec0)
      testEncodeDecode(BsonRecordTest, rec1)
      testEncodeDecode(BsonRecordTest, rec2)
      testEncodeDecode(BsonRecordTest, rec3)

      val mrec0 = BsonRecordMapTest.createRecord
      val mrec1 = BsonRecordMapTest.createRecord.bsonrecordmapfield(Map("a" -> sub))

      testEncodeDecode(BsonRecordMapTest, mrec0)
      testEncodeDecode(BsonRecordMapTest, mrec1)

      val lrec0 = BsonRecordListTest.createRecord
      val lrec1 = BsonRecordListTest.createRecord.bsonrecordlistfield(List(sub))

      testEncodeDecode(BsonRecordListTest, lrec0)
      testEncodeDecode(BsonRecordListTest, lrec1)
    }

    "support List fields" in {
      val binData: Array[Byte] = Array(11, 19, 20)

      val sub0 = FieldTypeTestRecord.createRecord
      val sub1 = FieldTypeTestRecord.createRecord.mandatoryStringField("mrx")

      val rec0 = ListTestRecord.createRecord
      val rec1 = ListTestRecord.createRecord.mandatoryStringListField(List("a", "b"))
      val rec2 = ListTestRecord.createRecord.mandatoryIntListField(List(1,12))
      val rec3 = ListTestRecord.createRecord.mandatoryMongoRefListField(List(sub1.id.get))
      val rec4 = ListTestRecord.createRecord.mandatoryMongoRefListField(List(sub0.id.get, sub1.id.get))
      val rec5 = ListTestRecord.createRecord.mandatoryJsonObjectListField(List(TypeTestJsonObject(1, "jsonobj1", Map("x" -> "1")), TypeTestJsonObject(2, "jsonobj2", Map("x" -> "2"))))
      val rec6 = ListTestRecord.createRecord.caseClassListField(List(CaseClassTestObject(12, "twelve", MyTestEnum.THREE)))

      testEncodeDecode(ListTestRecord, rec0)
      testEncodeDecode(ListTestRecord, rec1)
      testEncodeDecode(ListTestRecord, rec2)
      testEncodeDecode(ListTestRecord, rec3)
      testEncodeDecode(ListTestRecord, rec4)
      testEncodeDecode(ListTestRecord, rec5)
      testEncodeDecode(ListTestRecord, rec6)

      val mrec0 = MongoListTestRecord.createRecord
      val mrec1 = MongoListTestRecord.createRecord.patternListField(List(Pattern.compile("^Mongo")))
      val mrec2 = MongoListTestRecord.createRecord.dateListField(List(new Date))
      val mrec3 = MongoListTestRecord.createRecord.uuidListField(List(UUID.randomUUID))

      testEncodeDecode(MongoListTestRecord, mrec0)
      testEncodeDecode(MongoListTestRecord, mrec1)
      testEncodeDecode(MongoListTestRecord, mrec2)
      testEncodeDecode(MongoListTestRecord, mrec3)

      val brec0 = BasicListTestRecord.createRecord
      val brec1 = BasicListTestRecord.createRecord.binaryListField(List(binData))
      val brec2 = BasicListTestRecord.createRecord.booleanListField(List(false))
      val brec3 = BasicListTestRecord.createRecord.decimalListField(List(BigDecimal(27.33)))
      val brec4 = BasicListTestRecord.createRecord.doubleListField(List(12.34))
      val brec5 = BasicListTestRecord.createRecord.longListField(List(876000L))
      val brec6 = BasicListTestRecord.createRecord.stringListListField(List(List("abc")))
      val brec7 = BasicListTestRecord.createRecord.stringMapListField(List(Map("key" -> "abc")))
      val brec8 = BasicListTestRecord.createRecord.bigIntListField(List(BigInt(2000L)))

      testEncodeDecode(BasicListTestRecord, brec0)
      testEncodeDecode(BasicListTestRecord, brec1)
      testEncodeDecode(BasicListTestRecord, brec2)
      testEncodeDecode(BasicListTestRecord, brec3)
      testEncodeDecode(BasicListTestRecord, brec4)
      testEncodeDecode(BasicListTestRecord, brec5)
      testEncodeDecode(BasicListTestRecord, brec6)
      testEncodeDecode(BasicListTestRecord, brec7)
      testEncodeDecode(BasicListTestRecord, brec8)

      val jrec0 = MongoJodaListTestRecord.createRecord
      val jrec1 = MongoJodaListTestRecord.createRecord.dateTimeListField(List(DateTime.now))

      testEncodeDecode(MongoJodaListTestRecord, jrec0)
      testEncodeDecode(MongoJodaListTestRecord, jrec1)
    }

    "support Map fields" in {
      val binData: Array[Byte] = Array(12, 19, 20)

      val sub0 = FieldTypeTestRecord.createRecord
      val sub1 = FieldTypeTestRecord.createRecord.mandatoryStringField("mrx")

      val rec0 = MapTest.createRecord
      val rec1 = MapTest.createRecord.mandatoryStringMapField(Map("a" -> "b"))
      val rec2 = MapTest.createRecord.mandatoryIntMapField(Map("one" -> 1, "twelve" -> 12))
      val rec3 = MapTest.createRecord.binaryMapField(Map("bin" -> binData))
      val rec4 = MapTest.createRecord.booleanMapField(Map("bool" -> false))
      val rec5 = MapTest.createRecord.dateMapField(Map("when" -> new Date))
      val rec6 = MapTest.createRecord.decimalMapField(Map("bigd" -> BigDecimal(1.23)))
      val rec7 = MapTest.createRecord.doubleMapField(Map("double" -> 1234.0))
      val rec8 = MapTest.createRecord.longMapField(Map("long" -> 1234L))
      val rec9 = MapTest.createRecord.patternMapField(Map("regex" -> Pattern.compile("^Mongo")))
      val rec10 = MapTest.createRecord.stringListMapField(Map("list" -> List("hello")))
      val rec11 = MapTest.createRecord.stringMapMapField(Map("map" -> Map("a" -> "hello")))
      val rec12 = MapTest.createRecord.uuidMapField(Map("id" -> UUID.randomUUID))

      testEncodeDecode(MapTest, rec0)
      testEncodeDecode(MapTest, rec1)
      testEncodeDecode(MapTest, rec2)
      testEncodeDecode(MapTest, rec3)
      testEncodeDecode(MapTest, rec4)
      testEncodeDecode(MapTest, rec5)
      testEncodeDecode(MapTest, rec6)
      testEncodeDecode(MapTest, rec7)
      testEncodeDecode(MapTest, rec8)
      testEncodeDecode(MapTest, rec9)
      testEncodeDecode(MapTest, rec10)
      testEncodeDecode(MapTest, rec11)
      testEncodeDecode(MapTest, rec12)

      val jrec0 = JodaTimeMapTest.createRecord
      val jrec1 = JodaTimeMapTest.createRecord.jodatimeMapField(Map("dt" -> DateTime.now))

      testEncodeDecode(JodaTimeMapTest, jrec0)
      testEncodeDecode(JodaTimeMapTest, jrec1)
    }

    "support Pattern fields" in {
      val rec0 = PatternTest.createRecord
      val rec1 = PatternTest.createRecord.patternfield(Pattern.compile("^Mo", Pattern.CASE_INSENSITIVE))

      testEncodeDecode(PatternTest, rec0)
      testEncodeDecode(PatternTest, rec1)
    }

    "support UUID fields" in {
      val rec0 = UUIDTest.createRecord
      val rec1 = UUIDTest.createRecord.uuidfield(UUID.randomUUID)

      testEncodeDecode(UUIDTest, rec0)
      testEncodeDecode(UUIDTest, rec1)
    }

    "support JObject fields" in {
      val joftrFieldJObject: JObject = ("minutes" -> 59)

      val rec0 = JObjectFieldTestRecord.createRecord
      val rec1 = JObjectFieldTestRecord.createRecord.mandatoryJObjectField(joftrFieldJObject)

      testEncodeDecode(JObjectFieldTestRecord, rec0)
      testEncodeDecode(JObjectFieldTestRecord, rec1)
    }
  }
}
