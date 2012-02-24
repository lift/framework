/*
 * Copyright 2010-2011 WorldWide Conferencing, LLC
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

import java.util.{Date, UUID}
import java.util.regex.Pattern

import org.bson.types.ObjectId
import org.specs.Specification

import common._
import http.js.JsExp
import json._
import JsonDSL._

import net.liftweb.record.field.Countries

import com.mongodb._

/**
 * Systems under specification for MongoRecord.
 */
object MongoRecordSpec extends Specification("MongoRecord Specification") with MongoTestKit {

  import fixtures._

  "MongoRecord field introspection" should {
    checkMongoIsRunning

    val rec = MongoFieldTypeTestRecord.createRecord
    val allExpectedFieldNames: List[String] = "_id" :: (for {
      typeName <- "Date JsonObject ObjectId Pattern UUID".split(" ")
      flavor <- "mandatory legacyOptional".split(" ")
    } yield flavor + typeName + "Field").toList

    "introspect only the expected fields" in {
      rec.fields().map(_.name).filterNot(allExpectedFieldNames.contains(_)) must_== Nil
    }

    "correctly look up fields by name" in {
      for (name <- allExpectedFieldNames) {
        rec.fieldByName(name) must verify(_.isDefined)
      }
    }

    "not look up fields by bogus names" in {
      for (name <- allExpectedFieldNames) {
        rec.fieldByName("x" + name + "y") must not(verify(_.isDefined))
      }
    }
  }

  "MongoRecord lifecycle callbacks" should {
    checkMongoIsRunning

    def testOneHarness(scope: String, f: LifecycleTestRecord => HarnessedLifecycleCallbacks): Unit = {
      ("be called before validation when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).beforeValidationHarness = () => triggered = true
        rec.foreachCallback(_.beforeValidation)
        triggered must_== true
      }

      ("be called after validation when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).afterValidationHarness = () => triggered = true
        rec.foreachCallback(_.afterValidation)
        triggered must_== true
      }

      ("be called around validate when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggeredBefore = false
        var triggeredAfter = false
        f(rec).beforeValidationHarness = () => triggeredBefore = true
        f(rec).afterValidationHarness = () => triggeredAfter = true
        rec.validate must_== Nil
        triggeredBefore must_== true
        triggeredAfter must_== true
      }

      ("be called before save when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).beforeSaveHarness = () => triggered = true
        rec.foreachCallback(_.beforeSave)
        triggered must_== true
      }

      ("be called before create when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).beforeCreateHarness = () => triggered = true
        rec.foreachCallback(_.beforeCreate)
        triggered must_== true
      }

      ("be called before update when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).beforeUpdateHarness = () => triggered = true
        rec.foreachCallback(_.beforeUpdate)
        triggered must_== true
      }

      ("be called after save when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).afterSaveHarness = () => triggered = true
        rec.foreachCallback(_.afterSave)
        triggered must_== true
      }

      ("be called after create when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).afterCreateHarness = () => triggered = true
        rec.foreachCallback(_.afterCreate)
        triggered must_== true
      }

      ("be called after update when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).afterUpdateHarness = () => triggered = true
        rec.foreachCallback(_.afterUpdate)
        triggered must_== true
      }

      ("be called before delete when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).beforeDeleteHarness = () => triggered = true
        rec.foreachCallback(_.beforeDelete)
        triggered must_== true
      }

      ("be called after delete when specified at " + scope) in {
        val rec = LifecycleTestRecord.createRecord
        var triggered = false
        f(rec).afterDeleteHarness = () => triggered = true
        rec.foreachCallback(_.afterDelete)
        triggered must_== true
      }
    }

    testOneHarness("the field level", rec => rec.stringFieldWithCallbacks: HarnessedLifecycleCallbacks)
  }

  "MongoRecord" should {
    checkMongoIsRunning

    val binData: Array[Byte] = Array(18, 19, 20)

    val fttr = FieldTypeTestRecord.createRecord
      .mandatoryBooleanField(false)
      .mandatoryCountryField(Countries.USA)
      .mandatoryDecimalField(BigDecimal("3.14"))
      .mandatoryDoubleField(1999)
      .mandatoryEmailField("test@liftweb.net")
      .mandatoryEnumField(MyTestEnum.ONE)
      .mandatoryIntField(99)
      .mandatoryLocaleField("en_US")
      .mandatoryLongField(100L)
      .mandatoryPostalCodeField("55401")
      .mandatoryStringField("string")
      .mandatoryTextareaField("string")
      .mandatoryTimeZoneField("America/Chicago")

    val bftr = BinaryFieldTestRecord.createRecord
      .mandatoryBinaryField(binData)

    val mfttr = MongoFieldTypeTestRecord.createRecord
      .mandatoryDateField(new Date)
      .mandatoryJsonObjectField(TypeTestJsonObject(1, "jsonobj1", Map("x" -> "1")))
      .mandatoryObjectIdField(ObjectId.get)
      .mandatoryPatternField(Pattern.compile("^Mo", Pattern.CASE_INSENSITIVE))
      .mandatoryUUIDField(UUID.randomUUID)

    val mfttrJson =
      ("_id" -> ("$oid" -> mfttr.id.toString)) ~
      ("mandatoryDateField" -> ("$dt" -> mfttr.meta.formats.dateFormat.format(mfttr.mandatoryDateField.value))) ~
      ("legacyOptionalDateField" -> (None: Option[JObject])) ~
      ("mandatoryJsonObjectField" -> (("intField" -> 1) ~ ("stringField" -> "jsonobj1") ~ ("mapField" -> ("x" -> "1")))) ~
      ("legacyOptionalJsonObjectField" -> (("intField" -> 0) ~ ("stringField" -> "") ~ ("mapField" -> JObject(Nil)))) ~
      ("mandatoryObjectIdField", ("$oid" -> mfttr.mandatoryObjectIdField.value.toString)) ~
      ("legacyOptionalObjectIdField" -> (None: Option[JObject])) ~
      ("mandatoryPatternField" -> (("$regex" -> mfttr.mandatoryPatternField.value.pattern) ~ ("$flags" -> mfttr.mandatoryPatternField.value.flags))) ~
      ("legacyOptionalPatternField" -> (None: Option[JObject])) ~
      ("mandatoryUUIDField" -> ("$uuid" -> mfttr.mandatoryUUIDField.value.toString)) ~
      ("legacyOptionalUUIDField" -> (None: Option[JObject]))

    val ltr = ListTestRecord.createRecord
      .mandatoryStringListField(List("abc", "def", "ghi"))
      .mandatoryIntListField(List(4, 5, 6))
      .mandatoryMongoJsonObjectListField(List(TypeTestJsonObject(1, "jsonobj1", Map("x" -> "1")), TypeTestJsonObject(2, "jsonobj2", Map("x" -> "2"))))
      .mongoCaseClassListField(List(MongoCaseClassTestObject(1,"str")))

    val ltrJson =
      ("_id" -> ("$uuid" -> ltr.id.toString)) ~
      ("mandatoryStringListField" -> List("abc", "def", "ghi")) ~
      ("legacyOptionalStringListField" -> List[String]()) ~
      ("mandatoryIntListField" -> List(4, 5, 6)) ~
      ("legacyOptionalIntListField" -> List[Int]()) ~
      ("mandatoryMongoJsonObjectListField" -> List(
        (("intField" -> 1) ~ ("stringField" -> "jsonobj1") ~ ("mapField" -> ("x" -> "1"))),
        (("intField" -> 2) ~ ("stringField" -> "jsonobj2") ~ ("mapField" -> ("x" -> "2")))
      )) ~
      ("legacyOptionalMongoJsonObjectListField" -> List[JObject]()) ~
      ("mongoCaseClassListField" -> List(
        ("intField" -> 1) ~ ("stringField" -> "str")
      ))

    val mtr = MapTestRecord.createRecord
      .mandatoryStringMapField(Map("a" -> "abc", "b" -> "def", "c" -> "ghi"))
      .mandatoryIntMapField(Map("a" -> 4, "b" -> 5, "c" -> 6))

    val mtrJson =
      ("_id" -> mtr.id.toString) ~
      ("mandatoryStringMapField" -> (
        ("a" -> "abc") ~
        ("b" -> "def") ~
        ("c" -> "ghi")
      )) ~
      ("legacyOptionalStringMapField" -> JObject(Nil)) ~
      ("mandatoryIntMapField" -> (
        ("a" -> 4) ~
        ("b" -> 5) ~
        ("c" -> 6)
      )) ~
      ("legacyOptionalIntMapField" -> JObject(Nil))

    // SubRecord
    val ssr1 = SubSubRecord.createRecord.name("SubSubRecord1")
    val ssr2 = SubSubRecord.createRecord.name("SubSubRecord2")

    val sr1 = SubRecord.createRecord
      .name("SubRecord1")
      .subsub(ssr1)
      .subsublist(ssr1 :: ssr2 :: Nil)
      .slist("s1" :: "s2" :: Nil)
      .smap(Map("a" -> "s1", "b" -> "s2"))
      .pattern(Pattern.compile("^Mo", Pattern.CASE_INSENSITIVE))

    val sr2 = SubRecord.createRecord.name("SubRecord2")

    val srtr = SubRecordTestRecord.createRecord
      .mandatoryBsonRecordField(sr1)
      .mandatoryBsonRecordListField(List(sr1,sr2))

    val sr1Json =
      JObject(List(
        JField("name", JString("SubRecord1")),
        JField("subsub", JObject(List(
          JField("name", JString("SubSubRecord1"))
        ))),
        JField("subsublist", JArray(List(
          JObject(List(JField("name", JString("SubSubRecord1")))),
          JObject(List(JField("name", JString("SubSubRecord2"))))
        ))),
        JField("when", JObject(List(
          JField("$dt", JString(srtr.meta.formats.dateFormat.format(sr1.when.value)))
        ))),
        JField("slist", JArray(List(JString("s1"), JString("s2")))),
        JField("smap", JObject(List(
          JField("a", JString("s1")),
          JField("b", JString("s2"))
        ))),
        JField("oid", JObject(List(JField("$oid", JString(sr1.oid.value.toString))))),
        JField("pattern", JObject(List(
          JField("$regex", JString(sr1.pattern.value.pattern)),
          JField("$flags", JInt(sr1.pattern.value.flags))
        ))),
        JField("uuid", JObject(List(JField("$uuid", JString(sr1.uuid.value.toString)))))
      ))

    val sr2Json =
      JObject(List(
        JField("name", JString("SubRecord2")),
        JField("subsub", JObject(List(
          JField("name", JString(""))
        ))),
        JField("subsublist", JArray(List())),
        JField("when", JObject(List(
          JField("$dt", JString(srtr.meta.formats.dateFormat.format(sr2.when.value)))
        ))),
        JField("slist", JArray(List())),
        JField("smap", JObject(List())),
        JField("oid", JObject(List(JField("$oid", JString(sr2.oid.value.toString))))),
        JField("pattern", JObject(List(
          JField("$regex", JString(sr2.pattern.value.pattern)),
          JField("$flags", JInt(sr2.pattern.value.flags))
        ))),
        JField("uuid", JObject(List(JField("$uuid", JString(sr2.uuid.value.toString)))))
      ))

    val srtrJson = JObject(List(
      JField("_id", JObject(List(JField("$oid", JString(srtr.id.toString))))),
      JField("mandatoryBsonRecordField", sr1Json),
      JField("legacyOptionalBsonRecordField", JNothing),
      JField("mandatoryBsonRecordListField", JArray(List(
        sr1Json,
        sr2Json
      ))),
      JField("legacyOptionalBsonRecordListField", JArray(List()))
    ))

    "save and retrieve 'standard' type fields" in {
      checkMongoIsRunning

      fttr.save

      val fttrFromDb = FieldTypeTestRecord.find(fttr.id.value)
      fttrFromDb must notBeEmpty
      fttrFromDb foreach { tr =>
        tr mustEqual fttr
      }

      bftr.save

      val bftrFromDb = BinaryFieldTestRecord.find(bftr.id.value)
      bftrFromDb must notBeEmpty
      bftrFromDb foreach { tr =>
        tr mustEqual bftr
      }
    }

    "delete record properly" in {
      checkMongoIsRunning

      fttr.save
      FieldTypeTestRecord.find(fttr.id.value) must notBeEmpty
      fttr.delete_!
      FieldTypeTestRecord.find(fttr.id.value) must beEmpty
    }

    "save and retrieve Mongo type fields with set values" in {
      checkMongoIsRunning

      mfttr.save

      val mfttrFromDb = MongoFieldTypeTestRecord.find(mfttr.id.value)
      mfttrFromDb must notBeEmpty
      mfttrFromDb foreach { tr =>
        tr mustEqual mfttr
      }

      ltr.save

      val ltrFromDb = ListTestRecord.find(ltr.id.value)
      ltrFromDb must notBeEmpty
      ltrFromDb foreach { tr =>
        tr mustEqual ltr
      }

      mtr.save

      val mtrFromDb = MapTestRecord.find(mtr.id.value)
      mtrFromDb must notBeEmpty
      mtrFromDb foreach { tr =>
        tr mustEqual mtr
      }

      srtr.save

      val srtrFromDb = SubRecordTestRecord.find(srtr.id.value)
      srtrFromDb must notBeEmpty
      srtrFromDb foreach { tr =>
        tr mustEqual srtr
      }
    }

    "save and retrieve Mongo type fields with default values" in {
      checkMongoIsRunning

      val mfttrDef = MongoFieldTypeTestRecord.createRecord
      mfttrDef.save

      val mfttrFromDb = MongoFieldTypeTestRecord.find(mfttrDef.id.value)
      mfttrFromDb must notBeEmpty
      mfttrFromDb foreach { tr =>
        tr mustEqual mfttrDef
      }

      val ltrDef = ListTestRecord.createRecord
      ltrDef.save

      val ltrFromDb = ListTestRecord.find(ltrDef.id.value)
      ltrFromDb must notBeEmpty
      ltrFromDb foreach { tr =>
        tr mustEqual ltrDef
      }

      val mtrDef = MapTestRecord.createRecord
      mtrDef.save

      val mtrFromDb = MapTestRecord.find(mtrDef.id.value)
      mtrFromDb must notBeEmpty
      mtrFromDb foreach { tr =>
        tr mustEqual mtrDef
      }

      val srtrDef = SubRecordTestRecord.createRecord
      srtrDef.save

      val srtrFromDb = SubRecordTestRecord.find(srtrDef.id.value)
      srtrFromDb must notBeEmpty
      srtrFromDb foreach { tr =>
        tr mustEqual srtrDef
      }
    }

    "convert Mongo type fields to JValue" in {
      checkMongoIsRunning

      mfttr.asJValue mustEqual mfttrJson

      ltr.asJValue mustEqual ltrJson

      mtr.asJValue mustEqual mtrJson

      val srtrAsJValue = srtr.asJValue
      srtrAsJValue \\ "_id" mustEqual srtrJson \\ "_id"
      srtrAsJValue \\ "mandatoryBsonRecordField" mustEqual srtrJson \\ "mandatoryBsonRecordField"
      srtrAsJValue \\ "legacyOptionalBsonRecordField" mustEqual srtrJson \\ "legacyOptionalBsonRecordField"
      srtrAsJValue \\ "mandatoryBsonRecordListField" mustEqual srtrJson \\ "mandatoryBsonRecordListField"
      srtrAsJValue \\ "legacyOptionalBsonRecordListField" mustEqual srtrJson \\ "legacyOptionalBsonRecordListField"
    }

    "get set from json string using lift-json parser" in {
      checkMongoIsRunning

      val mfftrFromJson = MongoFieldTypeTestRecord.fromJsonString(compact(render(mfttrJson)))
      mfftrFromJson must notBeEmpty
      mfftrFromJson foreach { tr =>
        tr mustEqual mfttr
      }

      val ltrFromJson = ListTestRecord.fromJsonString(compact(render(ltrJson)))
      ltrFromJson must notBeEmpty
      ltrFromJson foreach { tr =>
        tr mustEqual ltr
      }

      val mtrFromJson = MapTestRecord.fromJsonString(compact(render(mtrJson)))
      mtrFromJson must notBeEmpty
      mtrFromJson foreach { tr =>
        tr mustEqual mtr
      }
    }

    "handle null" in {
      checkMongoIsRunning

      val ntr = NullTestRecord.createRecord
      ntr.nullstring.set(null)
      ntr.jsonobjlist.set(List(JsonObj("1", null), JsonObj("2", "jsonobj2")))

      ntr.save must_== ntr

      val ntrFromDb = NullTestRecord.find(ntr.id.value)

      ntrFromDb must notBeEmpty

      ntrFromDb foreach { n =>
        // goes in as
        ntr.nullstring.valueBox.map(_ must beNull)
        ntr.nullstring.value must beNull
        // comes out as
        n.nullstring.valueBox.map(_ must_== "")
        n.nullstring.value must_== ""
        // JsonObjects
        n.jsonobjlist.value.size must_== 2
        ntr.jsonobjlist.value.size must_== 2
        n.jsonobjlist.value(0).id must_== ntr.jsonobjlist.value(0).id
        n.jsonobjlist.value(0).name must beNull
        ntr.jsonobjlist.value(0).name must beNull
        n.jsonobjlist.value(1).id must_== ntr.jsonobjlist.value(1).id
        n.jsonobjlist.value(1).name must_== ntr.jsonobjlist.value(1).name
      }
    }

    "handle Box using JsonBoxSerializer" in {
      checkMongoIsRunning

      val btr = BoxTestRecord.createRecord
      btr.jsonobjlist.set(
        BoxTestJsonObj("1", Empty, Full("Full String1"), Failure("Failure1")) ::
        BoxTestJsonObj("2", Empty, Full("Full String2"), Failure("Failure2")) ::
        Nil
      )

      btr.save

      val btrFromDb = BoxTestRecord.find(btr.id.value)

      btrFromDb must notBeEmpty

      btrFromDb foreach { b =>
        b.jsonobjlist.value.size must_== 2
        btr.jsonobjlist.value.size must_== 2
        val sortedList = b.jsonobjlist.value.sortWith(_.id < _.id)
        sortedList(0).boxEmpty must_== Empty
        sortedList(0).boxFull must_== Full("Full String1")
        sortedList(0).boxFail must_== Failure("Failure1")
      }
    }

    "retrieve MongoRef objects properly" in {
      checkMongoIsRunning

      val ntr = NullTestRecord.createRecord
      val btr = BoxTestRecord.createRecord

      fttr.save
      ltr.save
      mtr.save
      ntr.save
      btr.save

      val rftr = RefFieldTestRecord.createRecord
        .mandatoryObjectIdRefField(fttr.id.is)
        .mandatoryUUIDRefField(ltr.id.is)
        .mandatoryStringRefField(mtr.id.is)
        .mandatoryIntRefField(ntr.id.is)
        .mandatoryLongRefField(btr.id.is)
        .mandatoryObjectIdRefListField(List(fttr.id.is))
        .mandatoryUUIDRefListField(List(ltr.id.is))
        .mandatoryStringRefListField(List(mtr.id.is))
        .mandatoryIntRefListField(List(ntr.id.is))
        .mandatoryLongRefListField(List(btr.id.is))

      // single objects
      rftr.mandatoryObjectIdRefField.obj mustEqual Full(fttr)
      rftr.mandatoryUUIDRefField.obj mustEqual Full(ltr)
      rftr.mandatoryStringRefField.obj mustEqual Full(mtr)
      rftr.mandatoryIntRefField.obj mustEqual Full(ntr)
      rftr.mandatoryLongRefField.obj mustEqual Full(btr)

      val fttr2 = FieldTypeTestRecord.createRecord.save

      rftr.mandatoryObjectIdRefField.cached_? mustEqual true
      rftr.mandatoryObjectIdRefField(fttr2.id.is)
      rftr.mandatoryObjectIdRefField.cached_? mustEqual false
      rftr.mandatoryObjectIdRefField.find mustEqual Full(fttr2)
      rftr.mandatoryObjectIdRefField.obj mustEqual Full(fttr2)
      rftr.mandatoryObjectIdRefField.cached_? mustEqual true

      // lists
      rftr.mandatoryObjectIdRefListField.objs mustEqual List(fttr)
      rftr.mandatoryUUIDRefListField.objs mustEqual List(ltr)
      rftr.mandatoryStringRefListField.objs mustEqual List(mtr)
      rftr.mandatoryIntRefListField.objs mustEqual List(ntr)
      rftr.mandatoryLongRefListField.objs mustEqual List(btr)

      val fttr3 = FieldTypeTestRecord.createRecord.save
      val objList = List(fttr2, fttr3)

      rftr.mandatoryObjectIdRefListField.cached_? mustEqual true
      rftr.mandatoryObjectIdRefListField(objList.map(_.id.is))
      rftr.mandatoryObjectIdRefListField.cached_? mustEqual false
      rftr.mandatoryObjectIdRefListField.findAll mustEqual objList
      rftr.mandatoryObjectIdRefListField.objs mustEqual objList
      rftr.mandatoryObjectIdRefListField.cached_? mustEqual true
    }

    "use defaultValue when field is not present in the database" in {
      checkMongoIsRunning

      val missingFieldDocId = ObjectId.get

      // create a dbobject with no fields manually
      val builder = BasicDBObjectBuilder.start
        .add("_id", missingFieldDocId)

      FieldTypeTestRecord.useColl { coll => coll.save(builder.get) }

      val recFromDb = FieldTypeTestRecord.find(missingFieldDocId)

      recFromDb must notBeEmpty

      recFromDb foreach { r =>
        r.mandatoryBooleanField.is must_== false
        r.legacyOptionalBooleanField
        r.optionalBooleanField.is must beEmpty
        r.mandatoryCountryField.is must_== Countries.C1
        r.legacyOptionalCountryField.valueBox must beEmpty
        r.optionalCountryField.is must beEmpty
        r.mandatoryDecimalField.is must_== 0.00
        r.legacyOptionalDecimalField.valueBox must beEmpty
        r.optionalDecimalField.is must beEmpty
        r.mandatoryDoubleField.is must_== 0d
        r.legacyOptionalDoubleField.valueBox must beEmpty
        r.optionalDoubleField.is must beEmpty
        r.mandatoryEmailField.is must_== ""
        r.legacyOptionalEmailField.valueBox must beEmpty
        r.optionalEmailField.is must beEmpty
        r.mandatoryEnumField.is must_== MyTestEnum.ONE
        r.legacyOptionalEnumField.valueBox must beEmpty
        r.optionalEnumField.is must beEmpty
        r.mandatoryIntField.is must_== 0
        r.legacyOptionalIntField.valueBox must beEmpty
        r.optionalIntField.is must beEmpty
        r.mandatoryLocaleField.is must_== "en_US"
        r.legacyOptionalLocaleField.valueBox must beEmpty
        r.optionalLocaleField.is must beEmpty
        r.mandatoryLongField.is must_== 0L
        r.legacyOptionalLongField.valueBox must beEmpty
        r.optionalLongField.is must beEmpty
        r.mandatoryPostalCodeField.is must_== ""
        r.legacyOptionalPostalCodeField.valueBox must beEmpty
        r.optionalPostalCodeField.is must beEmpty
        r.mandatoryStringField.is must_== ""
        r.legacyOptionalStringField.valueBox must beEmpty
        r.optionalStringField.is must beEmpty
        r.mandatoryTextareaField.is must_== ""
        r.legacyOptionalTextareaField.valueBox must beEmpty
        r.optionalTextareaField.is must beEmpty
        // r.mandatoryTimeZoneField.is must_== "America/Chicago"
        r.legacyOptionalTimeZoneField.valueBox must beEmpty
        r.optionalTimeZoneField.is must beEmpty
      }
    }

    "reset dirty flags on save" in {
      checkMongoIsRunning

      val fttr = FieldTypeTestRecord.createRecord.save
      fttr.mandatoryDecimalField(BigDecimal("3.14"))
      fttr.dirtyFields.length must_== 1
      fttr.save
      fttr.dirtyFields.length must_== 0
    }

    "update dirty fields for a FieldTypeTestRecord" in {
      checkMongoIsRunning

      val fttr = FieldTypeTestRecord.createRecord
        .legacyOptionalStringField("legacy optional string")
        .optionalStringField("optional string")
        .save

      fttr.mandatoryBooleanField(false)
      fttr.mandatoryDecimalField(BigDecimal("3.14"))
      fttr.mandatoryDoubleField(1999)
      fttr.mandatoryEnumField(MyTestEnum.ONE)
      fttr.mandatoryIntField(99)
      fttr.mandatoryLongField(100L)
      fttr.mandatoryStringField("string")
      fttr.optionalStringField(Empty)
      fttr.legacyOptionalStringField(Empty)

      fttr.dirtyFields.length must_== 9
      fttr.update
      fttr.dirtyFields.length must_== 0

      val fromDb = FieldTypeTestRecord.find(fttr.id.is)
      fromDb must notBeEmpty
      fromDb foreach { rec =>
        rec must_== fttr
        rec.dirtyFields.length must_== 0
      }

      val fttr2 = FieldTypeTestRecord.createRecord.save

      fttr2.legacyOptionalStringField("legacy optional string")
      fttr2.optionalStringField("optional string")

      fttr2.dirtyFields.length must_== 2
      fttr2.update
      fttr2.dirtyFields.length must_== 0

      val fromDb2 = FieldTypeTestRecord.find(fttr2.id.is)
      fromDb2 must notBeEmpty
      fromDb2 foreach { rec =>
        rec must_== fttr2
        rec.dirtyFields.length must_== 0
      }
    }

    "update dirty fields for a MongoFieldTypeTestRecord" in {
      checkMongoIsRunning

      val mfttr = MongoFieldTypeTestRecord.createRecord
        .legacyOptionalDateField(new Date)
        .legacyOptionalObjectIdField(ObjectId.get)
        .save

      mfttr.mandatoryDateField(new Date)
      mfttr.mandatoryJsonObjectField(TypeTestJsonObject(1, "jsonobj1", Map("x" -> "1")))
      mfttr.mandatoryObjectIdField(ObjectId.get)
      mfttr.mandatoryPatternField(Pattern.compile("^Mon", Pattern.CASE_INSENSITIVE))
      mfttr.mandatoryUUIDField(UUID.randomUUID)
      mfttr.legacyOptionalDateField(Empty)
      mfttr.legacyOptionalObjectIdField(Empty)

      mfttr.dirtyFields.length must_== 7
      mfttr.update
      mfttr.dirtyFields.length must_== 0

      val fromDb = MongoFieldTypeTestRecord.find(mfttr.id.is)
      fromDb must notBeEmpty
      fromDb foreach { rec =>
        rec must_== mfttr
        rec.dirtyFields.length must_== 0
      }

      val mfttr2 = MongoFieldTypeTestRecord.createRecord.save

      mfttr2.legacyOptionalDateField(new Date)
      mfttr2.legacyOptionalObjectIdField(ObjectId.get)

      mfttr2.dirtyFields.length must_== 2
      mfttr2.update
      mfttr2.dirtyFields.length must_== 0

      val fromDb2 = MongoFieldTypeTestRecord.find(mfttr2.id.is)
      fromDb2 must notBeEmpty
      fromDb2 foreach { rec =>
        rec must_== mfttr2
        rec.dirtyFields.length must_== 0
      }
    }

    "update dirty fields for a ListTestRecord" in {
      checkMongoIsRunning

      val ltr = ListTestRecord.createRecord.save

      ltr.mandatoryStringListField(List("abc", "def", "ghi"))
      ltr.mandatoryIntListField(List(4, 5, 6))
      ltr.mandatoryMongoJsonObjectListField(List(TypeTestJsonObject(1, "jsonobj1", Map("x" -> "1")), TypeTestJsonObject(2, "jsonobj2", Map("x" -> "2"))))
      ltr.mongoCaseClassListField(List(MongoCaseClassTestObject(1,"str")))

      ltr.dirtyFields.length must_== 4
      ltr.update
      ltr.dirtyFields.length must_== 0

      val fromDb = ListTestRecord.find(ltr.id.is)
      fromDb must notBeEmpty
      fromDb foreach { rec =>
        rec must_== ltr
        rec.dirtyFields.length must_== 0
      }
    }

    "update dirty fields for a MapTestRecord" in {
      checkMongoIsRunning

      val mtr = MapTestRecord.save

      mtr.mandatoryStringMapField(Map("a" -> "abc", "b" -> "def", "c" -> "ghi"))
      mtr.mandatoryIntMapField(Map("a" -> 4, "b" -> 5, "c" -> 6))

      mtr.dirtyFields.length must_== 2
      mtr.update
      mtr.dirtyFields.length must_== 0

      val fromDb = MapTestRecord.find(mtr.id.is)
      fromDb must notBeEmpty
      fromDb foreach { rec =>
        rec must_== mtr
        rec.dirtyFields.length must_== 0
      }
    }

    "update dirty fields for a SubRecordTestRecord" in {
      checkMongoIsRunning

      val srtr = SubRecordTestRecord.createRecord.save

      val ssr1 = SubSubRecord.createRecord.name("SubSubRecord1")
      val ssr2 = SubSubRecord.createRecord.name("SubSubRecord2")

      val sr1 = SubRecord.createRecord
        .name("SubRecord1")
        .subsub(ssr1)
        .subsublist(ssr1 :: ssr2 :: Nil)
        .slist("s1" :: "s2" :: Nil)
        .smap(Map("a" -> "s1", "b" -> "s2"))
        .pattern(Pattern.compile("^Mon", Pattern.CASE_INSENSITIVE))

      val sr2 = SubRecord.createRecord.name("SubRecord2")

      srtr.mandatoryBsonRecordField(sr1)
      srtr.mandatoryBsonRecordListField(List(sr1,sr2))

      srtr.dirtyFields.length must_== 2
      srtr.update
      srtr.dirtyFields.length must_== 0

      val fromDb = SubRecordTestRecord.find(srtr.id.is)
      fromDb must notBeEmpty
      fromDb foreach { rec =>
        rec must_== srtr
        rec.dirtyFields.length must_== 0
      }
    }
  }
}

