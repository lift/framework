/*
 * Copyright 2006-2014 WorldWide Conferencing, LLC
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

import java.util.{Calendar, Date, UUID}
import java.util.regex.Pattern

import org.bson.types.ObjectId
import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.execute.AsResult

import org.joda.time.DateTime

import common._
import json._
import mongodb.BsonDSL._
import util.Helpers.randomString
import http.{LiftSession, S}
import http.js.JE._
import http.js.JsExp
import net.liftweb.record._
import common.Box._
import xml.{Elem, NodeSeq, Text}
import util.{Helpers, FieldError}
import Helpers._

import org.bson.types.ObjectId

/**
 * Systems under specification for MongoField.
 */
object MongoFieldSpec extends Specification with MongoTestKit with AroundExample {
  "MongoField Specification".title
  sequential

  import fixtures._

  lazy val session = new LiftSession("", randomString(20), Empty)

  // One of these is for specs2 2.x, the other for specs2 1.x
  protected def around[T : AsResult](t: =>T) = S.initIfUninitted(session) { AsResult(t) }
  protected def around[T <% org.specs2.execute.Result](t: =>T) = S.initIfUninitted(session) { t }

  def passBasicTests[A](
    example: A,
    example2: A,
    mandatory: MandatoryTypedField[A],
    legacyOptionalBox: Box[MandatoryTypedField[A]],
    canCheckDefaultValues: Boolean = true
  )(implicit m: scala.reflect.Manifest[A]): Unit = {

    def commonBehaviorsForAllFlavors(field: MandatoryTypedField[A]) = {

      "which have the correct initial value" in {
        field.value must be_==(field.defaultValue).when(canCheckDefaultValues)
        field.valueBox must be_==(field.defaultValueBox).when(canCheckDefaultValues)
      }

      "which are readable and writable" in {
        field.set(example)
        field.value must_== example
        field.valueBox must_== Full(example)
        field.clear
        field.value must_!= example
        field.valueBox must_!= Full(example)
        field.setBox(Full(example))
        field.value must_== example
        field.valueBox must_== Full(example)
      }

      "which correctly clear back to the default" in {
        { field.clear; field.valueBox } must be_==(field.defaultValueBox).when(canCheckDefaultValues)
      }

      "which capture error conditions set in" in {
        // FIXME: This needs to be rearranged just so that it doesn't foul with subsequent examples
        // field.setBox(Failure("my failure"))
        // Failure("my failure") must_== Failure("my failure")
        pending
      }

      "which are only flagged as dirty_? when setBox is called with a different value" in {
        field.clear
        field match {
          case owned: OwnedField[_] => owned.owner.runSafe {
            field.resetDirty
          }
          case _ => field.resetDirty
        }
        field.dirty_? must_== false
        val valueBox = field.valueBox
        field.setBox(valueBox)
        field.dirty_? must_== false
        val exampleBox = Full(example)
        (valueBox === exampleBox) must_== false
        field.setBox(exampleBox)
        field.dirty_? must_== true
        val exampleBox2 = Full(example2)
        (exampleBox === exampleBox2) must_== false
        field.setBox(exampleBox2)
        field.dirty_? must_== true
        field.setBox(valueBox)
        success
      }
    }

    "support mandatory fields" in {
      "which are configured correctly" in {
        mandatory.optional_? must_== false
      }

      "which initialize to some value" in {
        mandatory.valueBox.isDefined must_== true
      }

      "common behaviors for all flavors" in {
        commonBehaviorsForAllFlavors(mandatory)
      }

      "which correctly fail to be set to Empty" in {
        mandatory.valueBox.isDefined must_== true
        mandatory.setBox(Empty)
        mandatory.valueBox must beLike { case Failure(s, _, _) => s must_== mandatory.notOptionalErrorMessage }
      }
    }

    legacyOptionalBox map { legacyOptional =>
      "support 'legacy' optional fields (override optional_?)" in {
        "which are configured correctly" in {
          legacyOptional.optional_? must_== true
        }

        "which initialize to Empty" in {
          legacyOptional.valueBox must_== Empty
        }

        "common behaviors for all flavors" in {
          commonBehaviorsForAllFlavors(legacyOptional)
        }

        "which do not fail when set to Empty" in {
          legacyOptional.set(example)
          legacyOptional.value must_== example
          legacyOptional.valueBox must_== Full(example)
          legacyOptional.clear
          if (canCheckDefaultValues) {
            legacyOptional.value must_== legacyOptional.defaultValue
            legacyOptional.valueBox must_== legacyOptional.defaultValueBox
          }
          legacyOptional.set(example)
          legacyOptional.value must_== example
          legacyOptional.valueBox must_== Full(example)
          legacyOptional.setBox(Empty)
          if (canCheckDefaultValues) {
            legacyOptional.value must_== legacyOptional.defaultValue
            legacyOptional.valueBox must_== legacyOptional.defaultValueBox
          }
          success
        }
      }
    }
  }

  def passConversionTests[A](example: A, mandatory: MandatoryTypedField[A], jsexp: JsExp, jvalue: JValue, formPattern: Box[NodeSeq], canCheckSetFromJValue: Boolean = true): Fragment = {

    /*
    "convert to JsExp" in {
      mandatory.set(example)
      mandatory.asJs mustEqual jsexp
    }*/

    "convert to JValue" in {
      mandatory.set(example)
      mandatory.asJValue mustEqual jvalue
    }

    if (canCheckSetFromJValue) {
      "get set from JValue" in {
        mandatory.setFromJValue(jvalue) mustEqual Full(example)
        mandatory.value mustEqual example
      }
    }

    "convert to form XML" in {
      formPattern foreach { fp =>
        mandatory.set(example)
        val session = new LiftSession("", randomString(20), Empty)
        S.initIfUninitted(session) {
          val formXml = mandatory.toForm
          formXml.isDefined must_== true
          formXml foreach { fprime =>
            val f = ("* [name]" #> ".*" & "select *" #> (((ns: NodeSeq) => ns.filter {
              case e: Elem => e.attribute("selected").map(_.text) == Some("selected")
              case _ => false
            }) andThen "* [value]" #> ".*"))(fprime)
            val ret: Boolean = Helpers.compareXml(f, fp)

            ret must_== true
          }
        }
      }
      success
    }
  }

  "DateField" should {
    val rec = MongoFieldTypeTestRecord.createRecord
    val now = new Date
    val nowStr = rec.meta.formats.dateFormat.format(now)
    val now2 = Calendar.getInstance()
      now2.add(Calendar.DATE, 1)
    passBasicTests(now, now2.getTime, rec.mandatoryDateField, Full(rec.legacyOptionalDateField), false)
    passConversionTests(
      now,
      rec.mandatoryDateField,
      JsObj(("$dt", Str(nowStr))),
      JObject(List(JField("$dt", JString(nowStr)))),
      Full(<input name=".*" type="text" tabindex="1" value={nowStr} id="mandatoryDateField_id"></input>)
    )
  }

  "JsonObjectField" should {
    val rec = MongoFieldTypeTestRecord.createRecord
    val ttjo = TypeTestJsonObject(1, "jsonobj1", Map("x" -> "a"))
    val ttjo2 = TypeTestJsonObject(2, "jsonobj2", Map("x" -> "b"))
    val json = ("intField" -> 1) ~ ("stringField" -> "jsonobj1") ~ ("mapField" -> (("x" -> "a")))
    passBasicTests(ttjo, ttjo2, rec.mandatoryJsonObjectField, Full(rec.legacyOptionalJsonObjectField))
    passConversionTests(
      ttjo,
      rec.mandatoryJsonObjectField,
      new JsExp {
        def toJsCmd = Printer.compact(render(json))
      },
      json,
      Empty
    )
  }

  "ObjectIdField" should {
    // The extra `in` here is required for compilation, or we get a strange ambiguous overload warning.
    "work and provide the appropriate date" in {
      val rec = MongoFieldTypeTestRecord.createRecord
      val oid = ObjectId.get
      val oid2 = ObjectId.get
      passBasicTests(oid, oid2, rec.mandatoryObjectIdField, Full(rec.legacyOptionalObjectIdField), false)
      passConversionTests(
        oid,
        rec.mandatoryObjectIdField,
        JsObj(("$oid", oid.toString)),
        JObject(List(JField("$oid", JString(oid.toString)))),
        Full(<input name=".*" type="text" tabindex="1" value={oid.toString} id="mandatoryObjectIdField_id"></input>)
      )
      rec.mandatoryObjectIdField(oid)

      oid.getDate must_== rec.mandatoryObjectIdField.createdAt
    }
  }

  "PatternField" should {
    val rec = PatternFieldTestRecord.createRecord
    val ptrn = Pattern.compile("^Mo", Pattern.CASE_INSENSITIVE)
    val ptrn2 = Pattern.compile("^MON", Pattern.CASE_INSENSITIVE)
    passBasicTests(ptrn, ptrn2, rec.mandatoryPatternField, Full(rec.legacyOptionalPatternField), false)
    passConversionTests(
      ptrn,
      rec.mandatoryPatternField,
      JsObj(("$regex", Str(ptrn.toString)), ("$flags", Num(2))),
      JObject(List(JField("$regex", JString(ptrn.toString)), JField("$flags", JInt(2)))),
      Empty,
      false
    )
  }

  "UUIDField" should {
    val rec = MongoFieldTypeTestRecord.createRecord
    val uuid = UUID.randomUUID
    val uuid2 = UUID.randomUUID
    passBasicTests(uuid, uuid2, rec.mandatoryUUIDField, Full(rec.legacyOptionalUUIDField), false)
    passConversionTests(
      uuid,
      rec.mandatoryUUIDField,
      JsObj(("$uuid", Str(uuid.toString))),
      JObject(List(JField("$uuid", JString(uuid.toString)))),
      Full(<input name=".*" type="text" tabindex="1" value={uuid.toString} id="mandatoryUUIDField_id"></input>)
    )
  }

  "PasswordField" should {
    "require a nonempty password" in {
      val rec = PasswordTestRecord.createRecord
      rec.password.setPassword("")
      rec.validate must_== (
        FieldError(rec.password, Text(S.?("password.must.be.set"))) ::
        Nil
      )
    }

    "require at least 3 character password" in {
      val rec = PasswordTestRecord.createRecord
      rec.password.setPassword("ab")
      rec.validate must_== (
        FieldError(rec.password, Text(S.?("password.too.short"))) ::
        Nil
      )
    }
  }

  "MongoListField (String)" should {
    "function correctly" in {
      val rec = ListTestRecord.createRecord
      val lst = List("abc", "def", "ghi")
      val lst2 = List("ab", "de", "gh")
      passBasicTests(lst, lst2, rec.mandatoryStringListField, Empty)
      passConversionTests(
        lst,
        rec.mandatoryStringListField,
        JsArray(Str("abc"), Str("def"), Str("ghi")),
        JArray(List(JString("abc"), JString("def"), JString("ghi"))),
        Empty
      )
    }
  }

  "MongoListField (Int)" should {
    "function correctly" in {
      val rec = ListTestRecord.createRecord
      val lst = List(4, 5, 6)
      val lst2 = List(1, 2, 3)
      passBasicTests(lst, lst2, rec.mandatoryIntListField, Empty)
      passConversionTests(
        lst,
        rec.mandatoryIntListField,
        JsArray(Num(4), Num(5), Num(6)),
        JArray(List(JInt(4), JInt(5), JInt(6))),
        Empty
      )
    }
  }

  "MongoListField (ObjectId)" should {
    "function correctly" in {
      val rec = MongoListTestRecord.createRecord
      val oid1 = ObjectId.get
      val oid2 = ObjectId.get
      val oid3 = ObjectId.get
      val oid4 = ObjectId.get
      val oid5 = ObjectId.get
      val oid6 = ObjectId.get
      val lst = List(oid1, oid2, oid3)
      val lst2 = List(oid4, oid5, oid6)
      passBasicTests(lst, lst2, rec.objectIdRefListField, Empty)
      passConversionTests(
        lst,
        rec.objectIdRefListField,
        JsArray(Str(oid1.toString), Str(oid2.toString), Str(oid3.toString)),
        JArray(List(
          JObject(List(JField("$oid", JString(oid1.toString)))),
          JObject(List(JField("$oid", JString(oid2.toString)))),
          JObject(List(JField("$oid", JString(oid3.toString))))
        )),
        Empty
      )
    }
  }

  "MongoListField (Pattern)" should {
    "function correctly" in {
      val rec = MongoListTestRecord.createRecord
      val ptrn1 = Pattern.compile("^Mo", Pattern.CASE_INSENSITIVE)
      val ptrn2 = Pattern.compile("^MON", Pattern.CASE_INSENSITIVE)
      val ptrn3 = Pattern.compile("^TUE")
      val ptrn4 = Pattern.compile("^WED")
      val lst1 = List(ptrn1, ptrn2)
      val lst2 = List(ptrn3, ptrn4)
      passBasicTests(lst1, lst2, rec.patternListField, Empty)
      passConversionTests(
        lst1,
        rec.patternListField,
        JsArray(Str(ptrn1.toString), Str(ptrn2.toString)),
        JArray(List(
          JsonRegex(ptrn1),
          JsonRegex(ptrn2)
        )),
        Empty,
        false
      )
    }
  }

  "MongoListField (Date)" should {
    "function correctly" in {
      val rec = MongoListTestRecord.createRecord
      val dt1 = new Date
      val dt2 = new Date
      val dt3 = new Date
      val dt4 = new Date
      val dt5 = new Date
      val dt6 = new Date
      val lst = List(dt1, dt2, dt3)
      val lst2 = List(dt4, dt5, dt6)
      passBasicTests(lst, lst2, rec.dateListField, Empty)
      passConversionTests(
        lst,
        rec.dateListField,
        JsArray(Str(dt1.toString), Str(dt2.toString), Str(dt3.toString)),
        JArray(List(
          JsonDate(dt1)(MongoListTestRecord.formats),
          JsonDate(dt2)(MongoListTestRecord.formats),
          JsonDate(dt3)(MongoListTestRecord.formats)
        )),
        Empty
      )
    }
  }

  "MongoListField (UUID)" should {
    "function correctly" in {
      val rec = MongoListTestRecord.createRecord
      val uuid1 = UUID.randomUUID
      val uuid2 = UUID.randomUUID
      val uuid3 = UUID.randomUUID
      val uuid4 = UUID.randomUUID
      val uuid5 = UUID.randomUUID
      val uuid6 = UUID.randomUUID
      val lst = List(uuid1, uuid2, uuid3)
      val lst2 = List(uuid4, uuid5, uuid6)
      passBasicTests(lst, lst2, rec.uuidListField, Empty)
      passConversionTests(
        lst,
        rec.uuidListField,
        JsArray(Str(uuid1.toString), Str(uuid2.toString), Str(uuid3.toString)),
        JArray(List(
          JsonUUID(uuid1),
          JsonUUID(uuid2),
          JsonUUID(uuid3)
        )),
        Empty
      )
    }
  }

  "MongoListField (DateTime)" should {
    "function correctly" in {
      val rec = MongoJodaListTestRecord.createRecord
      val dt1 = new DateTime
      val dt2 = new DateTime
      val dt3 = new DateTime
      val dt4 = new DateTime
      val dt5 = new DateTime
      val dt6 = new DateTime
      val lst = List(dt1, dt2, dt3)
      val lst2 = List(dt4, dt5, dt6)
      passBasicTests(lst, lst2, rec.dateTimeListField, Empty)
      passConversionTests(
        lst,
        rec.dateTimeListField,
        JsArray(Str(dt1.toString), Str(dt2.toString), Str(dt3.toString)),
        JArray(List(
          JsonDate(dt1.toDate)(MongoListTestRecord.formats),
          JsonDate(dt2.toDate)(MongoListTestRecord.formats),
          JsonDate(dt3.toDate)(MongoListTestRecord.formats)
        )),
        Empty
      )
    }
  }

  "MongoJsonObjectListField" should {
    "function correctly" in {
      val rec = ListTestRecord.createRecord
      val lst = List(TypeTestJsonObject(1, "jsonobj1", Map("x" -> "1")), TypeTestJsonObject(2, "jsonobj2", Map("x" -> "2")))
      val lst2 = List(TypeTestJsonObject(3, "jsonobj3", Map("x" -> "3")), TypeTestJsonObject(4, "jsonobj4", Map("x" -> "4")))
      val json = List(
        ("intField" -> 1) ~ ("stringField" -> "jsonobj1") ~ ("mapField" -> (("x" -> "1"))),
        ("intField" -> 2) ~ ("stringField" -> "jsonobj2") ~ ("mapField" -> (("x" -> "2")))
      )
      passBasicTests(lst, lst2, rec.mandatoryMongoJsonObjectListField, Empty)
      passConversionTests(
        lst,
        rec.mandatoryMongoJsonObjectListField,
        new JsExp {
          def toJsCmd = Printer.compact(render(json))
        },
        json,
        Empty
      )
    }
  }

  "MongoCaseClassListField" should {
    "setFromAny a List" in {
      val rec = ListTestRecord.createRecord
      val lst = List(MongoCaseClassTestObject(1,"str1", MyTestEnum.THREE))
      rec.mongoCaseClassListField.setFromAny(lst)
      rec.mongoCaseClassListField.value must_== lst
    }
  }

  "MongoMapField (String)" should {
    "function correctly" in {
      val rec = MapTestRecord.createRecord
      val map = Map("a" -> "abc", "b" -> "def", "c" -> "ghi")
      val map2 = Map("a" -> "ab", "b" -> "de", "c" -> "gh")
      passBasicTests(map, map2, rec.mandatoryStringMapField, Empty)
      passConversionTests(
        map,
        rec.mandatoryStringMapField,
        JsObj(("a", Str("abc")), ("b", Str("def")), ("c", Str("ghi"))),
        JObject(List(
          JField("a", JString("abc")),
          JField("b", JString("def")),
          JField("c", JString("ghi"))
        )),
        Empty
      )
    }
  }

  "MongoMapField (Int)" should {
    "function correctly" in {
      val rec = MapTestRecord.createRecord
      val map = Map("a" -> 4, "b" -> 5, "c" -> 6)
      val map2 = Map("a" -> 1, "b" -> 2, "c" -> 3)
      passBasicTests(map, map2, rec.mandatoryIntMapField, Empty)
      passConversionTests(
        map,
        rec.mandatoryIntMapField,
        JsObj(("a", Num(4)), ("b", Num(5)), ("c", Num(6))),
        JObject(List(
          JField("a", JInt(4)),
          JField("b", JInt(5)),
          JField("c", JInt(6))
        )),
        Empty
      )
    }
  }

  "BsonRecordField" should {
    "function correctly" in {
      val rec = SubRecordTestRecord.createRecord
      val subSubRec = SubSubRecord.createRecord.name("subsub")
      val subRec = SubRecord.createRecord.name("subrecord").subsub(subSubRec)
      val subRec2 = SubRecord.createRecord.name("subrecord2")

      val srJson =
        ("name" -> "subrecord") ~
        ("subsub" -> ("name" -> "subsub")) ~
        ("subsublist" -> JArray(Nil)) ~
        ("when" -> ("$dt" -> rec.meta.formats.dateFormat.format(subRec.when.value))) ~
        ("slist" -> JArray(Nil)) ~
        ("smap" -> JObject(Nil)) ~
        ("oid" -> ("$oid" -> subRec.oid.value.toString)) ~
        ("pattern" ->
            ("$regex" -> subRec.pattern.value.pattern) ~
            ("$flags" -> subRec.pattern.value.flags)
        ) ~
        ("uuid" -> ("$uuid" -> subRec.uuid.value.toString))

      val srJsExp = new JsExp {
        def toJsCmd = Printer.compact(render(srJson))
      }

      passBasicTests(subRec, subRec2, rec.mandatoryBsonRecordField, Full(rec.legacyOptionalBsonRecordField), false)
      passConversionTests(
        subRec,
        rec.mandatoryBsonRecordField,
        srJsExp,
        srJson,
        Empty
      )
    }
  }

  "BsonRecordListField" should {
    "function correctly" in {
      val rec = SubRecordTestRecord.createRecord
      val subSubRec = SubSubRecord.createRecord.name("subsub")
      val lst = List(SubRecord.createRecord.name("subrec1").subsub(subSubRec), SubRecord.createRecord.name("subrec2").subsub(subSubRec))
      val lst2 = List(SubRecord.createRecord.name("subrec3"), SubRecord.createRecord.name("subrec4"))
      val sr1Json =
        ("name" -> "subrec1") ~
        ("subsub" -> ("name" -> "subsub")) ~
        ("subsublist" -> JArray(Nil)) ~
        ("when" -> ("$dt" -> rec.meta.formats.dateFormat.format(lst(0).when.value))) ~
        ("slist" -> JArray(Nil)) ~
        ("smap" -> JObject(Nil)) ~
        ("oid" -> ("$oid" -> lst(0).oid.value.toString)) ~
        ("pattern" ->
            ("$regex" -> lst(0).pattern.value.pattern) ~
            ("$flags" -> lst(0).pattern.value.flags)
        ) ~
        ("uuid" -> ("$uuid" -> lst(0).uuid.value.toString))

      val sr2Json =
        ("name" -> "subrec2") ~
        ("subsub" -> ("name" -> "subsub")) ~
        ("subsublist" -> JArray(Nil)) ~
        ("when" -> ("$dt" -> rec.meta.formats.dateFormat.format(lst(1).when.value))) ~
        ("slist" -> JArray(Nil)) ~
        ("smap" -> JObject(Nil)) ~
        ("oid" -> ("$oid" -> lst(1).oid.value.toString)) ~
        ("pattern" ->
            ("$regex" -> lst(1).pattern.value.pattern) ~
            ("$flags" -> lst(1).pattern.value.flags)
        ) ~
        ("uuid" -> ("$uuid" -> lst(1).uuid.value.toString))

      val sr1JsExp = new JsExp {
        def toJsCmd = compact(render(sr1Json))
      }
      val sr2JsExp = new JsExp {
        def toJsCmd = compact(render(sr2Json))
      }

      passBasicTests(lst, lst2, rec.mandatoryBsonRecordListField, Full(rec.legacyOptionalBsonRecordListField))
      passConversionTests(
        lst,
        rec.mandatoryBsonRecordListField,
        JsArray(sr1JsExp, sr2JsExp),
        JArray(List(sr1Json, sr2Json)),
        Empty
      )
    }
  }

  "JObjectField" should {
    val jo: JValue = ("minutes" -> 59)
    val json: JObject = ("mandatoryJObjectField" -> jo)

    "convert to JValue" in {
      val rec = JObjectFieldTestRecord.createRecord
        .mandatoryJObjectField(json)

      rec.mandatoryJObjectField.asJValue must_== json

    }
    "get set from JValue" in {
      val rec = JObjectFieldTestRecord.createRecord
      val recFromJson = rec.mandatoryJObjectField.setFromJValue(json)

      recFromJson.isDefined must_== true
      recFromJson foreach { r =>
        r must_== json
      }
      success
    }
    "get set from JValue after BSON roundtrip" in {
      val joftrJson: JObject = ("_id" -> ("$oid" -> ObjectId.get.toString)) ~ ("mandatoryJObjectField" -> ("minutes" -> 59))
      val fromJsonBox = JObjectFieldTestRecord.fromJValue(joftrJson)

      fromJsonBox.isDefined must_== true

      fromJsonBox foreach { fromJson =>
        //Convert the test record, make a DBObject out of it, and make a record from that DBObject
        val fromBson = JObjectFieldTestRecord.fromDBObject(fromJson.asDBObject)
        fromBson.asJValue must_== fromJson.asJValue
      }
      success
    }
  }
}

