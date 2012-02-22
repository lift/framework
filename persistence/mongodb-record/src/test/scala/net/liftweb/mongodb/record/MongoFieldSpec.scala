/*
 * Copyright 2006-2011 WorldWide Conferencing, LLC
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
import json._
import BsonDSL._
import util.Helpers.randomString
import http.{LiftSession, S}
import http.js.JE._
import http.js.JsExp
import net.liftweb.record._
import common.Box._
import xml.{Elem, NodeSeq, Text}
import util.{Helpers, FieldError}
import Helpers._

/**
 * Systems under specification for MongoField.
 */
object MongoFieldSpec extends Specification("MongoField Specification") with MongoTestKit {
  import fixtures._

  def passBasicTests[A](
    example: A,
    mandatory: MandatoryTypedField[A],
    legacyOptional: MandatoryTypedField[A],
    canCheckDefaultValues: Boolean = true
  )(implicit m: scala.reflect.Manifest[A]): Unit = {

    def commonBehaviorsForAllFlavors(field: MandatoryTypedField[A]) = {

      "which have the correct initial value" in {
        field.value must beEqual(field.defaultValue).when(canCheckDefaultValues)
        field.valueBox must beEqual(field.defaultValueBox).when(canCheckDefaultValues)
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
        { field.clear; field.valueBox } must beEqual(field.defaultValueBox).when(canCheckDefaultValues)
      }

      "which capture error conditions set in" in {
        // FIXME: This needs to be rearranged just so that it doesn't foul with subsequent examples
        // field.setBox(Failure("my failure"))
        // Failure("my failure") must_== Failure("my failure")
      }
    }

    "support mandatory fields" in {
      setSequential()

      "which are configured correctly" in {
        mandatory.optional_? must_== false
      }

      "which initialize to some value" in {
        mandatory.valueBox must verify(_.isDefined)
      }

      "common behaviors for all flavors" in {
        commonBehaviorsForAllFlavors(mandatory)
      }

      "which correctly fail to be set to Empty" in {
        mandatory.valueBox must verify(_.isDefined)
        mandatory.setBox(Empty)
        mandatory.valueBox must beLike { case Failure(s, _, _) if s == mandatory.notOptionalErrorMessage => true }
      }
    }

    "support 'legacy' optional fields (override optional_?)" in {
      setSequential()

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
      }
    }
  }

  def passConversionTests[A](example: A, mandatory: MandatoryTypedField[A], jsexp: JsExp, jvalue: JValue, formPattern: Box[NodeSeq]): Unit = {

    /*
    "convert to JsExp" in {
      mandatory.set(example)
      mandatory.asJs mustEqual jsexp
    }*/

    "convert to JValue" in {
      mandatory.set(example)
      mandatory.asJValue mustEqual jvalue
    }

    if (!example.isInstanceOf[Pattern]) { // Patterns don't compare well
      "get set from JValue" in {
        mandatory.setFromJValue(jvalue) mustEqual Full(example)
        mandatory.value mustEqual example
        () // does not compile without this: no implicit argument matching parameter type scala.reflect.Manifest[org.specs.specification.Result[mandatory.MyType]]
      }
    }

    formPattern foreach { fp =>
      "convert to form XML" in {
        mandatory.set(example)
        val session = new LiftSession("", randomString(20), Empty)
        S.initIfUninitted(session) {
          val formXml = mandatory.toForm
          formXml must notBeEmpty
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
    }
  }

  "DateField" should {
    val rec = MongoFieldTypeTestRecord.createRecord
    val now = new Date
    val nowStr = rec.meta.formats.dateFormat.format(now)
    passBasicTests(now, rec.mandatoryDateField, rec.legacyOptionalDateField, false)
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
    val json = ("intField" -> 1) ~ ("stringField" -> "jsonobj1") ~ ("mapField" -> (("x" -> "a")))
    passBasicTests(ttjo, rec.mandatoryJsonObjectField, rec.legacyOptionalJsonObjectField)
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
    val rec = MongoFieldTypeTestRecord.createRecord
    val oid = ObjectId.get
    passBasicTests(oid, rec.mandatoryObjectIdField, rec.legacyOptionalObjectIdField, false)
    passConversionTests(
      oid,
      rec.mandatoryObjectIdField,
      JsObj(("$oid", oid.toString)),
      JObject(List(JField("$oid", JString(oid.toString)))),
      Full(<input name=".*" type="text" tabindex="1" value={oid.toString} id="mandatoryObjectIdField_id"></input>)
    )
  }

  "PatternField" should {
    val rec = MongoFieldTypeTestRecord.createRecord
    val ptrn = Pattern.compile("^Mo", Pattern.CASE_INSENSITIVE)
    passBasicTests(ptrn, rec.mandatoryPatternField, rec.legacyOptionalPatternField, false)
    passConversionTests(
      ptrn,
      rec.mandatoryPatternField,
      JsObj(("$regex", Str(ptrn.toString)), ("$flags", Num(2))),
      JObject(List(JField("$regex", JString(ptrn.toString)), JField("$flags", JInt(2)))),
      Empty
    )
  }

  "UUIDField" should {
    val rec = MongoFieldTypeTestRecord.createRecord
    val uuid = UUID.randomUUID
    passBasicTests(uuid, rec.mandatoryUUIDField, rec.legacyOptionalUUIDField, false)
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
        FieldError(rec.password, Text(S.??("password.must.be.set"))) ::
        Nil
      )
    }

    "require at least 3 character password" in {
      val rec = PasswordTestRecord.createRecord
      rec.password.setPassword("ab")
      rec.validate must_== (
        FieldError(rec.password, Text(S.??("password.too.short"))) ::
        Nil
      )
    }
  }

  "MongoListField (String)" should {
    "function correctly" in {
      val rec = ListTestRecord.createRecord
      val lst = List("abc", "def", "ghi")
      passBasicTests(lst, rec.mandatoryStringListField, rec.legacyOptionalStringListField)
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
      passBasicTests(lst, rec.mandatoryIntListField, rec.legacyOptionalIntListField)
      passConversionTests(
        lst,
        rec.mandatoryIntListField,
        JsArray(Num(4), Num(5), Num(6)),
        JArray(List(JInt(4), JInt(5), JInt(6))),
        Empty
      )
    }
  }

  "MongoJsonObjectListField" should {
    "function correctly" in {
      val rec = ListTestRecord.createRecord
      val lst = List(TypeTestJsonObject(1, "jsonobj1", Map("x" -> "1")), TypeTestJsonObject(2, "jsonobj2", Map("x" -> "2")))
      val json = List(
        ("intField" -> 1) ~ ("stringField" -> "jsonobj1") ~ ("mapField" -> (("x" -> "1"))),
        ("intField" -> 2) ~ ("stringField" -> "jsonobj2") ~ ("mapField" -> (("x" -> "2")))
      )
      passBasicTests(lst, rec.mandatoryMongoJsonObjectListField, rec.legacyOptionalMongoJsonObjectListField)
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
      val lst = List(MongoCaseClassTestObject(1,"str1"))
      rec.mongoCaseClassListField.setFromAny(lst)
      rec.mongoCaseClassListField.value must_== lst
    }
  }

  "MongoMapField (String)" should {
    "function correctly" in {
      val rec = MapTestRecord.createRecord
      val map = Map("a" -> "abc", "b" -> "def", "c" -> "ghi")
      passBasicTests(map, rec.mandatoryStringMapField, rec.legacyOptionalStringMapField)
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
      passBasicTests(map, rec.mandatoryIntMapField, rec.legacyOptionalIntMapField)
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
      val subRec = SubRecord.createRecord.name("subrecord")

      val srJson =
        JObject(List(
          JField("name", JString("subrecord")),
          JField("subsub", JObject(List(
            JField("name", JString(""))
          ))),
          JField("subsublist", JArray(List())),
          JField("when", JObject(List(
            JField("$dt", JString(rec.meta.formats.dateFormat.format(subRec.when.value)))
          ))),
          JField("slist", JArray(List())),
          JField("smap", JObject(List())),
          JField("oid", JObject(List(JField("$oid", JString(subRec.oid.value.toString))))),
          JField("pattern", JObject(List(
            JField("$regex", JString(subRec.pattern.value.pattern)),
            JField("$flags", JInt(subRec.pattern.value.flags))
          ))),
          JField("uuid", JObject(List(JField("$uuid", JString(subRec.uuid.value.toString)))))
        ))

      val srJsExp = new JsExp {
        def toJsCmd = Printer.compact(render(srJson))
      }

      passBasicTests(subRec, rec.mandatoryBsonRecordField, rec.legacyOptionalBsonRecordField, false)
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
      val lst = List(SubRecord.createRecord.name("subrec1"), SubRecord.createRecord.name("subrec2"))
      val sr1Json =
        JObject(List(
          JField("name", JString("subrec1")),
          JField("subsub", JObject(List(
            JField("name", JString(""))
          ))),
          JField("subsublist", JArray(List())),
          JField("when", JObject(List(
            JField("$dt", JString(rec.meta.formats.dateFormat.format(lst(0).when.value)))
          ))),
          JField("slist", JArray(List())),
          JField("smap", JObject(List())),
          JField("oid", JObject(List(JField("$oid", JString(lst(0).oid.value.toString))))),
          JField("pattern", JObject(List(
            JField("$regex", JString(lst(0).pattern.value.pattern)),
            JField("$flags", JInt(lst(0).pattern.value.flags))
          ))),
          JField("uuid", JObject(List(JField("$uuid", JString(lst(0).uuid.value.toString)))))
        ))
      val sr2Json =
        JObject(List(
          JField("name", JString("subrec2")),
          JField("subsub", JObject(List(
            JField("name", JString(""))
          ))),
          JField("subsublist", JArray(List())),
          JField("when", JObject(List(
            JField("$dt", JString(rec.meta.formats.dateFormat.format(lst(1).when.value)))
          ))),
          JField("slist", JArray(List())),
          JField("smap", JObject(List())),
          JField("oid", JObject(List(JField("$oid", JString(lst(1).oid.value.toString))))),
          JField("pattern", JObject(List(
            JField("$regex", JString(lst(1).pattern.value.pattern)),
            JField("$flags", JInt(lst(1).pattern.value.flags))
          ))),
          JField("uuid", JObject(List(JField("$uuid", JString(lst(1).uuid.value.toString)))))
        ))
      val sr1JsExp = new JsExp {
        def toJsCmd = compact(render(sr1Json))
      }
      val sr2JsExp = new JsExp {
        def toJsCmd = compact(render(sr2Json))
      }

      passBasicTests(lst, rec.mandatoryBsonRecordListField, rec.legacyOptionalBsonRecordListField)
      passConversionTests(
        lst,
        rec.mandatoryBsonRecordListField,
        JsArray(sr1JsExp, sr2JsExp),
        JArray(List(sr1Json, sr2Json)),
        Empty
      )
    }
  }
}

