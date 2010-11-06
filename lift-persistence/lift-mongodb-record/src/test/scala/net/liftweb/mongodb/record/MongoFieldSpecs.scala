/*
 * Copyright 2006-2010 WorldWide Conferencing, LLC
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

package net.liftweb {
package mongodb {
package record {

import field._
import common._
import http.{LiftSession, S}
import http.js.JE._
import http.js.JsExp
import json.JsonAST._
import util.FieldError
import util.Helpers.randomString

import java.util.{Calendar, Date, UUID}
import java.util.regex.Pattern

import scala.xml.Text

import org.bson.types.ObjectId
import org.specs.Specification
import org.specs.runner.JUnit4

import net.liftweb.record._

import com.mongodb.DBRef

class MongoFieldSpecsTest extends JUnit4(MongoFieldSpecs)

object MongoFieldSpecs extends Specification with MongoTestKit {
  import fixtures._

  def passBasicTests[A](example: A, mandatory: MandatoryTypedField[A], legacyOptional: MandatoryTypedField[A])(implicit m: scala.reflect.Manifest[A]): Unit = {
    val canCheckDefaultValues =
      !mandatory.defaultValue.isInstanceOf[Date] && // don't try to use the default value of date/time typed fields, because it changes from moment to moment!
      !mandatory.defaultValue.isInstanceOf[ObjectId] && // same with ObjectId
      !mandatory.defaultValue.isInstanceOf[Pattern] &&
      !mandatory.defaultValue.isInstanceOf[UUID] &&
      !mandatory.defaultValue.isInstanceOf[DBRef]


    def commonBehaviorsForAllFlavors(in: TypedField[A]): Unit = {
      if (canCheckDefaultValues) {
        "which have the correct initial value" in {
          mandatory.value must_== mandatory.defaultValue
          mandatory.valueBox must_== mandatory.defaultValueBox
        }
      }

      "which are readable and writable" in {
        mandatory.valueBox must verify(_.isDefined)
        mandatory.set(example)
        mandatory.value must_== example
        mandatory.valueBox must_== Full(example)
        mandatory.clear
        mandatory.value must_!= example
        mandatory.valueBox must_!= Full(example)
        mandatory.setBox(Full(example))
        mandatory.value must_== example
        mandatory.valueBox must_== Full(example)
      }

      if (canCheckDefaultValues) {
        "which correctly clear back to the default" in {
          mandatory.valueBox must verify(_.isDefined)
          mandatory.clear
          mandatory.valueBox must_== mandatory.defaultValueBox
        }
      }

      "which capture error conditions set in" in {
        mandatory.setBox(Failure("my failure"))
        mandatory.valueBox must_== Failure("my failure")
      }
    }

    "support mandatory fields" in {
      commonBehaviorsForAllFlavors(mandatory)

      "which are configured correctly" in {
        mandatory.optional_? must_== false
      }

      "which initialize to some value" in {
        mandatory.valueBox must verify(_.isDefined)
      }

      "which correctly fail to be set to Empty" in {
        mandatory.valueBox must verify(_.isDefined)
        mandatory.setBox(Empty)
        mandatory.valueBox must beLike { case Failure(s, _, _) if s == mandatory.notOptionalErrorMessage => true }
      }
    }

    "support 'legacy' optional fields (override optional_?)" in {
      commonBehaviorsForAllFlavors(legacyOptional)

      "which are configured correctly" in {
        legacyOptional.optional_? must_== true
      }

      "which initialize to Empty" in {
        legacyOptional.valueBox must_== Empty
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

  def passConversionTests[A](example: A, mandatory: MandatoryTypedField[A], jsexp: JsExp, jvalue: JValue, formPattern: Box[String]): Unit = {

    "convert to JsExp" in {
      mandatory.set(example)
      //println(mandatory.asJs)
      mandatory.asJs mustEqual jsexp
    }

    "convert to JValue" in {
      mandatory.set(example)
      //println(mandatory.asJValue)
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
          //println(formXml)
          formXml must notBeEmpty
          formXml foreach { f =>
            f.toString must beMatching(fp)
          }
        }
      }
    }
  }

  "DateField" should {
    val rec = MongoFieldTypeTestRecord.createRecord
    val now = new Date
    val nowStr = rec.meta.formats.dateFormat.format(now)
    passBasicTests(now, rec.mandatoryDateField, rec.legacyOptionalDateField)
    passConversionTests(
      now,
      rec.mandatoryDateField,
      JsObj(("$dt", Str(nowStr))),
      JObject(List(JField("$dt", JString(nowStr)))),
      Full("<input name=\".*\" type=\"text\" tabindex=\"1\" value=\""+nowStr+"\" id=\"mandatoryDateField_id_field\"></input>")
    )
  }

  "DBRefField" should {
    val rec = MongoFieldTypeTestRecord.createRecord
    val dbref = DBRefTestRecord.createRecord.getRef
    passBasicTests(dbref, rec.mandatoryDBRefField, rec.legacyOptionalDBRefField)
  }

  "JsonObjectField" should {
    val rec = MongoFieldTypeTestRecord.createRecord
    val ttjo = TypeTestJsonObject(1, "jsonobj1")
    passBasicTests(ttjo, rec.mandatoryJsonObjectField, rec.legacyOptionalJsonObjectField)
    passConversionTests(
      ttjo,
      rec.mandatoryJsonObjectField,
      JsObj(("intField", Num(1)), ("stringField", Str("jsonobj1"))),
      JObject(List(JField("intField", JInt(1)), JField("stringField", JString("jsonobj1")))),
      Empty
    )
  }

  "ObjectIdField" should {
    val rec = MongoFieldTypeTestRecord.createRecord
    val oid = ObjectId.get
    passBasicTests(oid, rec.mandatoryObjectIdField, rec.legacyOptionalObjectIdField)
    passConversionTests(
      oid,
      rec.mandatoryObjectIdField,
      JsObj(("$oid", oid.toString)),
      JObject(List(JField("$oid", JString(oid.toString)))),
      Full("<input name=\".*\" type=\"text\" tabindex=\"1\" value=\""+oid.toString+"\" id=\"mandatoryObjectIdField_id_field\"></input>")
    )
  }

  "PatternField" should {
    val rec = MongoFieldTypeTestRecord.createRecord
    val ptrn = Pattern.compile("^Mo", Pattern.CASE_INSENSITIVE)
    passBasicTests(ptrn, rec.mandatoryPatternField, rec.legacyOptionalPatternField)
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
    passBasicTests(uuid, rec.mandatoryUUIDField, rec.legacyOptionalUUIDField)
    passConversionTests(
      uuid,
      rec.mandatoryUUIDField,
      JsObj(("$uuid", Str(uuid.toString))),
      JObject(List(JField("$uuid", JString(uuid.toString)))),
      Full("<input name=\".*\" type=\"text\" tabindex=\"1\" value=\""+uuid.toString+"\" id=\"mandatoryUUIDField_id_field\"></input>")
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

  "MongoListField (Int)" should {
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

  "MongoJsonObjectListField" should {
    val rec = ListTestRecord.createRecord
    val lst = List(TypeTestJsonObject(1, "jsonobj1"), TypeTestJsonObject(2, "jsonobj2"))
    passBasicTests(lst, rec.mandatoryMongoJsonObjectListField, rec.legacyOptionalMongoJsonObjectListField)
    passConversionTests(
      lst,
      rec.mandatoryMongoJsonObjectListField,
      JsArray(
        JsObj(("intField", Num(1)), ("stringField", Str("jsonobj1"))),
        JsObj(("intField", Num(2)), ("stringField", Str("jsonobj2")))
      ),
      JArray(List(
        JObject(List(JField("intField", JInt(1)), JField("stringField", JString("jsonobj1")))),
        JObject(List(JField("intField", JInt(2)), JField("stringField", JString("jsonobj2"))))
      )),
      Empty
    )
  }

  "MongoMapField (String)" should {
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

  "MongoMapField (Int)" should {
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

}
}
}
