/*
 * Copyright 2010 WorldWide Conferencing, LLC
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
package record {

import java.util.Calendar
import scala.xml.{Node, Text}
import common.{Box, Empty, Full}
import http.js.JE._
import http.S
import field.{Countries, PasswordField, StringField}
import json.JsonAST._
import util.FieldError
import util.Helpers
import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit3}

import fixtures._

class RecordSpecsAsTest extends JUnit3(RecordSpecs)
object RecordSpecsRunner extends ConsoleRunner(RecordSpecs)

object RecordSpecs extends Specification {
  "Record field introspection" should {
    val rec = FieldTypeTestRecord.createRecord
    val allExpectedFieldNames: List[String] = (for {
      typeName <- "Binary Boolean Country DateTime Decimal Double Email Enum Int Locale Long PostalCode String Textarea TimeZone".split(" ")
      flavor <- "mandatory legacyOptional optional".split(" ")
    } yield flavor + typeName + "Field").toList

    "introspect only the expected fields" in {
      rec.fields().map(_.name).sort(_ < _) must_== allExpectedFieldNames.sort(_ < _)
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

  "Record lifecycle callbacks" should {
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

    testOneHarness("the record level", rec => rec)
    testOneHarness("the inner object level", rec => rec.innerObjectWithCallbacks: HarnessedLifecycleCallbacks)
    testOneHarness("the field level", rec => rec.stringFieldWithCallbacks: HarnessedLifecycleCallbacks)
  }

  "Record" should {
    val gu: Array[Byte] = Array(18, 19, 20)
    val cal = Calendar.getInstance
    val fttr = FieldTypeTestRecord.createRecord
      .mandatoryBinaryField(gu)
      .mandatoryBooleanField(false)
      .mandatoryCountryField(Countries.USA)
      .mandatoryDateTimeField(cal)
      .mandatoryDecimalField(BigDecimal("3.14"))
      .mandatoryDoubleField(1999)
      .mandatoryEmailField("test@liftweb.net")
      .mandatoryEnumField(MyTestEnum.ONE)
      .mandatoryIntField(99)
      .mandatoryLocaleField("en_US")
      .mandatoryLongField(100L)
      .mandatoryPostalCodeField("55401")
      .mandatoryStringField("foobar")
      .mandatoryTextareaField("foobar")
      .mandatoryTimeZoneField("America/Chicago")

    val json = "{\"mandatoryBooleanField\": false, \"mandatoryCountryField\": 1, \"mandatoryDateTimeField\": \""+Helpers.toInternetDate(cal.getTime)+"\",\"mandatoryDecimalField\": \"3.14\", \"mandatoryDoubleField\": 1999.0,\"mandatoryEmailField\":\"test@liftweb.net\",\"mandatoryEnumField\":0,\"mandatoryIntField\":99,\"mandatoryLocaleField\":\"en_US\",\"mandatoryLongField\":100,\"mandatoryPostalCodeField\":\"55401\",\"mandatoryStringField\":\"foobar\",\"mandatoryTextareaField\":\"foobar\",\"mandatoryTimeZoneField\":\"America/Chicago\",\"mandatoryBinaryField\":\"EhMU\"}"

    val fttrAsJsObj = JsObj(
      ("mandatoryBooleanField", JsFalse),
      ("legacyOptionalBooleanField", Str("null")),
      ("optionalBooleanField", Str("null")),
      ("mandatoryCountryField", Str(Countries.USA.toString)),
      ("legacyOptionalCountryField", Str("null")),
      ("optionalCountryField", Str("null")),
      ("mandatoryDateTimeField", Str(Helpers.toInternetDate(cal.getTime))),
      ("legacyOptionalDateTimeField", Str("null")),
      ("optionalDateTimeField", Str("null")),
      ("mandatoryDecimalField", Num(3.14)),
      ("legacyOptionalDecimalField", Str("null")),
      ("optionalDecimalField", Str("null")),
      ("mandatoryDoubleField", Num(1999.0)),
      ("legacyOptionalDoubleField", Str("null")),
      ("optionalDoubleField", Str("null")),
      ("mandatoryEmailField", Str("test@liftweb.net")),
      ("legacyOptionalEmailField", Str("null")),
      ("optionalEmailField", Str("null")),
      ("mandatoryEnumField", Num(MyTestEnum.ONE.id)),
      ("legacyOptionalEnumField", Str("null")),
      ("optionalEnumField", Str("null")),
      ("mandatoryIntField", Num(99)),
      ("legacyOptionalIntField", Str("null")),
      ("optionalIntField", Str("null")),
      ("mandatoryLocaleField", Str("en_US")),
      ("legacyOptionalLocaleField", Str("null")),
      ("optionalLocaleField", Str("null")),
      ("mandatoryLongField", Num(100)),
      ("legacyOptionalLongField", Str("null")),
      ("optionalLongField", Str("null")),
      ("mandatoryPostalCodeField", Str("55401")),
      ("legacyOptionalPostalCodeField", Str("null")),
      ("optionalPostalCodeField", Str("null")),
      ("mandatoryStringField", Str("foobar")),
      ("legacyOptionalStringField", Str("null")),
      ("optionalStringField", Str("null")),
      ("mandatoryTextareaField", Str("foobar")),
      ("legacyOptionalTextareaField", Str("null")),
      ("optionalTextareaField", Str("null")),
      ("mandatoryTimeZoneField", Str("America/Chicago")),
      ("legacyOptionalTimeZoneField", Str("null")),
      ("optionalTimeZoneField", Str("null")),
      ("optionalBinaryField", Str("null")),
      ("legacyOptionalBinaryField", Str("null")),
      ("mandatoryBinaryField", Str("121314"))
    )

    "convert to JsExp (via asJSON)" in {
      fttr.asJSON mustEqual fttrAsJsObj
    }

    "convert to JsExp (via asJsExp)" in {
      fttr.asJsExp mustEqual fttrAsJsObj
    }

    "convert to JValue" in {
      fttr.asJValue mustEqual JObject(List(
        JField("mandatoryBooleanField", JBool(false)),
        JField("legacyOptionalBooleanField", JNothing),
        JField("optionalBooleanField", JNothing),
        JField("mandatoryCountryField", JInt(Countries.USA.id)),
        JField("legacyOptionalCountryField", JNothing),
        JField("optionalCountryField", JNothing),
        JField("mandatoryDateTimeField", JString(Helpers.toInternetDate(cal.getTime))),
        JField("legacyOptionalDateTimeField", JNothing),
        JField("optionalDateTimeField", JNothing),
        JField("mandatoryDecimalField", JString("3.14")),
        JField("legacyOptionalDecimalField", JNothing),
        JField("optionalDecimalField", JNothing),
        JField("mandatoryDoubleField", JDouble(1999.0)),
        JField("legacyOptionalDoubleField", JNothing),
        JField("optionalDoubleField", JNothing),
        JField("mandatoryEmailField", JString("test@liftweb.net")),
        JField("legacyOptionalEmailField", JNothing),
        JField("optionalEmailField", JNothing),
        JField("mandatoryEnumField", JInt(MyTestEnum.ONE.id)),
        JField("legacyOptionalEnumField", JNothing),
        JField("optionalEnumField", JNothing),
        JField("mandatoryIntField", JInt(99)),
        JField("legacyOptionalIntField", JNothing),
        JField("optionalIntField", JNothing),
        JField("mandatoryLocaleField", JString("en_US")),
        JField("legacyOptionalLocaleField", JNothing),
        JField("optionalLocaleField", JNothing),
        JField("mandatoryLongField", JInt(100)),
        JField("legacyOptionalLongField", JNothing),
        JField("optionalLongField", JNothing),
        JField("mandatoryPostalCodeField", JString("55401")),
        JField("legacyOptionalPostalCodeField", JNothing),
        JField("optionalPostalCodeField", JNothing),
        JField("mandatoryStringField", JString("foobar")),
        JField("legacyOptionalStringField", JNothing),
        JField("optionalStringField", JNothing),
        JField("mandatoryTextareaField", JString("foobar")),
        JField("legacyOptionalTextareaField", JNothing),
        JField("optionalTextareaField", JNothing),
        JField("mandatoryTimeZoneField", JString("America/Chicago")),
        JField("legacyOptionalTimeZoneField", JNothing),
        JField("optionalTimeZoneField", JNothing),
        JField("mandatoryBinaryField", JString("EhMU")),
        JField("legacyOptionalBinaryField", JNothing),
        JField("optionalBinaryField", JNothing)
      ))
    }

    "get set from json string using lift-json parser" in {
      val fttrFromJson = FieldTypeTestRecord.fromJsonString(json)
      fttrFromJson must notBeEmpty
      fttrFromJson foreach { r =>
        r.mandatoryDecimalField.value mustEqual fttr.mandatoryDecimalField.value
        r mustEqual fttr
      }
    }

    "get set from json string using util.JSONParser" in {
      val fttrFromJSON = FieldTypeTestRecord.fromJSON(json)
      fttrFromJSON must notBeEmpty
      fttrFromJSON foreach { r =>
        r mustEqual fttr
      }
    }
  }
  
  "basic record" should {
    "order fields according to fieldOrder" in {
      BasicTestRecord.metaFields must_==  List(BasicTestRecord.field2, BasicTestRecord.field1, BasicTestRecord.field3)
    }
  }
}

}
}
