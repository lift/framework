/*
 * Copyright 2010-2014 WorldWide Conferencing, LLC
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
package record

import java.util.Calendar

import org.specs2.mutable.Specification
import org.specs2.specification.Fragment
import org.joda.time._

import http.js.JE._
import common._
import http.{S, LiftSession}
import json._
import util._
import util.Helpers._

import field.Countries
import fixtures._

import JsonDSL._


/**
 * Systems under specification for Record.
 */
object RecordSpec extends Specification {
  "Record Specification".title

  "Record field introspection" should {
    val rec = FieldTypeTestRecord.createRecord
    val allExpectedFieldNames: List[String] = (for {
      typeName <- "Binary Boolean Country DateTime Decimal Double Email Enum Int Locale Long PostalCode String Textarea TimeZone JodaTime".split(" ")
      flavor <- "mandatory legacyOptional optional".split(" ")
    } yield flavor + typeName + "Field").toList

    "introspect only the expected fields" in {
      rec.fields().map(_.name).sortWith(_ < _) must_== allExpectedFieldNames.sortWith(_ < _)
    }

    "correctly look up fields by name" in {
      val fields = allExpectedFieldNames.flatMap(rec.fieldByName _)

      fields.length must_== allExpectedFieldNames.length
    }

    "not look up fields by bogus names" in {
      val fields =
        allExpectedFieldNames.flatMap { name =>
          rec.fieldByName("x" + name + "y")
        }

      fields.length must_== 0
    }

    "ignore synthetic methods" in {
      SyntheticTestRecord.metaFields.size must_== 1
    }

  }

  "Record lifecycle callbacks" should {
    def testOneHarness(scope: String, f: LifecycleTestRecord => HarnessedLifecycleCallbacks): Fragment = {
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

  "Record" should {
    val session = new LiftSession("", randomString(20), Empty)
    S.initIfUninitted(session){
      val gu: Array[Byte] = Array(18, 19, 20)
      val cal = Calendar.getInstance
      val dt: DateTime = DateTime.now

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
        .mandatoryJodaTimeField(dt)

      val fttrJValue: JValue =
        ("mandatoryBooleanField" -> false) ~
        ("mandatoryCountryField" -> 1) ~
        ("mandatoryDateTimeField" -> Helpers.toInternetDate(cal.getTime)) ~
        ("mandatoryDecimalField" -> "3.14") ~
        ("mandatoryDoubleField" -> 1999.0) ~
        ("mandatoryEmailField" -> "test@liftweb.net") ~
        ("mandatoryEnumField" -> 0) ~
        ("mandatoryIntField" -> 99) ~
        ("mandatoryLocaleField" -> "en_US") ~
        ("mandatoryLongField" -> 100) ~
        ("mandatoryPostalCodeField" -> "55401") ~
        ("mandatoryStringField" -> "foobar") ~
        ("mandatoryTextareaField" -> "foobar") ~
        ("mandatoryTimeZoneField" -> "America/Chicago") ~
        ("mandatoryBinaryField" -> "EhMU") ~
        ("mandatoryJodaTimeField" -> dt.getMillis)

      val fttrJson: String = compact(render(fttrJValue))

      val fttrAsJsObj = JsObj(
        ("mandatoryBooleanField", JsFalse),
        ("mandatoryCountryField", Str(Countries.USA.toString)),
        ("mandatoryDateTimeField", Str(Helpers.toInternetDate(cal.getTime))),
        ("mandatoryDecimalField", Num(3.14)),
        ("mandatoryDoubleField", Num(1999.0)),
        ("mandatoryEmailField", Str("test@liftweb.net")),
        ("mandatoryEnumField", Str(MyTestEnum.ONE.toString)),
        ("mandatoryIntField", Num(99)),
        ("mandatoryLocaleField", Str("en_US")),
        ("mandatoryLongField", Num(100)),
        ("mandatoryPostalCodeField", Str("55401")),
        ("mandatoryStringField", Str("foobar")),
        ("mandatoryTextareaField", Str("foobar")),
        ("mandatoryTimeZoneField", Str("America/Chicago")),
        ("mandatoryBinaryField", Str("121314")),
        ("mandatoryJodaTimeField", Num(dt.getMillis)),
        ("legacyOptionalBooleanField", JsNull),
        ("optionalBooleanField", JsNull),
        ("legacyOptionalCountryField", JsNull),
        ("optionalCountryField", JsNull),
        ("legacyOptionalDateTimeField", JsNull),
        ("optionalDateTimeField", JsNull),
        ("legacyOptionalDecimalField", JsNull),
        ("optionalDecimalField", JsNull),
        ("legacyOptionalDoubleField", JsNull),
        ("optionalDoubleField", JsNull),
        ("legacyOptionalEmailField", JsNull),
        ("optionalEmailField", JsNull),
        ("legacyOptionalEnumField", JsNull),
        ("optionalEnumField", JsNull),
        ("legacyOptionalIntField", JsNull),
        ("optionalIntField", JsNull),
        ("legacyOptionalLocaleField", JsNull),
        ("optionalLocaleField", JsNull),
        ("legacyOptionalLongField", JsNull),
        ("optionalLongField", JsNull),
        ("legacyOptionalPostalCodeField", JsNull),
        ("optionalPostalCodeField", JsNull),
        ("legacyOptionalStringField", JsNull),
        ("optionalStringField", JsNull),
        ("legacyOptionalTextareaField", JsNull),
        ("optionalTextareaField", JsNull),
        ("legacyOptionalTimeZoneField", JsNull),
        ("optionalTimeZoneField", JsNull),
        ("optionalBinaryField", JsNull),
        ("legacyOptionalBinaryField", JsNull),
        ("legacyOptionalJodaTimeField", JsNull),
        ("optionalJodaTimeField", JsNull)
      )

      "convert to JsExp (via asJSON)" in {
        S.initIfUninitted(new LiftSession("", randomString(20), Empty)) {
          fttr.asJSON mustEqual fttrAsJsObj
        }
      }

      /*  Test broken
      "convert to JsExp (via asJsExp)" in {
        fttr.asJsExp mustEqual fttrAsJsObj
      }*/

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
          JField("optionalBinaryField", JNothing),
          JField("mandatoryJodaTimeField", JInt(dt.getMillis)),
          JField("legacyOptionalJodaTimeField", JNothing),
          JField("optionalJodaTimeField", JNothing)
        ))
      }

      "get set from json string using lift-json parser" in {
        S.initIfUninitted(new LiftSession("", randomString(20), Empty)) {
          val fttrFromJson = FieldTypeTestRecord.fromJsonString(fttrJson)

          fttrFromJson must_== Full(fttr)
        }
      }
    }
  }

  "basic record" should {
    "order fields according to fieldOrder" in {
      BasicTestRecord.metaFields must_==  List(BasicTestRecord.field2, BasicTestRecord.field1, BasicTestRecord.fieldThree)
    }
  }
}

