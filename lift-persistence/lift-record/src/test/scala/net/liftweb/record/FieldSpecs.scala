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

import field.{Countries, PasswordField, StringField}

import common.{Box, Empty, Failure, Full}
import http.{LiftSession, S}
import http.js.JE._
import http.js.JsExp
import json.JsonAST._
import util.FieldError
import util.Helpers._

import java.util.Calendar
import scala.xml.{Node, Text}

import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit3}

import fixtures._

class FieldSpecsAsTest extends JUnit3(FieldSpecs)
object FieldSpecsRunner extends ConsoleRunner(FieldSpecs)

object FieldSpecs extends Specification {
  def passBasicTests[A](example: A, mandatory: MandatoryTypedField[A], legacyOptional: MandatoryTypedField[A], optional: OptionalTypedField[A])(implicit m: scala.reflect.Manifest[A]): Unit = {
    val canCheckDefaultValues =
      !mandatory.defaultValue.isInstanceOf[Calendar] // don't try to use the default value of date/time typed fields, because it changes from moment to moment!

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

    "support optional fields" in {
      commonBehaviorsForAllFlavors(optional)

      "which are configured correctly" in {
        optional.optional_? must_== true
      }

      "which initialize to Empty" in {
        optional.valueBox must_== Empty
      }

      "which do not fail when set to Empty" in {
        optional.set(Some(example))
        optional.value must_== Some(example)
        optional.valueBox must_== Full(example)
        optional.set(None)
        optional.value must_== None
        optional.valueBox must_== Empty
        optional.set(Some(example))
        optional.value must_== Some(example)
        optional.valueBox must_== Full(example)
        optional.setBox(Empty)
        optional.value must_== None
        optional.valueBox must_== Empty
      }
    }
  }

  def passConversionTests[A](example: A, mandatory: MandatoryTypedField[A], jsexp: JsExp, jvalue: JValue, formPattern: Box[String]): Unit = {

    "convert to JsExp" in {
      mandatory.set(example)
      mandatory.asJs mustEqual jsexp
    }

    "convert to JValue" in {
      mandatory.set(example)
      mandatory.asJValue mustEqual jvalue
    }

    // toInternetDate doesn't retain millisecond data so, dates can't be compared accurately.
    if (!mandatory.defaultValue.isInstanceOf[Calendar]) {
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
          formXml foreach { f =>
            f.toString must beMatching(fp)
          }
        }
      }
    }
  }

    /* Since Array[Byte]s cannot be compared, commenting out this test for now
  "BinaryField" should {
    val rec = FieldTypeTestRecord.createRecord
    val a = new Array[Byte](3)
    a(0) = 1
    a(1) = 2
    a(2) = 3
    passBasicTests(a, rec.mandatoryBinaryField, rec.legacyOptionalBinaryField, rec.optionalBinaryField)
  }
    */

  "BooleanField" should {
    val rec = FieldTypeTestRecord.createRecord
    val bool = true
    passBasicTests(bool, rec.mandatoryBooleanField, rec.legacyOptionalBooleanField, rec.optionalBooleanField)
    passConversionTests(
      bool,
      rec.mandatoryBooleanField,
      JsTrue,
      JBool(bool),
      Full("<input value=\"false\" type=\"hidden\" name=\".*\"></input><input checked=\"checked\" tabIndex=\"1\" value=\"true\" type=\"checkbox\" name=\".*\" id=\"mandatoryBooleanField_id_field\"></input>")
    )
  }

  "CountryField" should {
    val rec = FieldTypeTestRecord.createRecord
    val country = Countries.Canada
    passBasicTests(country, rec.mandatoryCountryField, rec.legacyOptionalCountryField, rec.optionalCountryField)
    passConversionTests(
      country,
      rec.mandatoryCountryField,
      Str(country.toString),
      JInt(country.id),
      Full("<select tabindex=\"1\" name=\".*\" id=\"mandatoryCountryField_id_field\">.*<option value=\".*\" selected=\"selected\">"+country.toString+"</option>.*</select>")
    )
  }

  "DateTimeField" should {
    val rec = FieldTypeTestRecord.createRecord
    val dt = Calendar.getInstance
    val dtStr = toInternetDate(dt.getTime)
    passBasicTests(dt, rec.mandatoryDateTimeField, rec.legacyOptionalDateTimeField, rec.optionalDateTimeField)
    passConversionTests(
      dt,
      rec.mandatoryDateTimeField,
      Str(dtStr),
      JString(dtStr),
      Full("<input name=\".*\" type=\"text\" tabindex=\"1\" value=\""+dtStr+"\" id=\"mandatoryDateTimeField_id_field\"></input>")
    )
  }

  "DecimalField" should {
    val rec = FieldTypeTestRecord.createRecord
    val bd = BigDecimal("12.34")
    passBasicTests(bd, rec.mandatoryDecimalField, rec.legacyOptionalDecimalField, rec.optionalDecimalField)
    passConversionTests(
      bd,
      rec.mandatoryDecimalField,
      Str(bd.toString),
      JString(bd.toString),
      Full("<input name=\".*\" type=\"text\" tabindex=\"1\" value=\""+bd.toString+"\" id=\"mandatoryDecimalField_id_field\"></input>")
    )
  }

  "DoubleField" should {
    val rec = FieldTypeTestRecord.createRecord
    val d = 12.34
    passBasicTests(d, rec.mandatoryDoubleField, rec.legacyOptionalDoubleField, rec.optionalDoubleField)
    passConversionTests(
      d,
      rec.mandatoryDoubleField,
      Str(d.toString),
      JDouble(d),
      Full("<input name=\".*\" type=\"text\" tabindex=\"1\" value=\""+d.toString+"\" id=\"mandatoryDoubleField_id_field\"></input>")
    )
  }

  "EmailField" should {
    val rec = FieldTypeTestRecord.createRecord
    val email = "foo@bar.baz"
    passBasicTests(email, rec.mandatoryEmailField, rec.legacyOptionalEmailField, rec.optionalEmailField)
    passConversionTests(
      email,
      rec.mandatoryEmailField,
      Str(email),
      JString(email),
      Full("<input name=\".*\" type=\"text\" maxlength=\"100\" tabindex=\"1\" value=\""+email+"\" id=\"mandatoryEmailField_id_field\"></input>")
    )
  }

  "EnumField" should {
    val rec = FieldTypeTestRecord.createRecord
    val ev = MyTestEnum.TWO
    passBasicTests(ev, rec.mandatoryEnumField, rec.legacyOptionalEnumField, rec.optionalEnumField)
    passConversionTests(
      ev,
      rec.mandatoryEnumField,
      Str(ev.toString),
      JInt(ev.id),
      Full("<select tabindex=\"1\" name=\".*\" id=\"mandatoryEnumField_id_field\">.*<option value=\".*\" selected=\"selected\">"+ev.toString+"</option>.*</select>")
    )
  }

  "IntField" should {
    val rec = FieldTypeTestRecord.createRecord
    val num = 123
    passBasicTests(num, rec.mandatoryIntField, rec.legacyOptionalIntField, rec.optionalIntField)
    passConversionTests(
      num,
      rec.mandatoryIntField,
      Str(num.toString),
      JInt(num),
      Full("<input name=\".*\" type=\"text\" tabindex=\"1\" value=\""+num.toString+"\" id=\"mandatoryIntField_id_field\"></input>")
    )
  }

  "LocaleField" should {
    val rec = FieldTypeTestRecord.createRecord
    val example = java.util.Locale.getDefault.toString match {
      case "en_US" => "en_GB"
      case _ => "en_US"
    }
    passBasicTests(example, rec.mandatoryLocaleField, rec.legacyOptionalLocaleField, rec.optionalLocaleField)
  }

  "LongField" should {
    val rec = FieldTypeTestRecord.createRecord
    val lng = 1234L
    passBasicTests(lng, rec.mandatoryLongField, rec.legacyOptionalLongField, rec.optionalLongField)
    passConversionTests(
      lng,
      rec.mandatoryLongField,
      Str(lng.toString),
      JInt(lng),
      Full("<input name=\".*\" type=\"text\" tabindex=\"1\" value=\""+lng.toString+"\" id=\"mandatoryLongField_id_field\"></input>")
    )
  }

  "PasswordField" should {
    "require a nonempty password" in {
      val rec = PasswordTestRecord.createRecord.password("")

      rec.validate must_== (
        FieldError(rec.password, Text(S.??("password.must.be.set"))) ::
        Nil
      )
    }

    "validate the unencrypted value" in {
      val rec = PasswordTestRecord.createRecord.password("testvalue")

      rec.validate must_== (
        FieldError(rec.password, Text("no way!")) ::
        Nil
      )
    }
  }

  "PostalCodeField" should {
    val rec = FieldTypeTestRecord.createRecord
    val zip = "02452"
    rec.mandatoryCountryField.set(Countries.USA)
    passBasicTests(zip, rec.mandatoryPostalCodeField, rec.legacyOptionalPostalCodeField, rec.optionalPostalCodeField)
    passConversionTests(
      zip,
      rec.mandatoryPostalCodeField,
      Str(zip),
      JString(zip),
      Full("<input name=\".*\" type=\"text\" maxlength=\"32\" tabindex=\"1\" value=\""+zip+"\" id=\"mandatoryPostalCodeField_id_field\"></input>")
    )
  }

  "StringField" should {
    {
      val rec = FieldTypeTestRecord.createRecord
      val str = "foobar"
      passBasicTests(str, rec.mandatoryStringField, rec.legacyOptionalStringField, rec.optionalStringField)
      passConversionTests(
        str,
        rec.mandatoryStringField,
        Str(str),
        JString(str),
        Full("<input name=\".*\" type=\"text\" maxlength=\"100\" tabindex=\"1\" value=\""+str+"\" id=\"mandatoryStringField_id_field\"></input>")
      )
    }

    "honor validators configured in the usual way" in {
      val rec = StringTestRecord.createRecord

      rec.validate must_== (
        FieldError(rec.string, Text("String field name must be at least 3 characters.")) ::
        Nil
      )
    }

    "honor harnessed validators" in {
      val rec = ValidationTestRecord.createRecord
      val field = rec.stringFieldWithValidation
  
      "which always succeed" in {
        field.validationHarness = _ => Nil
        rec.validate must_== Nil
      }

      "which always fail" in {
        val fieldError = FieldError(field, Text("failed"))
        field.validationHarness = s => FieldError(rec.stringFieldWithValidation, Text("failed")) :: Nil
        rec.validate must_== (fieldError :: Nil)
      }

      "which receive the value" in {
        var received: String = null
        field.set("foobar")
        field.validationHarness = s => { received = s; Nil }
        rec.validate must_== Nil
        received must_== "foobar"
      }
    }

    "support filtering" in {
      val rec = FilterTestRecord.createRecord
      val field = rec.stringFieldWithFiltering

      "which does nothing" in {
        field.set("foobar")
        field.value must_== "foobar"
        field.valueBox must_== Full("foobar")
      }

      "which trims the input at the value level" in {
        field.setFilterHarness = _.trim
        field.set("  foobar  ")
        field.value must_== "foobar"
        field.valueBox must_== Full("foobar")
      }

      "which trims the input at the box level" in {
        field.setFilterBoxHarness = _.map(_.trim)
        field.set("   foobar   ")
        field.value must_== "foobar"
        field.valueBox must_== Full("foobar")
      }

      "which Empties the box" in {
        field.setFilterBoxHarness = s => Empty
        field.set("foobar")
        field.value must_== field.defaultValue
        field.valueBox must_== Empty
      }

      "which Fails" in {
        field.setFilterBoxHarness = s => Failure("my failure")
        field.set("foobar")
        field.value must_== field.defaultValue
        field.valueBox must_== Failure("my failure")
      }
    }
  }

  "TextareaField" should {
    val rec = FieldTypeTestRecord.createRecord
    val txt = "foobar"
    passBasicTests(txt, rec.mandatoryTextareaField, rec.legacyOptionalTextareaField, rec.optionalTextareaField)
    passConversionTests(
      txt,
      rec.mandatoryTextareaField,
      Str(txt),
      JString(txt),
      Full("<textarea name=\".*\" rows=\"8\" tabindex=\"1\" cols=\"20\" id=\"mandatoryTextareaField_id_field\">"+txt+"</textarea>")
    )
  }

  "TimeZoneField" should {
    val rec = FieldTypeTestRecord.createRecord
    val example = java.util.TimeZone.getDefault.getID match {
      case "America/New_York" => "Europe/London"
      case _ => "America/New_York"
    }
    passBasicTests(example, rec.mandatoryTimeZoneField, rec.legacyOptionalTimeZoneField, rec.optionalTimeZoneField)
    passConversionTests(
      example,
      rec.mandatoryTimeZoneField,
      Str(example),
      JString(example),
      Full("<select tabindex=\"1\" name=\".*\" id=\"mandatoryTimeZoneField_id_field\">.*<option value=\""+example+"\" selected=\"selected\">"+example+"</option>.*</select>")
    )
  }
}

}
}
