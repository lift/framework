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

import _root_.java.util.Calendar
import _root_.net.liftweb.common.{Box, Empty, Failure, Full}
import _root_.net.liftweb.http.S
import _root_.net.liftweb.record.field.{Countries, PasswordField, StringField}
import _root_.net.liftweb.util.FieldError
import _root_.org.specs._
import _root_.org.specs.runner.{ConsoleRunner, JUnit3}
import _root_.scala.xml.{Node, Text}

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

    "support mandatory fields" >> {
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

    "support 'legacy' optional fields (override optional_?)" >> {
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

    "support optional fields" >> {
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
    passBasicTests(true, rec.mandatoryBooleanField, rec.legacyOptionalBooleanField, rec.optionalBooleanField)
  }

  "CountryField" should {
    val rec = FieldTypeTestRecord.createRecord
    passBasicTests(Countries.Canada, rec.mandatoryCountryField, rec.legacyOptionalCountryField, rec.optionalCountryField)
  }

  "DateTimeField" should {
    val rec = FieldTypeTestRecord.createRecord
    passBasicTests(Calendar.getInstance, rec.mandatoryDateTimeField, rec.legacyOptionalDateTimeField, rec.optionalDateTimeField)
  }

  "DecimalField" should {
    val rec = FieldTypeTestRecord.createRecord
    passBasicTests(BigDecimal("12.34"), rec.mandatoryDecimalField, rec.legacyOptionalDecimalField, rec.optionalDecimalField)
  }

  "DoubleField" should {
    val rec = FieldTypeTestRecord.createRecord
    passBasicTests(12.34, rec.mandatoryDoubleField, rec.legacyOptionalDoubleField, rec.optionalDoubleField)
  }

  "EmailField" should {
    val rec = FieldTypeTestRecord.createRecord
    passBasicTests("foo@bar.baz", rec.mandatoryEmailField, rec.legacyOptionalEmailField, rec.optionalEmailField)
  }

  "EnumField" should {
    val rec = FieldTypeTestRecord.createRecord
    passBasicTests(MyTestEnum.TWO, rec.mandatoryEnumField, rec.legacyOptionalEnumField, rec.optionalEnumField)
  }

  "IntField" should {
    val rec = FieldTypeTestRecord.createRecord
    passBasicTests(123, rec.mandatoryIntField, rec.legacyOptionalIntField, rec.optionalIntField)
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
    passBasicTests(1234L, rec.mandatoryLongField, rec.legacyOptionalLongField, rec.optionalLongField)
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
    rec.mandatoryCountryField.set(Countries.USA)
    passBasicTests("02452", rec.mandatoryPostalCodeField, rec.legacyOptionalPostalCodeField, rec.optionalPostalCodeField)
  }

  "StringField" should {
    {
      val rec = FieldTypeTestRecord.createRecord
      passBasicTests("foobar", rec.mandatoryStringField, rec.legacyOptionalStringField, rec.optionalStringField)
    }

    "honor validators configured in the usual way" in {
      val rec = StringTestRecord.createRecord

      rec.validate must_== (
        FieldError(rec.string, Text("String field name must be at least 3 characters.")) ::
        Nil
      )
    }

    "honor harnessed validators" >> {
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

    "support filtering" >> {
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
    passBasicTests("foobar", rec.mandatoryTextareaField, rec.legacyOptionalTextareaField, rec.optionalTextareaField)
  }

  "TimeZoneField" should {
    val rec = FieldTypeTestRecord.createRecord
    val example = java.util.TimeZone.getDefault.getID match {
      case "America/New_York" => "Europe/London"
      case _ => "America/New_York"
    }
    passBasicTests(example, rec.mandatoryTimeZoneField, rec.legacyOptionalTimeZoneField, rec.optionalTimeZoneField)
  }
}

}
}
