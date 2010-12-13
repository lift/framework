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
package fixtures {

import java.math.MathContext
import scala.xml.Text
import common.{Box, Empty, Full}
import util.{FieldError, Helpers}
import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit3}

import field._

class BasicTestRecord private () extends Record[BasicTestRecord] {
  def meta = BasicTestRecord

  object field1 extends StringField(this,10)
  object field2 extends StringField(this,10)
  object field3 extends StringField(this,10)
}

object BasicTestRecord extends BasicTestRecord with MetaRecord[BasicTestRecord] {
  override def fieldOrder = List(field2,field1)
}

class PasswordTestRecord private () extends Record[PasswordTestRecord] {
  def meta = PasswordTestRecord

  object password extends PasswordField(this) {
    override def validations = validateNonEmptyPassword _ ::
    super.validations

    def validateNonEmptyPassword(v: String): List[FieldError] = 
      v match {
        case "testvalue" => Text("no way!")
        case _ => Nil
      }
  }
}

object PasswordTestRecord extends PasswordTestRecord with MetaRecord[PasswordTestRecord]

class StringTestRecord private () extends Record[StringTestRecord] {
  def meta = StringTestRecord

  object string extends StringField(this, 32) {
    override def validations =
      valMinLen(3, "String field name must be at least 3 characters.") _ ::
      super.validations
  }
}

object StringTestRecord extends StringTestRecord with MetaRecord[StringTestRecord]

object MyTestEnum extends Enumeration {
  val ONE = Value("ONE")
  val TWO = Value("TWO")
  val THREE = Value("THREE")
}

trait HarnessedLifecycleCallbacks extends LifecycleCallbacks {
  var beforeValidationHarness: () => Unit = () => ()
  override def beforeValidation = beforeValidationHarness()
  var afterValidationHarness: () => Unit = () => ()
  override def afterValidation = afterValidationHarness()

  var beforeSaveHarness: () => Unit = () => ()
  override def beforeSave = beforeSaveHarness()
  var beforeCreateHarness: () => Unit = () => ()
  override def beforeCreate = beforeCreateHarness()
  var beforeUpdateHarness: () => Unit = () => ()
  override def beforeUpdate = beforeUpdateHarness()

  var afterSaveHarness: () => Unit = () => ()
  override def afterSave = afterSaveHarness()
  var afterCreateHarness: () => Unit = () => ()
  override def afterCreate = afterCreateHarness()
  var afterUpdateHarness: () => Unit = () => ()
  override def afterUpdate = afterUpdateHarness()

  var beforeDeleteHarness: () => Unit = () => ()
  override def beforeDelete = beforeDeleteHarness()
  var afterDeleteHarness: () => Unit = () => ()
  override def afterDelete = afterDeleteHarness()
}

class LifecycleTestRecord private () extends Record[LifecycleTestRecord] with HarnessedLifecycleCallbacks {
  def meta = LifecycleTestRecord

  def foreachCallback(f: LifecycleCallbacks => Any): Unit =
    meta.foreachCallback(this, f)

  object innerObjectWithCallbacks extends LifecycleCallbacks with HarnessedLifecycleCallbacks

  object stringFieldWithCallbacks extends StringField(this, 100) with LifecycleCallbacks with HarnessedLifecycleCallbacks
}

object LifecycleTestRecord extends LifecycleTestRecord with MetaRecord[LifecycleTestRecord] {
  // without this, the Scala 2.7 compiler panics, so don't blame me if you remove it and it's confusing!
  override def foreachCallback(inst: LifecycleTestRecord, f: LifecycleCallbacks => Any) = super.foreachCallback(inst, f)
}


class ValidationTestRecord private() extends Record[ValidationTestRecord] {
  def meta = ValidationTestRecord

  object stringFieldWithValidation extends StringField(this, 100) {
    var validationHarness: ValueType => List[FieldError] = x => Nil
    override def validations = validationHarness :: super.validations
  }
}

object ValidationTestRecord extends ValidationTestRecord with MetaRecord[ValidationTestRecord]


class FilterTestRecord private() extends Record[FilterTestRecord] {
  def meta = FilterTestRecord

  object stringFieldWithFiltering extends StringField(this, 100) {
    var setFilterHarness: ValueType => ValueType = identity _
    override protected def setFilter = setFilterHarness :: super.setFilter

    var setFilterBoxHarness: Box[MyType] => Box[MyType] = identity _
    override protected def setFilterBox = setFilterBoxHarness :: super.setFilterBox
  }
}

object FilterTestRecord extends FilterTestRecord with MetaRecord[FilterTestRecord]


class FieldTypeTestRecord private () extends Record[FieldTypeTestRecord] {
  def meta = FieldTypeTestRecord

  object mandatoryBinaryField extends BinaryField(this)
  object legacyOptionalBinaryField extends BinaryField(this) { override def optional_? = true }
  object optionalBinaryField extends OptionalBinaryField(this)

  object mandatoryBooleanField extends BooleanField(this)
  object legacyOptionalBooleanField extends BooleanField(this) { override def optional_? = true }
  object optionalBooleanField extends OptionalBooleanField(this)

  object mandatoryCountryField extends CountryField(this)
  object legacyOptionalCountryField extends CountryField(this) { override def optional_? = true }
  object optionalCountryField extends OptionalCountryField(this)

  object mandatoryDateTimeField extends DateTimeField(this)
  object legacyOptionalDateTimeField extends DateTimeField(this) { override def optional_? = true }
  object optionalDateTimeField extends OptionalDateTimeField(this)

  object mandatoryDecimalField extends DecimalField(this, MathContext.UNLIMITED, 2)
  object legacyOptionalDecimalField extends DecimalField(this, MathContext.UNLIMITED, 2) { override def optional_? = true }
  object optionalDecimalField extends OptionalDecimalField(this, MathContext.UNLIMITED, 2)

  object mandatoryDoubleField extends DoubleField(this)
  object legacyOptionalDoubleField extends DoubleField(this) { override def optional_? = true }
  object optionalDoubleField extends OptionalDoubleField(this)

  object mandatoryEmailField extends EmailField(this, 100)
  object legacyOptionalEmailField extends EmailField(this, 100) { override def optional_? = true }
  object optionalEmailField extends OptionalEmailField(this, 100)

  object mandatoryEnumField extends EnumField(this, MyTestEnum)
  object legacyOptionalEnumField extends EnumField(this, MyTestEnum) { override def optional_? = true }
  object optionalEnumField extends OptionalEnumField(this, MyTestEnum)

  object mandatoryIntField extends IntField(this)
  object legacyOptionalIntField extends IntField(this) { override def optional_? = true }
  object optionalIntField extends OptionalIntField(this)

  object mandatoryLocaleField extends LocaleField(this)
  object legacyOptionalLocaleField extends LocaleField(this) { override def optional_? = true }
  object optionalLocaleField extends OptionalLocaleField(this)

  object mandatoryLongField extends LongField(this)
  object legacyOptionalLongField extends LongField(this) { override def optional_? = true }
  object optionalLongField extends OptionalLongField(this)

    /*
  object mandatoryPasswordField extends PasswordField(this)
  object legacyOptionalPasswordField extends PasswordField(this) { override def optional_? = true }
  object optionalPasswordField extends OptionalPasswordField(this)
    */

  // FIXME would be nice to have some of these PostalCode fields depend on an OptionalCountryField, but the type sig of
  // PostalCodeField does not yet allow it.
  object mandatoryPostalCodeField extends PostalCodeField(this, mandatoryCountryField)
  object legacyOptionalPostalCodeField extends PostalCodeField(this, mandatoryCountryField) { override def optional_? = true }
  object optionalPostalCodeField extends OptionalPostalCodeField(this, mandatoryCountryField)

  object mandatoryStringField extends StringField(this, 100)
  object legacyOptionalStringField extends StringField(this, 100) { override def optional_? = true }
  object optionalStringField extends OptionalStringField(this, 100)

  object mandatoryTextareaField extends TextareaField(this, 100)
  object legacyOptionalTextareaField extends TextareaField(this, 100) { override def optional_? = true }
  object optionalTextareaField extends OptionalTextareaField(this, 100)

  object mandatoryTimeZoneField extends TimeZoneField(this)
  object legacyOptionalTimeZoneField extends TimeZoneField(this) { override def optional_? = true }
  object optionalTimeZoneField extends OptionalTimeZoneField(this)

  override def equals(other: Any): Boolean = other match {
    case that:FieldTypeTestRecord =>
      //this.mandatoryBinaryField.value mustEqual that.mandatoryBinaryField.value
      this.mandatoryBooleanField.value == that.mandatoryBooleanField.value &&
      this.mandatoryCountryField.value == that.mandatoryCountryField.value &&
      Helpers.toInternetDate(this.mandatoryDateTimeField.value.getTime) ==
        Helpers.toInternetDate(that.mandatoryDateTimeField.value.getTime) &&
      //this.mandatoryDecimalField.value == that.mandatoryDecimalField.value &&
      this.mandatoryDoubleField.value == that.mandatoryDoubleField.value &&
      this.mandatoryEmailField.value == that.mandatoryEmailField.value &&
      this.mandatoryEnumField.value == that.mandatoryEnumField.value &&
      this.mandatoryIntField.value == that.mandatoryIntField.value &&
      this.mandatoryLocaleField.value == that.mandatoryLocaleField.value &&
      this.mandatoryLongField.value == that.mandatoryLongField.value &&
      this.mandatoryPostalCodeField.value == that.mandatoryPostalCodeField.value &&
      this.mandatoryStringField.value == that.mandatoryStringField.value &&
      this.mandatoryTextareaField.value == that.mandatoryTextareaField.value &&
      this.mandatoryTimeZoneField.value == that.mandatoryTimeZoneField.value
    case _ => false
  }
}

object FieldTypeTestRecord extends FieldTypeTestRecord with MetaRecord[FieldTypeTestRecord]

}
}
}
