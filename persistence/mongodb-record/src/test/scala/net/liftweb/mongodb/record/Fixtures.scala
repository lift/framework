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
package mongodb {
package record {
package fixtures {

import field._

import common.{Box, Empty, Failure, Full}
import json.ext.JsonBoxSerializer
import util.FieldError

import java.math.MathContext
import scala.xml.Text

import net.liftweb.record._
import net.liftweb.record.field._

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

class FieldTypeTestRecord private () extends MongoRecord[FieldTypeTestRecord] with MongoId[FieldTypeTestRecord] {
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

  /*
  object mandatoryDateTimeField extends DateTimeField(this)
  object legacyOptionalDateTimeField extends DateTimeField(this) { override def optional_? = true }
  object optionalDateTimeField extends OptionalDateTimeField(this)
  */

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
      //this.mandatoryBinaryField.value == that.mandatoryBinaryField.value &&
      this.mandatoryBooleanField.value == that.mandatoryBooleanField.value &&
      this.mandatoryCountryField.value == that.mandatoryCountryField.value &&
      this.mandatoryDecimalField.value == that.mandatoryDecimalField.value &&
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

object FieldTypeTestRecord extends FieldTypeTestRecord with MongoMetaRecord[FieldTypeTestRecord]


case class TypeTestJsonObject(
  intField: Int,
  stringField: String
) extends JsonObject[TypeTestJsonObject]
{
  // TODO: Add more types
  def meta = TypeTestJsonObject
}
object TypeTestJsonObject extends JsonObjectMeta[TypeTestJsonObject]

class DBRefTestRecord private () extends MongoRecord[DBRefTestRecord] with MongoId[DBRefTestRecord] {
  def meta = DBRefTestRecord
}
object DBRefTestRecord extends DBRefTestRecord with MongoMetaRecord[DBRefTestRecord]

class MongoFieldTypeTestRecord private () extends MongoRecord[MongoFieldTypeTestRecord] with MongoId[MongoFieldTypeTestRecord] {
  def meta = MongoFieldTypeTestRecord

  object mandatoryDateField extends DateField(this)
  object legacyOptionalDateField extends DateField(this) { override def optional_? = true }

  object mandatoryDBRefField extends DBRefField[MongoFieldTypeTestRecord, DBRefTestRecord](this, DBRefTestRecord)
  object legacyOptionalDBRefField extends DBRefField[MongoFieldTypeTestRecord, DBRefTestRecord](this, DBRefTestRecord) { override def optional_? = true }

  object mandatoryJsonObjectField extends JsonObjectField(this, TypeTestJsonObject) {
    def defaultValue = TypeTestJsonObject(0, "")
  }
  object legacyOptionalJsonObjectField extends JsonObjectField(this, TypeTestJsonObject) {
    override def optional_? = true
    def defaultValue = TypeTestJsonObject(0, "")
  }

  object mandatoryObjectIdField extends ObjectIdField(this)
  object legacyOptionalObjectIdField extends ObjectIdField(this) { override def optional_? = true }

  object mandatoryPatternField extends PatternField(this)
  object legacyOptionalPatternField extends PatternField(this) { override def optional_? = true }

  object mandatoryUUIDField extends UUIDField(this)
  object legacyOptionalUUIDField extends UUIDField(this) { override def optional_? = true }

  override def equals(other: Any): Boolean = other match {
    case that:MongoFieldTypeTestRecord =>
      this.mandatoryDateField.value == that.mandatoryDateField.value &&
      //this.mandatoryDBRefField.value.getId == that.mandatoryDBRefField.value.getId &&
      //this.mandatoryDBRefField.value.getRef == that.mandatoryDBRefField.value.getRef &&
      this.mandatoryJsonObjectField.value == that.mandatoryJsonObjectField.value &&
      this.mandatoryObjectIdField.value == that.mandatoryObjectIdField.value &&
      this.mandatoryPatternField.value.pattern == that.mandatoryPatternField.value.pattern &&
      this.mandatoryPatternField.value.flags == that.mandatoryPatternField.value.flags &&
      this.mandatoryUUIDField.value == that.mandatoryUUIDField.value
    case _ => false
  }
}

object MongoFieldTypeTestRecord extends MongoFieldTypeTestRecord with MongoMetaRecord[MongoFieldTypeTestRecord] {
  override def formats = allFormats
}

class PasswordTestRecord private () extends MongoRecord[PasswordTestRecord] with MongoId[PasswordTestRecord] {
  def meta = PasswordTestRecord

  object password extends MongoPasswordField(this, 3)
}
object PasswordTestRecord extends PasswordTestRecord with MongoMetaRecord[PasswordTestRecord]

class ListTestRecord private () extends MongoRecord[ListTestRecord] with MongoId[ListTestRecord] {
  def meta = ListTestRecord

  object mandatoryStringListField extends MongoListField[ListTestRecord, String](this)
  object legacyOptionalStringListField extends MongoListField[ListTestRecord, String](this) { override def optional_? = true }

  object mandatoryIntListField extends MongoListField[ListTestRecord, Int](this)
  object legacyOptionalIntListField extends MongoListField[ListTestRecord, Int](this) { override def optional_? = true }

  object mandatoryMongoJsonObjectListField extends MongoJsonObjectListField(this, TypeTestJsonObject)
  object legacyOptionalMongoJsonObjectListField extends MongoJsonObjectListField(this, TypeTestJsonObject) { override def optional_? = true }

  // TODO: More List types

  override def equals(other: Any): Boolean = other match {
    case that:ListTestRecord =>
      this.mandatoryStringListField.value == that.mandatoryStringListField.value &&
      this.mandatoryIntListField.value == that.mandatoryIntListField.value &&
      this.mandatoryMongoJsonObjectListField.value == that.mandatoryMongoJsonObjectListField.value
    case _ => false
  }
}
object ListTestRecord extends ListTestRecord with MongoMetaRecord[ListTestRecord] {
  override def formats = allFormats
}

class MapTestRecord extends MongoRecord[MapTestRecord] with MongoId[MapTestRecord] {
  def meta = MapTestRecord

  object mandatoryStringMapField extends MongoMapField[MapTestRecord, String](this)
  object legacyOptionalStringMapField extends MongoMapField[MapTestRecord, String](this) { override def optional_? = true }

  object mandatoryIntMapField extends MongoMapField[MapTestRecord, Int](this)
  object legacyOptionalIntMapField extends MongoMapField[MapTestRecord, Int](this) { override def optional_? = true }

  // TODO: More Map types, including JsonObject (will require a new Field type)

  override def equals(other: Any): Boolean = other match {
    case that:MapTestRecord =>
      this.mandatoryStringMapField.value == that.mandatoryStringMapField.value &&
      this.mandatoryIntMapField.value == that.mandatoryIntMapField.value
    case _ => false
  }
}
object MapTestRecord extends MapTestRecord with MongoMetaRecord[MapTestRecord] {
  override def formats = allFormats
}

class LifecycleTestRecord private ()
  extends MongoRecord[LifecycleTestRecord]
  with MongoId[LifecycleTestRecord]
  with HarnessedLifecycleCallbacks
{
  def meta = LifecycleTestRecord

  def foreachCallback(f: LifecycleCallbacks => Any): Unit =
    meta.foreachCallback(this, f)

  object innerObjectWithCallbacks extends LifecycleCallbacks with HarnessedLifecycleCallbacks

  object stringFieldWithCallbacks extends StringField(this, 100) with LifecycleCallbacks with HarnessedLifecycleCallbacks
}

object LifecycleTestRecord extends LifecycleTestRecord with MongoMetaRecord[LifecycleTestRecord] {
  // without this, the Scala 2.7 compiler panics, so don't blame me if you remove it and it's confusing!
  override def foreachCallback(inst: LifecycleTestRecord, f: LifecycleCallbacks => Any) = super.foreachCallback(inst, f)
}

case class JsonObj(id: String, name: String) extends JsonObject[JsonObj] {
  def meta = JsonObj
}
object JsonObj extends JsonObjectMeta[JsonObj]

class NullTestRecord extends MongoRecord[NullTestRecord] with MongoId[NullTestRecord] {

  def meta = NullTestRecord

  object nullstring extends StringField(this, 32) {
    override def optional_? = true
  }

  object jsonobj extends JsonObjectField[NullTestRecord, JsonObj](this, JsonObj) {
    def defaultValue = JsonObj("1", null)
  }
  object jsonobjlist extends MongoJsonObjectListField[NullTestRecord, JsonObj](this, JsonObj)
}

object NullTestRecord extends NullTestRecord with MongoMetaRecord[NullTestRecord]

case class BoxTestJsonObj(id: String, boxEmpty: Box[String], boxFull: Box[String], boxFail: Box[String])
extends JsonObject[BoxTestJsonObj] {
  def meta = BoxTestJsonObj
}
object BoxTestJsonObj extends JsonObjectMeta[BoxTestJsonObj]

class BoxTestRecord extends MongoRecord[BoxTestRecord] with MongoId[BoxTestRecord] {
  def meta = BoxTestRecord

  object jsonobj extends JsonObjectField[BoxTestRecord, BoxTestJsonObj](this, BoxTestJsonObj) {
    def defaultValue = BoxTestJsonObj("0", Empty, Full("Full String"), Failure("Failure"))
  }
  object jsonobjlist extends MongoJsonObjectListField[BoxTestRecord, BoxTestJsonObj](this, BoxTestJsonObj)
}
object BoxTestRecord extends BoxTestRecord with MongoMetaRecord[BoxTestRecord] {
  override def formats = super.formats + new JsonBoxSerializer
}

}
}
}
}
