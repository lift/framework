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
package mongodb
package record
package fixtures

import field._

import common._
import json._
import json.ext.{EnumSerializer, JsonBoxSerializer}
import http.SHtml
import util.{FieldError, Helpers}

import java.math.MathContext
import java.util.{Date, UUID}
import java.util.regex.Pattern
import scala.xml.Text

import net.liftweb.record._
import net.liftweb.record.field._
import net.liftweb.record.field.joda._

import org.bson.types.ObjectId
import org.joda.time.DateTime

object MyTestEnum extends Enumeration {
  val ONE = Value("ONE")
  val TWO = Value("TWO")
  val THREE = Value("THREE")
}

trait HarnessedLifecycleCallbacks extends LifecycleCallbacks {
  this: BaseField =>

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

class FieldTypeTestRecord private () extends MongoRecord[FieldTypeTestRecord] with ObjectIdPk[FieldTypeTestRecord] {
  def meta = FieldTypeTestRecord

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

  object mandatoryJodaTimeField extends JodaTimeField(this)
  object legacyOptionalJodaTimeField extends JodaTimeField(this) { override def optional_? = true }
  object optionalJodaTimeField extends OptionalJodaTimeField(this)
}

object FieldTypeTestRecord extends FieldTypeTestRecord with MongoMetaRecord[FieldTypeTestRecord]

class BinaryFieldTestRecord extends MongoRecord[BinaryFieldTestRecord] with IntPk[BinaryFieldTestRecord] {
  def meta = BinaryFieldTestRecord

  object mandatoryBinaryField extends BinaryField(this) {
    // compare the elements of the Array
    override def equals(other: Any): Boolean = other match {
      case that: BinaryField[_] =>
        this.value.zip(that.value).filter(t => t._1 != t._2).length == 0
      case _ => false
    }
  }
  object legacyOptionalBinaryField extends BinaryField(this) {
    override def optional_? = true
    // compare the elements of the Array
    override def equals(other: Any): Boolean = other match {
      case that: BinaryField[_] => (this.valueBox, that.valueBox) match {
        case (Empty, Empty) => true
        case (Full(a), Full(b)) =>
          a.zip(b).filter(t => t._1 != t._2).length == 0
        case _ => false
      }
      case _ => false
    }
  }
  object optionalBinaryField extends OptionalBinaryField(this) {
    // compare the elements of the Array
    override def equals(other: Any): Boolean = other match {
      case that: OptionalBinaryField[_] => (this.valueBox, that.valueBox) match {
        case (Empty, Empty) => true
        case (Full(a), Full(b)) =>
          a.zip(b).filter(t => t._1 != t._2).length == 0
        case _ => false
      }
      case _ => false
    }
  }

  override def equals(other: Any): Boolean = other match {
    case that:BinaryFieldTestRecord =>
      this.id.value == that.id.value &&
      this.mandatoryBinaryField == that.mandatoryBinaryField &&
      this.legacyOptionalBinaryField == that.legacyOptionalBinaryField &&
      this.optionalBinaryField == that.optionalBinaryField
    case _ => false
  }
}
object BinaryFieldTestRecord extends BinaryFieldTestRecord with MongoMetaRecord[BinaryFieldTestRecord]


case class TypeTestJsonObject(
  intField: Int,
  stringField: String,
  mapField: Map[String, String]
) extends JsonObject[TypeTestJsonObject]
{
  // TODO: Add more types
  def meta = TypeTestJsonObject
}
object TypeTestJsonObject extends JsonObjectMeta[TypeTestJsonObject]

class DBRefTestRecord private () extends MongoRecord[DBRefTestRecord] with ObjectIdPk[DBRefTestRecord] {
  def meta = DBRefTestRecord
}
object DBRefTestRecord extends DBRefTestRecord with MongoMetaRecord[DBRefTestRecord]

class MongoFieldTypeTestRecord private () extends MongoRecord[MongoFieldTypeTestRecord] with ObjectIdPk[MongoFieldTypeTestRecord] {
  def meta = MongoFieldTypeTestRecord

  object mandatoryDateField extends DateField(this)
  object legacyOptionalDateField extends DateField(this) { override def optional_? = true }

  object mandatoryJsonObjectField extends JsonObjectField(this, TypeTestJsonObject) {
    def defaultValue = TypeTestJsonObject(0, "", Map[String, String]())
  }
  object legacyOptionalJsonObjectField extends JsonObjectField(this, TypeTestJsonObject) {
    override def optional_? = true
    def defaultValue = TypeTestJsonObject(0, "", Map[String, String]())
  }

  object mandatoryObjectIdField extends ObjectIdField(this)
  object legacyOptionalObjectIdField extends ObjectIdField(this) { override def optional_? = true }

  object mandatoryUUIDField extends UUIDField(this)
  object legacyOptionalUUIDField extends UUIDField(this) { override def optional_? = true }

  object mandatoryMongoCaseClassField extends MongoCaseClassField[MongoFieldTypeTestRecord, MongoCaseClassTestObject](this) {
    override def formats = owner.meta.formats
  }
}

object MongoFieldTypeTestRecord extends MongoFieldTypeTestRecord with MongoMetaRecord[MongoFieldTypeTestRecord] {
  override def formats = allFormats + new EnumSerializer(MyTestEnum)
}

class PatternFieldTestRecord private () extends MongoRecord[PatternFieldTestRecord] with ObjectIdPk[PatternFieldTestRecord] {
  def meta = PatternFieldTestRecord

  import java.util.regex.Pattern

  object mandatoryPatternField extends PatternField(this)
  object legacyOptionalPatternField extends PatternField(this) { override def optional_? = true }

  override def equals(other: Any): Boolean = equalsWithPatternCheck(other)
}

object PatternFieldTestRecord extends PatternFieldTestRecord with MongoMetaRecord[PatternFieldTestRecord] {
  override def formats = allFormats
}

class PasswordTestRecord private () extends MongoRecord[PasswordTestRecord] with ObjectIdPk[PasswordTestRecord] {
  def meta = PasswordTestRecord

  object password extends MongoPasswordField(this, 3)
}
object PasswordTestRecord extends PasswordTestRecord with MongoMetaRecord[PasswordTestRecord]

case class MongoCaseClassTestObject(intField: Int, stringField: String, enum: MyTestEnum.Value)

class ListTestRecord private () extends MongoRecord[ListTestRecord] with UUIDPk[ListTestRecord] {
  def meta = ListTestRecord

  object mandatoryStringListField extends MongoListField[ListTestRecord, String](this)
  object mandatoryMongoRefListField extends ObjectIdRefListField(this, FieldTypeTestRecord)
  object mandatoryIntListField extends MongoListField[ListTestRecord, Int](this)
  object mandatoryMongoJsonObjectListField extends MongoJsonObjectListField(this, TypeTestJsonObject)
  object mongoCaseClassListField extends MongoCaseClassListField[ListTestRecord, MongoCaseClassTestObject](this) {
    override def formats = owner.meta.formats
  }
}
object ListTestRecord extends ListTestRecord with MongoMetaRecord[ListTestRecord] {
  override def formats = allFormats + new EnumSerializer(MyTestEnum)
}

class MongoListTestRecord private () extends MongoRecord[MongoListTestRecord] with UUIDPk[MongoListTestRecord] {
  def meta = MongoListTestRecord

  object objectIdRefListField extends ObjectIdRefListField(this, FieldTypeTestRecord)
  object patternListField extends MongoListField[MongoListTestRecord, Pattern](this)
  object dateListField extends MongoListField[MongoListTestRecord, Date](this)
  object uuidListField extends MongoListField[MongoListTestRecord, UUID](this)
}
object MongoListTestRecord extends MongoListTestRecord with MongoMetaRecord[MongoListTestRecord] {
  override def formats = DefaultFormats.lossless + new ObjectIdSerializer + new PatternSerializer + new DateSerializer
}

class MongoJodaListTestRecord private () extends MongoRecord[MongoJodaListTestRecord] with UUIDPk[MongoJodaListTestRecord] {
  def meta = MongoJodaListTestRecord

  object dateTimeListField extends MongoListField[MongoJodaListTestRecord, DateTime](this)
}
object MongoJodaListTestRecord extends MongoJodaListTestRecord with MongoMetaRecord[MongoJodaListTestRecord] {
  override def formats = DefaultFormats.lossless + new DateTimeSerializer
}

class MapTestRecord private () extends MongoRecord[MapTestRecord] with StringPk[MapTestRecord] {
  def meta = MapTestRecord

  object mandatoryStringMapField extends MongoMapField[MapTestRecord, String](this)
  object mandatoryIntMapField extends MongoMapField[MapTestRecord, Int](this)

  // TODO: More Map types, including JsonObject (will require a new Field type)
}
object MapTestRecord extends MapTestRecord with MongoMetaRecord[MapTestRecord] {
  override def formats = allFormats
}

class LifecycleTestRecord private ()
  extends MongoRecord[LifecycleTestRecord]
  with ObjectIdPk[LifecycleTestRecord]
{
  def meta = LifecycleTestRecord

  def foreachCallback(f: LifecycleCallbacks => Any): Unit =
    meta.foreachCallback(this, f)

  object stringFieldWithCallbacks extends StringField(this, 100) with HarnessedLifecycleCallbacks
}

object LifecycleTestRecord extends LifecycleTestRecord with MongoMetaRecord[LifecycleTestRecord]

/*
 * SubRecord fields
 */
class SubRecord private () extends BsonRecord[SubRecord] {
  def meta = SubRecord

  object name extends StringField(this, 12)
  object subsub extends BsonRecordField(this, SubSubRecord)
  object subsublist extends BsonRecordListField(this, SubSubRecord)
  object when extends DateField(this)
  object slist extends MongoListField[SubRecord, String](this)
  object smap extends MongoMapField[SubRecord, String](this)
  object oid extends ObjectIdField(this)
  object pattern extends PatternField(this)
  object uuid extends UUIDField(this)

  override def equals(other: Any): Boolean = equalsWithPatternCheck(other)
}
object SubRecord extends SubRecord with BsonMetaRecord[SubRecord] {
  override def formats = allFormats
}

class SubSubRecord private () extends BsonRecord[SubSubRecord] {
  def meta = SubSubRecord

  object name extends StringField(this, 12)
}
object SubSubRecord extends SubSubRecord with BsonMetaRecord[SubSubRecord] {
  override def formats = allFormats
}

class SubRecordTestRecord private () extends MongoRecord[SubRecordTestRecord] with ObjectIdPk[SubRecordTestRecord] {
  def meta = SubRecordTestRecord

  object mandatoryBsonRecordField extends BsonRecordField(this, SubRecord)
  object legacyOptionalBsonRecordField extends BsonRecordField(this, SubRecord) {
    override def optional_? = true
  }

  object mandatoryBsonRecordListField extends BsonRecordListField(this, SubRecord)
  object legacyOptionalBsonRecordListField extends BsonRecordListField(this, SubRecord) {
    override def optional_? = true
  }
}
object SubRecordTestRecord extends SubRecordTestRecord with MongoMetaRecord[SubRecordTestRecord] {
  override def formats = allFormats
}

case class JsonObj(id: String, name: String) extends JsonObject[JsonObj] {
  def meta = JsonObj
}
object JsonObj extends JsonObjectMeta[JsonObj]

class NullTestRecord private () extends MongoRecord[NullTestRecord] with IntPk[NullTestRecord] {

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

class BoxTestRecord private () extends MongoRecord[BoxTestRecord] with LongPk[BoxTestRecord] {
  def meta = BoxTestRecord

  object jsonobj extends JsonObjectField[BoxTestRecord, BoxTestJsonObj](this, BoxTestJsonObj) {
    def defaultValue = BoxTestJsonObj("0", Empty, Full("Full String"), Failure("Failure"))
  }
  object jsonobjlist extends MongoJsonObjectListField[BoxTestRecord, BoxTestJsonObj](this, BoxTestJsonObj)

}
object BoxTestRecord extends BoxTestRecord with MongoMetaRecord[BoxTestRecord] {
  override def formats = super.formats + new JsonBoxSerializer
}

/*
 * MongoRefFields
 */
class RefFieldTestRecord private () extends MongoRecord[RefFieldTestRecord] with ObjectIdPk[RefFieldTestRecord] {
  def meta = RefFieldTestRecord

  object mandatoryObjectIdRefField extends ObjectIdRefField(this, FieldTypeTestRecord)
  object mandatoryUUIDRefField extends UUIDRefField(this, ListTestRecord)
  object mandatoryStringRefField extends StringRefField(this, MapTestRecord, 100)
  object mandatoryIntRefField extends IntRefField(this, NullTestRecord)
  object mandatoryLongRefField extends LongRefField(this, BoxTestRecord)

  object mandatoryObjectIdRefListField extends ObjectIdRefListField(this, FieldTypeTestRecord)
  object mandatoryUUIDRefListField extends UUIDRefListField(this, ListTestRecord)
  object mandatoryStringRefListField extends StringRefListField(this, MapTestRecord)
  object mandatoryIntRefListField extends IntRefListField(this, NullTestRecord)
  object mandatoryLongRefListField extends LongRefListField(this, BoxTestRecord)
}

object RefFieldTestRecord extends RefFieldTestRecord with MongoMetaRecord[RefFieldTestRecord] {
  override def formats = allFormats
}


class JObjectFieldTestRecord private () extends MongoRecord[JObjectFieldTestRecord]  with ObjectIdPk[JObjectFieldTestRecord] {
  def meta = JObjectFieldTestRecord

  object mandatoryJObjectField extends JObjectField(this)
}

object JObjectFieldTestRecord extends JObjectFieldTestRecord with MongoMetaRecord[JObjectFieldTestRecord] {
  override def formats = allFormats
}

class CustomFieldName private () extends MongoRecord[CustomFieldName] with ObjectIdPk[CustomFieldName] {
  def meta = CustomFieldName

  object customField extends StringField(this, 256)
}

object CustomFieldName extends CustomFieldName with MongoMetaRecord[CustomFieldName]
