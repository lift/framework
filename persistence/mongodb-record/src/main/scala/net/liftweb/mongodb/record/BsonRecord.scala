/*
 * Copyright 2011-2013 WorldWide Conferencing, LLC
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

import common._

import java.util.regex.Pattern
import scala.collection.JavaConversions._

import net.liftweb.record.{Field, MetaRecord, Record}
import net.liftweb.record.field._

import com.mongodb._
import java.util.prefs.BackingStoreException

/** Specialized Record that can be encoded and decoded from BSON (DBObject) */
trait BsonRecord[MyType <: BsonRecord[MyType]] extends Record[MyType] {
  self: MyType =>

  /** Refines meta to require a BsonMetaRecord */
  def meta: BsonMetaRecord[MyType]

  /**
    * Encode a record instance into a DBObject
    */
  def asDBObject: DBObject = meta.asDBObject(this)

  /**
    * Set the fields of this record from the given DBObject
    */
  def setFieldsFromDBObject(dbo: DBObject): Unit = meta.setFieldsFromDBObject(this, dbo)

 /**
   * Save the instance and return the instance
   */
  override def saveTheRecord(): Box[MyType] = throw new BackingStoreException("BSON Records don't save themselves")

  /**
    * Pattern.equals doesn't work properly so it needs a special check. If you use PatternField, be sure to override equals with this.
    */
  protected def equalsWithPatternCheck(other: Any): Boolean = {
    other match {
      case that: BsonRecord[MyType] =>
        that.fields.corresponds(this.fields) { (a,b) =>
          (a.name == b.name) && ((a.valueBox, b.valueBox) match {
            case (Full(ap: Pattern), Full(bp: Pattern)) => ap.pattern == bp.pattern && ap.flags == bp.flags
            case _ => a.valueBox == b.valueBox
          })
        }
      case _ => false
    }
  }
}

/** Specialized MetaRecord that deals with BsonRecords */
trait BsonMetaRecord[BaseRecord <: BsonRecord[BaseRecord]] extends MetaRecord[BaseRecord] with JsonFormats {
  self: BaseRecord =>

  /**
    * Create a BasicDBObject from the field names and values.
    * - MongoFieldFlavor types (List) are converted to DBObjects
    *   using asDBObject
    */
  def asDBObject(inst: BaseRecord): DBObject = {
    val dbo = BasicDBObjectBuilder.start // use this so regex patterns can be stored.

    for {
      field <- fields(inst)
      dbValue <- fieldDbValue(field)
    } { dbo.add(field.name, dbValue) }

    dbo.get
  }

  /**
    * Return the value of a field suitable to be put in a DBObject
    */
  def fieldDbValue(f: Field[_, BaseRecord]): Box[Any] = {
    import Meta.Reflection._
    import field.MongoFieldFlavor

    f match {
      case field if (field.optional_? && field.valueBox.isEmpty) => Empty // don't add to DBObject
      case field: EnumTypedField[_] =>
        field.asInstanceOf[EnumTypedField[Enumeration]].valueBox map {
          v => v.id
        }
      case field: EnumNameTypedField[_] =>
        field.asInstanceOf[EnumNameTypedField[Enumeration]].valueBox map {
          v => v.toString
        }
      case field: MongoFieldFlavor[_] =>
        Full(field.asInstanceOf[MongoFieldFlavor[Any]].asDBObject)
      case field => field.valueBox map (_.asInstanceOf[AnyRef] match {
        case null => null
        case x if primitive_?(x.getClass) => x
        case x if mongotype_?(x.getClass) => x
        case x if datetype_?(x.getClass) => datetype2dbovalue(x)
        case x: BsonRecord[_] => x.asDBObject
        case x: Array[Byte] => x
        case o => o.toString
      })
    }
  }

  /**
    * Creates a new record, then sets the fields with the given DBObject.
    *
    * @param dbo - the DBObject
    * @return Box[BaseRecord]
    */
  def fromDBObject(dbo: DBObject): BaseRecord = {
    val inst: BaseRecord = createRecord
    setFieldsFromDBObject(inst, dbo)
    inst
  }

  /**
    * Populate the inst's fields with the values from a DBObject. Values are set
    * using setFromAny passing it the DBObject returned from Mongo.
    *
    * @param inst - the record that will be populated
    * @param dbo - The DBObject
    * @return Unit
    */
  def setFieldsFromDBObject(inst: BaseRecord, dbo: DBObject): Unit = {
    for (k <- dbo.keySet; field <- inst.fieldByName(k.toString)) {
      field.setFromAny(dbo.get(k.toString))
    }
    inst.runSafe {
      inst.fields.foreach(_.resetDirty)
    }
  }
}
