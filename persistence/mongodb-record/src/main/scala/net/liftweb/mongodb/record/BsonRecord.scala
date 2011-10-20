/*
 * Copyright 2011 WorldWide Conferencing, LLC
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

  override def toString = {
    val fieldList = this.fields.map(f => "%s=%s" format (f.name,
        f.valueBox match {
          case Full(c: java.util.Calendar) => c.getTime().toString()
          case Full(null) => ""
          case Full(v) => v.toString
          case _ => ""
        }))

    "%s={%s}" format (this.getClass.toString, fieldList.mkString(", "))
  }


 /**
  * Save the instance and return the instance
  */
  override def saveTheRecord(): Box[MyType] = throw new BackingStoreException("BSON Records don't save themselves")
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

    import Meta.Reflection._
    import field.MongoFieldFlavor

    val dbo = BasicDBObjectBuilder.start // use this so regex patterns can be stored.

    for (f <- fields(inst)) {
      f match {
        case field if (field.optional_? && field.valueBox.isEmpty) => // don't add to DBObject
        case field: EnumTypedField[Enumeration] =>
          field.asInstanceOf[EnumTypedField[Enumeration]].valueBox foreach {
            v => dbo.add(f.name, v.id)
          }
        case field: EnumNameTypedField[Enumeration] =>
          field.asInstanceOf[EnumNameTypedField[Enumeration]].valueBox foreach {
            v => dbo.add(f.name, v.toString)
          }
        case field: MongoFieldFlavor[Any] =>
          dbo.add(f.name, field.asInstanceOf[MongoFieldFlavor[Any]].asDBObject)
        case field => field.valueBox foreach (_.asInstanceOf[AnyRef] match {
          case null => dbo.add(f.name, null)
          case x if primitive_?(x.getClass) => dbo.add(f.name, x)
          case x if mongotype_?(x.getClass) => dbo.add(f.name, x)
          case x if datetype_?(x.getClass) => dbo.add(f.name, datetype2dbovalue(x))
          case x: BsonRecord[_] => dbo.add(f.name, x.asDBObject)
          case x: Array[Byte] => dbo.add(f.name, x)
          case o => dbo.add(f.name, o.toString)
        })
      }
    }
    dbo.get
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
