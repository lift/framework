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

import net.liftweb.common.{Box, Full}
import net.liftweb.record.{MetaRecord, Record}

import com.mongodb.{BasicDBObject, DBObject, DBRef}

import org.bson.types.ObjectId

trait MongoRecord[MyType <: MongoRecord[MyType]] extends Record[MyType] {
  self: MyType =>

  /*
  * every mongo record must have an _id field. Override this with the value of your _id object.
  */
  def id: Any

  /**
  * The meta record (the object that contains the meta result for this type)
  */
  def meta: MongoMetaRecord[MyType]

  /**
  * Save the instance and return the instance
  */
  def save(strict: Boolean): MyType = {
    runSafe {
      meta.save(this, strict)
    }
    this
  }

  def save: MyType = save(false)

  /**
  * Delete the instance from backing store
  */
  def delete_! : Boolean = {
    runSafe {
      meta.delete_!(this)
    }
  }

/* Set mongoIdentifier in meta object. No need to support calcDbId for sharding
  private var dbMongoIdentifier: Option[MongoIdentifier] = None

  def mongoIdentifier = dbMongoIdentifier getOrElse calcDbId

  def dbCalculateMongoIdentifier: PartialFunction[MyType, MongoIdentifier] = Map.empty

  private def calcDbId = if (dbCalculateMongoIdentifier.isDefinedAt(this)) dbCalculateMongoIdentifier(this)
                        else meta.dbDefaultMongoIdentifier
*/

  /**
  * Append a function to perform after the commit happens
  * @param func - the function to perform after the commit happens

  def doPostCommit(func: () => Unit) {
    //DB.appendPostFunc(connectionIdentifier, func)
  }
*/

  /**
  * Encode a record instance into a DBObject
  */
  def asDBObject: DBObject = meta.asDBObject(this)

  /**
  * Set the fields of this record from the given DBObject
  */
  def setFieldsFromDBObject(dbo: DBObject): Unit = meta.setFieldsFromDBObject(this, dbo)
}

/**
* Mix this into a Record to add an ObjectIdField
*/
trait MongoId[OwnerType <: MongoRecord[OwnerType]] {
  self: OwnerType =>

  import field.ObjectIdField

  object _id extends ObjectIdField(this.asInstanceOf[OwnerType])

  // convenience method that returns the value of _id
  def id = _id.value

  /*
  * Get the DBRef for this record
  */
  def getRef: DBRef = {
    MongoDB.use(meta.mongoIdentifier) ( db =>
      new DBRef(db, meta.collectionName, _id.value)
    )
  }
}

}
}
}
