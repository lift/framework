/*
 * Copyright 2010-2011 WorldWide Conferencing, LLC
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

import net.liftweb.record.{MetaRecord, Record}

import com.mongodb.{BasicDBObject, DBObject, DBRef, WriteConcern}

import org.bson.types.ObjectId
import common.{Full, Box}

trait MongoRecord[MyType <: MongoRecord[MyType]] extends BsonRecord[MyType] {
  self: MyType =>

  /*
  * Every MongoRecord must have an _id field. Use a MongoPkField to
  * satisfy this.

  * This may change to type MandatoryTypedField in the
  * future (once MongoId is removed.)
  */
  def id: Any

  /**
  * The meta record (the object that contains the meta result for this type)
  */
  def meta: MongoMetaRecord[MyType]

  /**
  * Save the instance and return the instance
  */
  def save(concern: WriteConcern): MyType = {
    runSafe {
      meta.save(this, concern)
    }
    this
  }


 /**
  * Save the instance and return the instance
  */
  override def saveTheRecord(): Box[MyType] = {save; Full(this)}

  /**
  * Save the instance and return the instance
  * @param safe - if true will use WriteConcern SAFE else NORMAL
  */
  def save(safe: Boolean): MyType = {
    save(if (safe) WriteConcern.SAFE else WriteConcern.NORMAL)
  }

  /**
  * Save the instance and return the instance
  * WILL NOT RAISE MONGO SERVER ERRORS.
  * Use save(Boolean) or save(WriteConcern) to control error behavior
  */
  def save: MyType = save(false)

  /**
    * Update only the dirty fields
    */
  def update: MyType = {
    runSafe {
      meta.update(this)
    }
    this
  }

  /**
  * Delete the instance from backing store
  */
  def delete_! : Boolean = {
    runSafe {
      meta.delete_!(this)
    }
  }
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
