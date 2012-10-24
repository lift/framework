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
package field

import common._
import http.{S, SHtml}
import net.liftweb.record.{Field, MandatoryTypedField, TypedField}

import java.util.UUID

import org.bson.types.ObjectId

/*
 * Trait for creating a Field for storing a list of "foreign keys". Caches the
 * items after fetching. Implementations are available for ObjectId, UUID, String,
 * Int, and Long, but you can extend this.
 *
 * toForm produces a multi-select form element. You just need to supply the
 * options by overriding the options method.
 */
abstract class MongoRefListField[OwnerType <: BsonRecord[OwnerType], RefType <: MongoRecord[RefType], MyType]
  (rec: OwnerType)(implicit mf: Manifest[MyType]) extends MongoListField[OwnerType, MyType](rec) {

  /** The MongoMetaRecord of the referenced object **/
  def refMeta: MongoMetaRecord[RefType]

  /**
    * Find the referenced objects
    */
  def findAll = refMeta.findAllByList(this.value)

  /*
   * get the referenced objects
   */
  def objs = synchronized {
    if (!_calcedObjs) {
      _calcedObjs = true
      this._objs = findAll
    }
    _objs
  }

  def cached_? : Boolean = synchronized { _calcedObjs }

  def primeObjs(objs: List[RefType]) = synchronized {
    _objs = objs
    _calcedObjs = true
  }

  private var _objs: List[RefType] = Nil
  private var _calcedObjs = false

  override def setBox(in: Box[MyType]): Box[MyType] = synchronized {
    _calcedObjs = false // invalidate the cache
    super.setBox(in)
  }
}

class ObjectIdRefListField[OwnerType <: BsonRecord[OwnerType], RefType <: MongoRecord[RefType]](
  rec: OwnerType, val refMeta: MongoMetaRecord[RefType]
) extends MongoRefListField[OwnerType, RefType, ObjectId](rec) {}

class UUIDRefListField[OwnerType <: BsonRecord[OwnerType], RefType <: MongoRecord[RefType]](
  rec: OwnerType, val refMeta: MongoMetaRecord[RefType]
) extends MongoRefListField[OwnerType, RefType, UUID](rec) {}

class StringRefListField[OwnerType <: BsonRecord[OwnerType], RefType <: MongoRecord[RefType]](
  rec: OwnerType, val refMeta: MongoMetaRecord[RefType]
) extends MongoRefListField[OwnerType, RefType, String](rec) {}

class IntRefListField[OwnerType <: BsonRecord[OwnerType], RefType <: MongoRecord[RefType]](
  rec: OwnerType, val refMeta: MongoMetaRecord[RefType]
) extends MongoRefListField[OwnerType, RefType, Int](rec) {}

class LongRefListField[OwnerType <: BsonRecord[OwnerType], RefType <: MongoRecord[RefType]](
  rec: OwnerType, val refMeta: MongoMetaRecord[RefType]
) extends MongoRefListField[OwnerType, RefType, Long](rec) {}
