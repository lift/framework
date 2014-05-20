/**
  * Copyright 2014 WorldWide Conferencing, LLC
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

import org.bson.types.ObjectId

/**
  * Extend this to create extractors for your MongoRecords.
  *
  * Example:
  *    object AsUser extends AsMongoRecord(User)
  */
class AsMongoRecord[A <: MongoRecord[A]](meta: MongoMetaRecord[A]) {

  def unapply(in: String): Option[A] = asMongoRecord(in)

  def asMongoRecord(in: String): Option[A] =
    if (ObjectId.isValid(in)) meta.find(new ObjectId(in))
    else None
}
