/*
 * Copyright 2020 WorldWide Conferencing, LLC
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

package net.liftweb.mongodb
package codecs

import org.bson.BsonType

/**
 * A companion object for BsonTypeClassMap.
 */
object BsonTypeClassMap {
  def apply(replacements: (BsonType, Class[_])*): BsonTypeClassMap = {
    val jreplacements = new java.util.HashMap[BsonType, Class[_]]()
    replacements.foreach(kv => jreplacements.put(kv._1, kv._2))
    new BsonTypeClassMap(jreplacements)
  }
}