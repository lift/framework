/*
 * Copyright 2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package squerylrecord {

import _root_.net.liftweb.record.MandatoryTypedField
import _root_.org.squeryl.IndirectKeyedEntity

/**
 * Trait to mix into records that have a primary key. The primary key field must be named "idField", though
 * the name of the database column can be changed from that using @Column(name="id") or similar.
 */
trait KeyedRecord[K] extends IndirectKeyedEntity[K, MandatoryTypedField[K]] {
  /** The primary key field of the record. Must not be optional. */
  def idField: MandatoryTypedField[K]

  /** Implement requirements of KeyedEntity by returning the current value of idField */
  def id = idField.value
}

}
}
