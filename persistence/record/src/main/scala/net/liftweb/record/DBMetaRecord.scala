/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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
package record {

import _root_.net.liftweb._
import util._
import common._
import _root_.scala.xml._
import _root_.net.liftweb.mapper.{ConnectionIdentifier, Safe, DefaultConnectionIdentifier, DB, KeyObfuscator, SuperConnection, QueryParam}
import _root_.java.sql.{ResultSet, Types, PreparedStatement, Statement}

trait DBMetaRecord[BaseRecord <: DBRecord[BaseRecord]] extends MetaRecord[BaseRecord] {
  self: BaseRecord =>

  /**
   * Save the instance in the appropriate backing store
   */
  def save(inst: BaseRecord): Boolean = {
    foreachCallback(inst, _.beforeSave)
    try {
      true // TODO: implement this
    } finally {
      foreachCallback(inst, _.afterSave)
    }
  }

  /**
   * Was this instance saved in backing store?
   */
  def saved_?(inst: BaseRecord): Boolean = true

  /**
   * Delete the instance from backing store
   */
  def delete_!(inst: BaseRecord): Boolean = {
    foreachCallback(inst, _.beforeDelete)
    try {
      true // TODO: implement this
    } finally {
      foreachCallback(inst, _.afterDelete)
    }
  }

  def dbDefaultConnectionIdentifier: ConnectionIdentifier = DefaultConnectionIdentifier

  def afterCommit: List[BaseRecord => Unit] = Nil

  // To be continued with DB related stuff
}

}
}
