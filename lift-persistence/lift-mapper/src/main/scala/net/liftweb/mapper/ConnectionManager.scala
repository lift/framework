/*
 * Copyright 2006-2010 WorldWide Conferencing, LLC
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
package mapper {

import _root_.java.sql.Connection
import _root_.net.liftweb.common._

/**
 * Vend JDBC connections
 */
trait ConnectionManager {
  def newConnection(name: ConnectionIdentifier): Box[Connection]
  def releaseConnection(conn: Connection)
  def newSuperConnection(name: ConnectionIdentifier): Box[SuperConnection] = Empty
}

}
}
