/*
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
package util

import Helpers._
import common._

trait ConnectionIdentifier {
  def jndiName: String

  override def toString() = "ConnectionIdentifier(" + jndiName + ")"

  override def hashCode() = jndiName.hashCode()

  override def equals(other: Any): Boolean = other match {
    case ci: ConnectionIdentifier => ci.jndiName == this.jndiName
    case _ => false
  }
}

case object DefaultConnectionIdentifier extends ConnectionIdentifier {
  val jndiName = "lift"
}
