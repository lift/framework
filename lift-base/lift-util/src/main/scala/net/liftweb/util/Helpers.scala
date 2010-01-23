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
package util {

import _root_.scala.xml._

/**
 * The Helpers object provides a lot of utility functions:<ul>
 * <li>Time and date
 * <li>URL
 * <li>Hash generation
 * <li>Class instantiation
 * <li>Control abstractions
 * <li>Basic types conversions
 * <li>XML bindings
 * </ul>
 */

object Helpers extends TimeHelpers with StringHelpers with ListHelpers
with SecurityHelpers with BindHelpers with HttpHelpers
with IoHelpers with BasicTypesHelpers
with ClassHelpers with ControlHelpers
{

}

/**
 * Used for type-safe pattern matching of an Any and returns a Seq[Node]
 */
object SafeNodeSeq {
  // I didn't use unapplySeq as I ran into a compiler(2.7.1 final) crash at LiftRules#convertResponse.
  // I opened the scala ticket https://lampsvn.epfl.ch/trac/scala/ticket/1059#comment:1
  def unapply(any: Any) : Option[Seq[Node]] = any match {
    case s: Seq[_] =>  Some(s flatMap ( _ match {
            case n: Node => n
            case _ => NodeSeq.Empty
          }))
    case _ => None
  }
}

}
}
