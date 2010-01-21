/*
 * Copyright 2008-2010 WorldWide Conferencing, LLC
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

/**
 * Holds the implicit conversions from/to MonadicCondition
 */
object MonadicConversions {

  implicit def bool2Monadic(cond: Boolean) = cond match {
    case true => True
    case _ => False(Nil)
  }

  implicit def monadic2Bool(cond: MonadicCondition): Boolean = cond match {
    case True => true
    case _ => false
  }

}

/**
 * A MonadicCondition allows building boolean expressions of the form
 * (a(0) && a(1) && .. && a(n)), where a(k) is a boolean expression, and
 * collecting the computation failures to a list of messages.
 *
 * <pre>
 * Example:
 *
 *   val isTooYoung = true;
 *   val isTooBad = false;
 *   val isTooStupid = true;
 *
 *   val exp = (!isTooYoung ~ "too young") &&
 *             (!isTooBad ~ "too bad") &&
 *             (!isTooStupid ~ "too stupid")
 *
 *   println(exp match {
 *     case False(msgs) => msgs mkString("Test failed because it is '", "' and '", "'.")
 *     case _ => "success"
 *   })
 * </pre>
 */
trait MonadicCondition {
  def && (cond: MonadicCondition): MonadicCondition
  def ~ (msg: String): MonadicCondition
}

case object True extends MonadicCondition {
  def && (cond: MonadicCondition): MonadicCondition = cond match {
    case f @ False(m) => f
    case _ => this
  }
  def ~ (msg: String): MonadicCondition = this
}

case class False(msgs: List[String]) extends MonadicCondition {
  def && (cond: MonadicCondition): MonadicCondition = cond match {
    case False(m) => False(m ::: msgs)
    case _ => this
  }
  def ~ (msg: String): MonadicCondition = False(msg :: msgs)
}

}
}
