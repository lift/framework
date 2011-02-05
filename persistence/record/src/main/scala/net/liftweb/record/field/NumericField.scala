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
package field {

import net.liftweb.http.{S}
import net.liftweb.http.js._
import net.liftweb.util._
import net.liftweb.common._
import scala.reflect.Manifest
import scala.xml._
import S._
import Helpers._
import JE._

trait NumericTypedField[MyType] extends TypedField[MyType] {

  /** Augments genericSetFromAny with support for values of type Number (optionally wrapped in any of the usual suspects) */
  protected final def setNumericFromAny(in: Any, f: Number => MyType)(implicit m: Manifest[MyType]): Box[MyType] =
    in match {
      case     (n: Number) => setBox(Full(f(n)))
      case Some(n: Number) => setBox(Full(f(n)))
      case Full(n: Number) => setBox(Full(f(n)))
      case (n: Number)::_  => setBox(Full(f(n)))
      case _ => genericSetFromAny(in)
    }

  private def elem = S.fmapFunc((s: List[String]) => setFromAny(s)) {
    funcName => <input type="text" name={funcName} value={valueBox.map(_.toString) openOr ""} tabindex={tabIndex toString}/>
  }

  /**
   * Returns form input of this field
   */
  def toForm: Box[NodeSeq] =
    uniqueFieldId match {
      case Full(id) => Full(elem % ("id" -> (id + "_field")))
      case _ => Full(elem)
    }

  override def noValueErrorMessage = S.??("number.required")

  def asJs = valueBox.map(v => JsRaw(String.valueOf(v))) openOr JsNull

}

}
}
}
