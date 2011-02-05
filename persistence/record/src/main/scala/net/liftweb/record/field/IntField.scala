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

import scala.xml._
import net.liftweb.common._
import net.liftweb.http.S
import net.liftweb.json.JsonAST.{JInt, JNothing, JNull, JValue}
import net.liftweb.util._
import Helpers._
import S._

trait IntTypedField extends NumericTypedField[Int] {
  def setFromAny(in: Any): Box[Int] = setNumericFromAny(in, _.intValue)

  def setFromString(s: String): Box[Int] = s match {
    case "" if optional_? => setBox(Empty)
    case _                => setBox(tryo(java.lang.Integer.parseInt(s)))
  }

  def defaultValue = 0

  def asJValue: JValue = valueBox.map(i => JInt(BigInt(i))) openOr (JNothing: JValue)
  def setFromJValue(jvalue: JValue): Box[Int] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JInt(i)                      => setBox(Full(i.intValue))
    case other                        => setBox(FieldHelpers.expectedA("JInt", other))
  }
}

class IntField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends Field[Int, OwnerType] with MandatoryTypedField[Int] with IntTypedField {

  def owner = rec

  def this(rec: OwnerType, value: Int) = {
    this(rec)
    set(value)
  }
}

class OptionalIntField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends Field[Int, OwnerType] with OptionalTypedField[Int] with IntTypedField {

  def owner = rec

  def this(rec: OwnerType, value: Box[Int]) = {
    this(rec)
    setBox(value)
  }
}

}
}
}
