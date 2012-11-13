/*
 * Copyright 2007-2011 WorldWide Conferencing, LLC
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
package record
package field

import scala.xml._
import net.liftweb.common._
import net.liftweb.http.{S}
import net.liftweb.http.js._
import net.liftweb.json.JsonAST.JValue
import net.liftweb.util._
import Helpers._
import S._
import JE._


trait BinaryTypedField extends TypedField[Array[Byte]] {
  
  def setFromAny(in: Any): Box[Array[Byte]] = genericSetFromAny(in)

  def setFromString(s: String): Box[Array[Byte]] = s match {
    case null|"" if optional_? => setBox(Empty)
    case null|"" => setBox(Failure(notOptionalErrorMessage))
    case _ => setBox(tryo(s.getBytes("UTF-8")))
  }

  def toForm: Box[NodeSeq] = Empty

  def asJs = valueBox.map(v => Str(hexEncode(v))) openOr JsNull

  def asJValue = asJString(base64Encode _)
  def setFromJValue(jvalue: JValue) = setFromJString(jvalue)(s => tryo(base64Decode(s)))
}
  
class BinaryField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends Field[Array[Byte], OwnerType] with MandatoryTypedField[Array[Byte]] with BinaryTypedField {

  def owner = rec

  def this(rec: OwnerType, value: Array[Byte]) = {
    this(rec)
    set(value)
  }

  def defaultValue = Array(0)
}

class OptionalBinaryField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends Field[Array[Byte], OwnerType] with OptionalTypedField[Array[Byte]] with BinaryTypedField {

  def owner = rec

  def this(rec: OwnerType, value: Box[Array[Byte]]) = {
    this(rec)
    setBox(value)
  }
}

