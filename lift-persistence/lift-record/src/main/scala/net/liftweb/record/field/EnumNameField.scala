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

import scala.reflect.Manifest
import scala.xml._
import net.liftweb.common._
import net.liftweb.http.js._
import net.liftweb.http.{S, SHtml}
import net.liftweb.json.JsonAST.{JInt, JNothing, JNull, JString, JValue}
import net.liftweb.util._
import Box.option2Box
import S._
import Helpers._
import JE._


trait EnumNameTypedField[EnumType <: Enumeration] extends TypedField[EnumType#Value] {
  protected val enum: EnumType
  protected val valueManifest: Manifest[EnumType#Value]

  def setFromAny(in: Any): Box[EnumType#Value] = genericSetFromAny(in)(valueManifest)

  def setFromString(s: String): Box[EnumType#Value] = s match {
    case "" if optional_? => setBox(Empty)
    case _                => setBox(enum.values.find(_.toString == s))
  }

  /** Label for the selection item representing Empty, show when this field is optional. Defaults to the empty string. */
  def emptyOptionLabel: String = ""

  /**
   * Build a list of (value, label) options for a select list.  Return a tuple of (Box[String], String) where the first string
   * is the value of the field and the second string is the Text name of the Value.
   */
  def buildDisplayList: List[(Box[EnumType#Value], String)] = {
    val options = enum.map(a => (Full(a), a.toString)).toList
    if (optional_?) (Empty, emptyOptionLabel)::options else options
  }

  private def elem = SHtml.selectObj[Box[EnumType#Value]](buildDisplayList, Full(valueBox), setBox(_)) % ("tabindex" -> tabIndex.toString)

  def toForm: Box[NodeSeq]  =
    uniqueFieldId match {
      case Full(id) => Full(elem % ("id" -> (id + "_field")))
      case _ => Full(elem)
    }

  def defaultValue: EnumType#Value = enum.iterator.next

  def asJs = valueBox.map(_ => Str(toString)) openOr JsNull

  def asJStringName: JValue = valueBox.map(v => JString(v.toString)) openOr (JNothing: JValue)
  def setFromJStringName(jvalue: JValue): Box[EnumType#Value] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JString(s)                   => setBox(enum.values.find(_.toString == s) ?~ ("Unknown value \"" + s + "\""))
    case other                        => setBox(FieldHelpers.expectedA("JString", other))
  }

  def asJValue: JValue = asJStringName
  def setFromJValue(jvalue: JValue): Box[EnumType#Value] = setFromJStringName(jvalue)
}

class EnumNameField[OwnerType <: Record[OwnerType], EnumType <: Enumeration](rec: OwnerType, protected val enum: EnumType)(implicit m: Manifest[EnumType#Value])
  extends Field[EnumType#Value, OwnerType] with MandatoryTypedField[EnumType#Value] with EnumNameTypedField[EnumType]
{
  def this(rec: OwnerType, enum: EnumType, value: EnumType#Value)(implicit m: Manifest[EnumType#Value]) = {
    this(rec, enum)
    set(value)
  }

  def owner = rec
  protected val valueManifest = m
}

class OptionalEnumNameField[OwnerType <: Record[OwnerType], EnumType <: Enumeration](rec: OwnerType, protected val enum: EnumType)(implicit m: Manifest[EnumType#Value])
  extends Field[EnumType#Value, OwnerType] with OptionalTypedField[EnumType#Value] with EnumNameTypedField[EnumType]
{
  def this(rec: OwnerType, enum: EnumType, value: Box[EnumType#Value])(implicit m: Manifest[EnumType#Value]) = {
    this(rec, enum)
    setBox(value)
  }

  def owner = rec
  protected val valueManifest = m
}

}
}
}
