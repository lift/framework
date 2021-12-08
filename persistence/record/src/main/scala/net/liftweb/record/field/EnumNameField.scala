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

import reflect.Manifest
import scala.xml._

import common._
import Box.option2Box
import json._
import util._
import http.js._
import http.{S, SHtml}
import S._
import Helpers._
import JE._


trait EnumNameTypedField[EnumType <: Enumeration] extends TypedField[EnumType#Value] {
  protected val e: EnumType
  protected val valueManifest: Manifest[EnumType#Value]

  def setFromAny(in: Any): Box[EnumType#Value] = genericSetFromAny(in)(valueManifest)

  def setFromString(s: String): Box[EnumType#Value] = s match {
    case null|"" if optional_? => setBox(Empty)
    case null|"" => setBox(Failure(notOptionalErrorMessage))
    case _ => setBox(e.values.find(_.toString == s))
  }

  /** Label for the selection item representing Empty, show when this field is optional. Defaults to the empty string. */
  def emptyOptionLabel: String = ""

  /**
   * Build a list of (value, label) options for a select list.  Return a tuple of (Box[String], String) where the first string
   * is the value of the field and the second string is the Text name of the Value.
   */
  def buildDisplayList: List[(Box[EnumType#Value], String)] = {
    val options = e.values.toList.map(a => (Full(a), a.toString))
    if (optional_?) (Empty, emptyOptionLabel)::options else options
  }

  private def elem = SHtml.selectObj[Box[EnumType#Value]](buildDisplayList, Full(valueBox), setBox(_)) % ("tabindex" -> tabIndex.toString)

  def toForm: Box[NodeSeq]  =
    uniqueFieldId match {
      case Full(id) => Full(elem % ("id" -> id))
      case _ => Full(elem)
    }

  def defaultValue: EnumType#Value = e.values.iterator.next()

  def asJs = valueBox.map(_ => Str(toString)) openOr JsNull

  def asJStringName: JValue = valueBox.map(v => JString(v.toString)) openOr (JNothing: JValue)
  def setFromJStringName(jvalue: JValue): Box[EnumType#Value] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JString(s)                   => setBox(e.values.find(_.toString == s) ?~ ("Unknown value \"" + s + "\""))
    case other                        => setBox(FieldHelpers.expectedA("JString", other))
  }

  def asJValue: JValue = asJStringName
  def setFromJValue(jvalue: JValue): Box[EnumType#Value] = setFromJStringName(jvalue)
}

@scala.annotation.nowarn("msg=The parameter name should be a String, not a symbol.")
class EnumNameField[OwnerType <: Record[OwnerType], EnumType <: Enumeration](@deprecatedName(Symbol("rec")) val owner: OwnerType,
  protected val e: EnumType)(implicit m: Manifest[EnumType#Value]
) extends Field[EnumType#Value, OwnerType] with MandatoryTypedField[EnumType#Value] with EnumNameTypedField[EnumType] {

  def this(@deprecatedName(Symbol("rec")) owner: OwnerType, e: EnumType, value: EnumType#Value)(implicit m: Manifest[EnumType#Value]) = {
    this(owner, e)
    set(value)
  }

  protected val valueManifest = m
}

@scala.annotation.nowarn("msg=The parameter name should be a String, not a symbol.")
class OptionalEnumNameField[OwnerType <: Record[OwnerType], EnumType <: Enumeration](@deprecatedName(Symbol("rec")) val owner: OwnerType,
  protected val e: EnumType)(implicit m: Manifest[EnumType#Value]
) extends Field[EnumType#Value, OwnerType] with OptionalTypedField[EnumType#Value] with EnumNameTypedField[EnumType] {

  def this(@deprecatedName(Symbol("rec")) owner: OwnerType, e: EnumType, value: Box[EnumType#Value])(implicit m: Manifest[EnumType#Value]) = {
    this(owner, e)
    setBox(value)
  }

  protected val valueManifest = m
}

