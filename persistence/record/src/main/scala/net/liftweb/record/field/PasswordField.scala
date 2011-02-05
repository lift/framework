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

import _root_.scala.xml._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http.{S}
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.json.JsonAST.{JNothing, JNull, JString, JValue}
import _root_.net.liftweb.mapper.{Safe}
import _root_.net.liftweb.util._
import _root_.java.util.regex._
import Helpers._
import S._
import JE._

object PasswordField {
  @volatile var blankPw = "*******"
  @volatile var minPasswordLength = 5
}

trait PasswordTypedField extends TypedField[String] {
  private val salt_i = FatLazy(Safe.randomString(16))
  private var invalidMsg : String = ""

  def salt = this.salt_i

  private var validatedValue: Box[String] = valueBox

  def match_?(toTest: String): Boolean = 
    is == hash("{"+toTest+"} salt={"+salt_i.get+"}")

  override def set_!(in: Box[String]): Box[String] = {
    validatedValue = in
    in.map(s => hash("{"+s+"} salt={"+salt_i.get+"}"))
  }

  def setFromAny(in: Any): Box[String] = {
    in match {
      case (a: Array[String]) if (a.length == 2 && a(0)   == a(1)) => setBox(Full(a(0)))
      case (h1: String) :: (h2: String) :: Nil if h1 == h2 => setBox(Full(h1))
      case _ => genericSetFromAny(in)
    }
  }

  def setFromString(s: String): Box[String] = s match {
    case "" if optional_? => setBox(Empty)
    case _                => setBox(Full(s))
  }

  override def validate: List[FieldError] = runValidation(validatedValue)

  override def notOptionalErrorMessage = S.??("password.must.be.set")

  private def elem = S.fmapFunc(SFuncHolder(this.setFromAny(_))){
    funcName => <input type="password"
      name={funcName}
      value={valueBox openOr ""}
      tabindex={tabIndex toString}/>}

  def toForm: Box[NodeSeq] =
    uniqueFieldId match {
      case Full(id) => Full(elem % ("id" -> (id + "_field")))
      case _ => Full(elem)
    }

  protected def validatePassword(pwdValue: ValueType): List[FieldError] = 
    toBoxMyType(pwdValue) match {
      case Empty|Full(""|null) if !optional_? => Text(notOptionalErrorMessage)
      case Full(s) if s == "*" || s == PasswordField.blankPw || s.length < PasswordField.minPasswordLength => 
        Text(S.??("password.too.short"))
      case _ => Nil
    }

  override def validations = validatePassword _ :: Nil

  def defaultValue = ""

  def asJs = valueBox.map(Str) openOr JsNull

  def asJValue: JValue = valueBox.map(v => JString(v)) openOr (JNothing: JValue)
  def setFromJValue(jvalue: JValue): Box[MyType] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JString(s)                   => setFromString(s)
    case other                        => setBox(FieldHelpers.expectedA("JString", other))
  }
}

class PasswordField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends Field[String, OwnerType] with MandatoryTypedField[String] with PasswordTypedField {

  def this(rec: OwnerType, value: String) = {
    this(rec)
    set(value)
  }

  def owner = rec
}

class OptionalPasswordField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends Field[String, OwnerType] with OptionalTypedField[String] with PasswordTypedField {

  def this(rec: OwnerType, value: Box[String]) = {
    this(rec)
    setBox(value)
  }

  def owner = rec
}

}
}
}
