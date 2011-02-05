/*
 * Copyright 2010 WorldWide Conferencing, LLC
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
package mongodb {
package record {
package field {

import scala.xml.{Node, NodeSeq, Text}

import net.liftweb.common.{Box, Empty, Failure, Full}
import net.liftweb.http.S
import net.liftweb.http.js.JE._
import net.liftweb.mapper.Safe
import net.liftweb.util.{FatLazy, FieldError, Helpers}

import Helpers._

case class Password(pwd: String, salt: String) extends JsonObject[Password] {
  def meta = Password
}

object Password extends JsonObjectMeta[Password] {
  def apply(in: String): Password = Password(in, "")
}

object MongoPasswordField {
  val blankPw = "*******"

  def encrypt(s: String, salt: String) = hash("{"+s+"} salt={" + salt + "}")
}

class MongoPasswordField[OwnerType <: MongoRecord[OwnerType]](rec: OwnerType, minLen: Int) extends JsonObjectField[OwnerType, Password](rec, Password) {

  def this(rec: OwnerType) = {
    this(rec, 3)
  }

  def setPassword(in: String) = set(Password(in))

  private val salt_i = FatLazy(Safe.randomString(16))

  var validatorValue: Box[Password] = valueBox

  override def set_!(in: Box[Password]): Box[Password] = {
    validatorValue = in
    in.map(p =>
      if (p.salt.length == 0) // only encrypt the password if it hasn't already been encrypted
        Password(MongoPasswordField.encrypt(p.pwd, salt_i.get), salt_i.get)
      else
        p
    )
  }

  override def validate: List[FieldError] = runValidation(validatorValue)

  private def elem = S.fmapFunc(S.SFuncHolder(this.setPassword(_))) {
    funcName => <input type="password"
      name={funcName}
      value=""
      tabindex={tabIndex toString}/>}

  override def toForm: Box[NodeSeq] =
    uniqueFieldId match {
      case Full(id) => Full(elem % ("id" -> (id+"_field")))
      case _ => Full(elem)
    }

  private def validatePassword(pwd: Password): List[FieldError] = pwd match {
    case null | Password("", _) | Password("*", _) | Password(MongoPasswordField.blankPw, _) =>
      Text(S.??("password.must.be.set"))
    case Password(pwd, _) if pwd.length < minLen =>
      Text(S.??("password.too.short"))
    case _ => Nil
  }

  override def validations = validatePassword _ :: Nil

  override def defaultValue = Password("")

  override def asJs = valueBox.map(vb => Str(vb.pwd)) openOr Str(defaultValue.pwd)

  def isMatch(toMatch: String): Boolean =
    MongoPasswordField.encrypt(toMatch, value.salt) == value.pwd
}

}
}
}
}
