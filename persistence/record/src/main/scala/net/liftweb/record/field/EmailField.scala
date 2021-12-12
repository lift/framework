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
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.proto._
import net.liftweb.http.S

object EmailField {
  def emailPattern = ProtoRules.emailRegexPattern.vend

  def validEmailAddr_?(email: String): Boolean = emailPattern.matcher(email).matches
}

trait EmailTypedField extends TypedField[String] {
  private def validateEmail(emailValue: ValueType): List[FieldError] = {
    toBoxMyType(emailValue) match {
      case Full(email) if (optional_? && email.isEmpty) => Nil
      case Full(email) if EmailField.validEmailAddr_?(email) => Nil
      case _ => Text(S.?("invalid.email.address"))
    }
  }

  override def validations = validateEmail _ :: Nil
}

@scala.annotation.nowarn("msg=The parameter name should be a String, not a symbol.")
class EmailField[OwnerType <: Record[OwnerType]](@deprecatedName(Symbol("rec")) owner: OwnerType, maxLength: Int)
  extends StringField[OwnerType](owner, maxLength) with EmailTypedField

@scala.annotation.nowarn("msg=The parameter name should be a String, not a symbol.")
class OptionalEmailField[OwnerType <: Record[OwnerType]](@deprecatedName(Symbol("rec")) owner: OwnerType, maxLength: Int)
  extends OptionalStringField[OwnerType](owner, maxLength) with EmailTypedField

