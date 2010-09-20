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
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.mapper.{Safe}
import _root_.net.liftweb.http.{S}
import _root_.java.util.regex._
import _root_.java.util.regex.{Pattern => RegexPattern}
import Helpers._
import S._


trait PostalCodeTypedField extends StringTypedField {
  protected val country: CountryField[_]

  override def setFilter = toUpper _ :: trim _ :: super.setFilter

  override def validations = validatePostalCode _ :: Nil

  def validatePostalCode(in: ValueType): List[FieldError] = country.value match {
    case Countries.USA       => valRegex(RegexPattern.compile("[0-9]{5}(\\-[0-9]{4})?"), S.??("invalid.zip.code"))(in)
    case Countries.Sweden    => valRegex(RegexPattern.compile("[0-9]{3}[ ]?[0-9]{2}"), S.??("invalid.postal.code"))(in)
    case Countries.Australia => valRegex(RegexPattern.compile("(0?|[1-9])[0-9]{3}"), S.??("invalid.postal.code"))(in)
    case Countries.Canada    => valRegex(RegexPattern.compile("[A-Z][0-9][A-Z][ ][0-9][A-Z][0-9]"), S.??("invalid.postal.code"))(in)
    case _ => genericCheck(in)
  }

  private def genericCheck(zip: ValueType): List[FieldError] = {
    toBoxMyType(zip) flatMap {
      case null => Full(Text(S.??("invalid.postal.code")))
      case s if s.length < 3 => Full(Text(S.??("invalid.postal.code")))
      case _ => Empty
    }
  }
}

class PostalCodeField[OwnerType <: Record[OwnerType]](rec: OwnerType, val country: CountryField[OwnerType])
extends StringField(rec, 32) with PostalCodeTypedField

class OptionalPostalCodeField[OwnerType <: Record[OwnerType]](rec: OwnerType, val country: CountryField[OwnerType])
extends OptionalStringField(rec, 32) with PostalCodeTypedField

}
}
}
