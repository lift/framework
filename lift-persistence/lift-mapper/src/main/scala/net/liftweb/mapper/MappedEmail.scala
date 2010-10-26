/*
 * Copyright 2006-2010 WorldWide Conferencing, LLC
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
package mapper {

import _root_.java.util.regex._
import _root_.scala.xml.Text
import _root_.net.liftweb.http.{S}
import _root_.net.liftweb.util.{FieldError}
import _root_.net.liftweb.proto._

object MappedEmail {
  def emailPattern = ProtoRules.emailRegexPattern.vend

  def validEmailAddr_?(email: String): Boolean = emailPattern.matcher(email).matches
}

abstract class MappedEmail[T<:Mapper[T]](owner: T, maxLen: Int) extends MappedString[T](owner, maxLen) {

  override def setFilter = notNull _ :: toLower _ :: trim _ :: super.setFilter

  override def validate =
    (if (MappedEmail.emailPattern.matcher(i_is_!).matches) Nil else List(FieldError(this, Text(S.??("invalid.email.address"))))) :::
    super.validate

}

}
}
