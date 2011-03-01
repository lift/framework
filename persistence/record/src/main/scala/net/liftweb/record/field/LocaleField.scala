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

import java.util.{Locale}
import xml._

import common._
import util._
import Helpers._
import http.{S, SHtml}
import S._


object LocaleField {
  lazy val localeList = Locale
    .getAvailableLocales.toList
    .sortWith(_.getDisplayName < _.getDisplayName)
    .map(lo => (lo.toString, lo.getDisplayName))
}

trait LocaleTypedField extends TypedField[String] {
  /** Build a list of string pairs for a select list. */
  def buildDisplayList: List[(String, String)]

  private def elem = SHtml.select(buildDisplayList, Full(valueBox.map(_.toString) openOr ""),
                                  locale => setBox(Full(locale))) % ("tabindex" -> tabIndex.toString)

  override def toForm: Box[NodeSeq] =
    uniqueFieldId match {
      case Full(id) => Full(elem % ("id" -> (id + "_field")))
      case _ => Full(elem)
    }
}

class LocaleField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends StringField(rec, 16) with LocaleTypedField {

  override def defaultValue = Locale.getDefault.toString

  def isAsLocale: Locale = Locale.getAvailableLocales.filter(_.toString == value).toList match {
    case Nil => Locale.getDefault
    case x :: xs => x
  }

  def buildDisplayList: List[(String, String)] = LocaleField.localeList

}

class OptionalLocaleField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends OptionalStringField(rec, 16) with LocaleTypedField {

  /** Label for the selection item representing Empty, show when this field is optional. Defaults to the empty string. */
  def emptyOptionLabel: String = ""

  def buildDisplayList: List[(String, String)] = ("", emptyOptionLabel)::LocaleField.localeList
}

