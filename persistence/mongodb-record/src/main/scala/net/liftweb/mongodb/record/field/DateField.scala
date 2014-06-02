/*
* Copyright 2010-2014 WorldWide Conferencing, LLC
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

package net.liftweb
package mongodb
package record
package field

import java.util.Date

import scala.xml.NodeSeq

import common._
import http.js.JE.{JsNull, JsRaw}
import http.S
import json._
import mongodb.record._
import net.liftweb.record._
import net.liftweb.record.field._
import util.Helpers._

trait DateTypedField extends TypedField[Date] {

  def formats: Formats

  def setFromAny(in: Any): Box[Date] = in match {
    case d: Date => setBox(Full(d))
    case Some(d: Date) => setBox(Full(d))
    case Full(d: Date) => setBox(Full(d))
    case (d: Date) :: _ => setBox(Full(d))
    case s: String => setFromString(s)
    case Some(s: String) => setFromString(s)
    case Full(s: String) => setFromString(s)
    case null|None|Empty => setBox(defaultValueBox)
    case f: Failure => setBox(f)
    case o => setFromString(o.toString)
  }

  def setFromString(in: String): Box[Date] = formats.dateFormat.parse(in) match {
    case Some(d: Date) => setBox(Full(d))
    case other => setBox(Failure("Invalid Date string: "+in))
  }

  def setFromJValue(jvalue: JValue): Box[Date] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JObject(JField("$dt", JString(s)) :: Nil) => setFromString(s)
    case other => setBox(FieldHelpers.expectedA("JObject", other))
  }

  private def elem =
    S.fmapFunc(S.SFuncHolder(this.setFromAny(_))){funcName =>
      <input type="text"
        name={funcName}
        value={valueBox.map(v => formats.dateFormat.format(v)) openOr ""}
        tabindex={tabIndex.toString}/>
    }

  def toForm =
    uniqueFieldId match {
      case Full(id) => Full(elem % ("id" -> id))
      case _ => Full(elem)
    }

  def asJs = asJValue match {
    case JNothing => JsNull
    case jv => JsRaw(Printer.compact(render(jv)))
  }

  def asJValue: JValue = valueBox.map(v => JsonDate(v)(formats)) openOr (JNothing: JValue)
}

class DateField[OwnerType <: BsonRecord[OwnerType]](rec: OwnerType)
  extends Field[Date, OwnerType] with MandatoryTypedField[Date] with DateTypedField {

  def owner = rec

  def formats = owner.meta.formats

  def this(rec: OwnerType, value: Date) = {
    this(rec)
    setBox(Full(value))
  }

  def defaultValue = new Date

  override def toString = value match {
    case null => "null"
    case d => valueBox.map {
      v => formats.dateFormat.format(v)
    } openOr ""
  }
}

class OptionalDateField[OwnerType <: BsonRecord[OwnerType]](rec: OwnerType)
  extends Field[Date, OwnerType] with OptionalTypedField[Date] with DateTypedField {

  def owner = rec

  def formats = owner.meta.formats

  def this(rec: OwnerType, value: Box[Date]) = {
    this(rec)
    setBox(value)
  }
}
