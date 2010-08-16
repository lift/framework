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
import _root_.net.liftweb.json.JsonAST.JValue
import _root_.net.liftweb.util._
import _root_.java.util.{Calendar, Date}
import Helpers._
import S._
import JE._

trait DateTimeTypedField extends TypedField[Calendar] {
  private final def dateToCal(d: Date): Calendar = {
    val cal = Calendar.getInstance()
    cal.setTime(d)
    cal
  }

  def setFromAny(in : Any): Box[Calendar] = toDate(in).flatMap(d => setBox(Full(dateToCal(d)))) or genericSetFromAny(in)

  def setFromString(s: String): Box[Calendar] = s match {
    case "" if optional_? => setBox(Empty)
    case other            => setBox(tryo(dateToCal(parseInternetDate(s))))
  }

  private def elem =
    S.fmapFunc(SFuncHolder(this.setFromAny(_))){funcName =>
      <input type="text"
        name={funcName}
        value={valueBox.map(s => toInternetDate(s.getTime)) openOr ""}
        tabindex={tabIndex toString}/>
    }

  def toForm: Box[NodeSeq] =
    uniqueFieldId match {
      case Full(id) => Full(elem % ("id" -> (id + "_field")))
      case _        => Full(elem)
    }

  def asJs = valueBox.map(v => Str(toInternetDate(v.getTime))) openOr JsNull

  def asJValue = asJString(v => toInternetDate(v.getTime))
  def setFromJValue(jvalue: JValue) = setFromJString(jvalue) {
    v => boxParseInternetDate(v).map(d => {
      val cal = Calendar.getInstance
      cal.setTime(d)
      cal
    })
  }
}

class DateTimeField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends Field[Calendar, OwnerType] with MandatoryTypedField[Calendar] with DateTimeTypedField {

  def owner = rec

  def this(rec: OwnerType, value: Calendar) = {
    this(rec)
    setBox(Full(value))
  }

  def defaultValue = Calendar.getInstance
}

class OptionalDateTimeField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends Field[Calendar, OwnerType] with OptionalTypedField[Calendar] with DateTimeTypedField {

  def owner = rec

  def this(rec: OwnerType, value: Box[Calendar]) = {
    this(rec)
    setBox(value)
  }
}

}
}
}
