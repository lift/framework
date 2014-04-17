/*
 * Copyright 2007-2012 WorldWide Conferencing, LLC
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
import net.liftweb.common._
import net.liftweb.http.{S}
import net.liftweb.http.js._
import net.liftweb.json._
import net.liftweb.util._
import java.util.{Calendar, Date}
import Helpers._
import S._
import JE._

trait DateTimeTypedField extends TypedField[Calendar] {
  private final def dateToCal(d: Date): Calendar = {
    val cal = Calendar.getInstance()
    cal.setTime(d)
    cal
  }

  val formats = new DefaultFormats {
    override def dateFormatter = Helpers.internetDateFormatter
  }

  def setFromAny(in : Any): Box[Calendar] = toDate(in).flatMap(d => setBox(Full(dateToCal(d)))) or genericSetFromAny(in)

  def setFromString(s: String): Box[Calendar] = s match {
    case null|"" if optional_? => setBox(Empty)
    case null|"" => setBox(Failure(notOptionalErrorMessage))
    case other => setBox(tryo(dateToCal(parseInternetDate(s))))
  }

  private def elem =
    S.fmapFunc(SFuncHolder(this.setFromAny(_))){funcName =>
      <input type={formInputType}
        name={funcName}
        value={valueBox.map(s => toInternetDate(s.getTime)) openOr ""}
        tabindex={tabIndex.toString}/>
    }

  def toForm: Box[NodeSeq] =
    uniqueFieldId match {
      case Full(id) => Full(elem % ("id" -> id))
      case _        => Full(elem)
    }

  def asJs = valueBox.map(v => Str(formats.dateFormat.format(v.getTime))) openOr JsNull

  def asJValue = asJString(v => formats.dateFormat.format(v.getTime))
  def setFromJValue(jvalue: JValue) = setFromJString(jvalue) {
    v => formats.dateFormat.parse(v).map(d => {
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

