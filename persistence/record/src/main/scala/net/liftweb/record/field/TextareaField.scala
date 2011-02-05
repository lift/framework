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

import scala.xml._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http.{S}
import S._
import Helpers._

trait TextareaTypedField extends StringTypedField {
  private def elem = S.fmapFunc(SFuncHolder(this.setFromAny(_))){
    funcName => <textarea name={funcName}
      rows={textareaRows.toString}
      cols={textareaCols.toString}
      tabindex={tabIndex toString}>{valueBox openOr ""}</textarea>
  }

  override def toForm: Box[NodeSeq] =
    uniqueFieldId match {
      case Full(id) =>  Full(elem % ("id" -> (id + "_field")))
      case _ => Full(elem)
    }


  override def toString = valueBox match {
    case Full(s) if s.length >= 100 => s.substring(0,40) + " ... " + s.substring(s.length - 40)
    case _ => super.toString
  }

  def textareaRows  = 8

  def textareaCols = 20
}

class TextareaField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int)
  extends StringField(rec, maxLength) with TextareaTypedField

class OptionalTextareaField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int)
  extends OptionalStringField(rec, maxLength) with TextareaTypedField

}
}
}
