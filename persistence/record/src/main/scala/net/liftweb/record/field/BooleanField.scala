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
import net.liftweb.common._
import net.liftweb.http.js._
import net.liftweb.http.{S, SHtml}
import net.liftweb.json.JsonAST.{JBool, JNothing, JNull, JValue}
import net.liftweb.util._
import Helpers._
import S._
import JE._

trait BooleanTypedField extends TypedField[Boolean] {
  
  def setFromAny(in: Any): Box[Boolean] = in match{
      case b: java.lang.Boolean => setBox(Full(b.booleanValue))
      case Full(b: java.lang.Boolean) => setBox(Full(b.booleanValue))
      case Some(b: java.lang.Boolean) => setBox(Full(b.booleanValue))
      case (b: java.lang.Boolean) :: _ => setBox(Full(b.booleanValue))
      case _ => genericSetFromAny(in)
  }

  def setFromString(s: String): Box[Boolean] = 
    if(s == null || s.isEmpty) {
      if(optional_?)
    	  setBox(Empty)
       else
          setBox(Failure(notOptionalErrorMessage))
    } else {
      setBox(tryo(toBoolean(s)))
    }

  private def elem(attrs: SHtml.ElemAttr*) =
      SHtml.checkbox(valueBox openOr false, (b: Boolean) => this.setBox(Full(b)), (("tabindex" -> tabIndex.toString): SHtml.ElemAttr) :: attrs.toList: _*)

  def toForm: Box[NodeSeq] =
    // FIXME? no support for optional_?
    uniqueFieldId match {
      case Full(id) => Full(elem("id" -> id))
      case _ => Full(elem())
    }

  def asJs: JsExp = valueBox.map(boolToJsExp) openOr JsNull

  def asJValue = valueBox.map(JBool) openOr (JNothing: JValue)
  def setFromJValue(jvalue: JValue) = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JBool(b)                     => setBox(Full(b))
    case other                        => setBox(FieldHelpers.expectedA("JBool", other))
  }
}

class BooleanField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends Field[Boolean, OwnerType] with MandatoryTypedField[Boolean] with BooleanTypedField {

  def owner = rec

  def this(rec: OwnerType, value: Boolean) = {
    this(rec)
    set(value)
  }

  def defaultValue = false
}

class OptionalBooleanField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends Field[Boolean, OwnerType] with OptionalTypedField[Boolean] with BooleanTypedField {

  def owner = rec

  def this(rec: OwnerType, value: Box[Boolean]) = {
    this(rec)
    setBox(value)
  }
}

