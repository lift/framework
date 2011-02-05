/*
* Copyright 2010 WorldWide Conferencing, LLC
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

package net.liftweb {
package mongodb {
package record {
package field {

import java.util.UUID

import scala.xml.NodeSeq

import net.liftweb.common.{Box, Empty, Failure, Full}
import net.liftweb.http.js.JE.{JsNull, JsRaw}
import net.liftweb.http.S
import net.liftweb.json.JsonAST._
import net.liftweb.json.Printer
import net.liftweb.mongodb.record._
import net.liftweb.record.{Field, FieldHelpers, MandatoryTypedField}
import net.liftweb.util.Helpers._

class UUIDField[OwnerType <: MongoRecord[OwnerType]](rec: OwnerType)
  extends Field[UUID, OwnerType]
  with MandatoryTypedField[UUID]
{

  def owner = rec

  def defaultValue = UUID.randomUUID

  def setFromAny(in: Any): Box[UUID] = in match {
    case uid: UUID => setBox(Full(uid))
    case Some(uid: UUID) => setBox(Full(uid))
    case Full(uid: UUID) => setBox(Full(uid))
    case (uid: UUID) :: _ => setBox(Full(uid))
    case s: String => setFromString(s)
    case Some(s: String) => setFromString(s)
    case Full(s: String) => setFromString(s)
    case null|None|Empty => setBox(defaultValueBox)
    case f: Failure => setBox(f)
    case o => setFromString(o.toString)
  }

  def setFromJValue(jvalue: JValue): Box[UUID] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JObject(JField("$uuid", JString(s)) :: Nil) => setFromString(s)
    case other => setBox(FieldHelpers.expectedA("JObject", other))
  }

  def setFromString(in: String): Box[UUID] = tryo(UUID.fromString(in)) match {
    case Full(uid: UUID) => setBox(Full(uid))
    case f: Failure => setBox(f)
    case other => setBox(Failure("Invalid UUID string: "+in))
  }

  private def elem =
    S.fmapFunc(S.SFuncHolder(this.setFromAny(_))){funcName =>
      <input type="text"
        name={funcName}
        value={valueBox.map(v => v.toString) openOr ""}
        tabindex={tabIndex toString}/>
    }

  def toForm =
    uniqueFieldId match {
      case Full(id) => Full(elem % ("id" -> (id + "_field")))
      case _ => Full(elem)
    }

  def asJs = asJValue match {
    case JNothing => JsNull
    case jv => JsRaw(Printer.compact(render(jv)))
  }

  def asJValue: JValue = valueBox.map(v => Meta.Reflection.uuidAsJValue(v)) openOr (JNothing: JValue)

}

}
}
}
}
