/*
 * Copyright 2010 WorldWide Conferencing, LLC
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
package mongodb {
package record {
package field {

import _root_.net.liftweb.common.{Box, Empty, Failure, Full}
import _root_.net.liftweb.http.js.JE.Str
import _root_.net.liftweb.json.JsonAST.{JNothing, JObject, JValue}
import _root_.net.liftweb.json.JsonParser
import _root_.net.liftweb.record.{Field, MandatoryTypedField, Record}
import _root_.scala.xml.NodeSeq

@Deprecated
class JObjectField[OwnerType <: Record[OwnerType]](rec: OwnerType) extends Field[JObject, OwnerType] with MandatoryTypedField[JObject] {

  def asJs = Str(toString)

  def asJValue = (JNothing: JValue) // not implemented

  def setFromJValue(jvalue: JValue) = Empty // not implemented

  def asXHtml = <div></div>

  def defaultValue = JObject(List())

  def setFromAny(in: Any): Box[JObject] = in match {
    case jv: JObject => Full(set(jv))
    case Some(jv: JObject) => Full(set(jv))
    case Full(jv: JObject) => Full(set(jv))
    case seq: Seq[_] if !seq.isEmpty => seq.map(setFromAny).apply(0)
    case (s: String) :: _ => setFromString(s)
    case null => Full(set(null))
    case s: String => setFromString(s)
    case None | Empty | Failure(_, _, _) => Full(set(null))
    case o => setFromString(o.toString)
  }

  // assume string is json
  def setFromString(in: String): Box[JObject] = {
    // use lift-json to parse string into a JObject
    Full(set(JsonParser.parse(in).asInstanceOf[JObject]))
  }

  def toForm: Box[NodeSeq] = Empty

  def owner = rec
}

}
}
}
}
