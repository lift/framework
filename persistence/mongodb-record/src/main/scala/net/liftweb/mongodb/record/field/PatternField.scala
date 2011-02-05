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

import java.util.regex.Pattern

import net.liftweb.common.{Box, Empty, Failure, Full}
import net.liftweb.http.js.JE.{JsNull, Str}
import net.liftweb.json.JsonAST._
import net.liftweb.json.{JsonParser, Printer}
import net.liftweb.mongodb.record._
import net.liftweb.record.{Field, FieldHelpers, MandatoryTypedField}
import net.liftweb.util.Helpers.tryo

class PatternField[OwnerType <: MongoRecord[OwnerType]](rec: OwnerType)
  extends Field[Pattern, OwnerType]
  with MandatoryTypedField[Pattern]
{

  def owner = rec

  def defaultValue = Pattern.compile("")

  def setFromAny(in: Any): Box[Pattern] = in match {
    case p: Pattern => setBox(Full(p))
    case Some(p: Pattern) => setBox(Full(p))
    case Full(p: Pattern) => setBox(Full(p))
    case (p: Pattern) :: _ => setBox(Full(p))
    case s: String => setFromString(s)
    case Some(s: String) => setFromString(s)
    case Full(s: String) => setFromString(s)
    case null|None|Empty => setBox(defaultValueBox)
    case f: Failure => setBox(f)
    case o => setFromString(o.toString)
  }

  def setFromJValue(jvalue: JValue): Box[Pattern] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JObject(JField("$regex", JString(s)) :: JField("$flags", JInt(f)) :: Nil) =>
      setBox(Full(Pattern.compile(s, f.intValue)))
    case other => setBox(FieldHelpers.expectedA("JObject", other))
  }

  // parse String into a JObject
  def setFromString(in: String): Box[Pattern] = tryo(JsonParser.parse(in)) match {
    case Full(jv: JValue) => setFromJValue(jv)
    case f: Failure => setBox(f)
    case other => setBox(Failure("Error parsing String into a JValue: "+in))
  }

  def toForm = Empty

  def asJs = asJValue match {
    case JNothing => JsNull
    case jv => Str(Printer.compact(render(jv)))
  }

  def asJValue: JValue = valueBox.map(v => Meta.Reflection.patternAsJValue(v)) openOr (JNothing: JValue)
}

}
}
}
}
