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

import _root_.net.liftweb.http.js.{JsExp, JsObj}
import _root_.net.liftweb.http.js.JE.{JsArray, JsFalse, JsNull, JsObj, JsTrue, Num, Str}
import _root_.net.liftweb.json.JsonAST.{JArray, JBool, JInt, JDouble, JField, JNothing, JNull, JObject, JString, JValue}

object RecordHelpers {
  
  /* For the moment, I couldn't find any other way to bridge JValue and JsExp, so I wrote something simple here */
  implicit def jvalueToJsExp(jvalue: JValue): JsExp = {
    jvalue match {
      case JArray(vs)  => JsArray(vs.map(jvalueToJsExp): _*)
      case JBool(b)    => if (b) JsTrue else JsFalse
      case JDouble(d)  => Num(d)
      case JField(n,v) => error("no parallel")
      case JInt(i)     => Num(i)
      case JNothing    => JsNull
      case JNull       => JsNull
      case JObject(fs) => JsObj(fs.map(f => (f.name, jvalueToJsExp(f.value))): _*)
      case JString(s)  => Str(s)
    }
  }
}



}
}