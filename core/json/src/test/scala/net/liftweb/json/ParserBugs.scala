/*
 * Copyright 2009-2011 WorldWide Conferencing, LLC
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
package json

import util.control.Exception._
import org.specs2.mutable.Specification

import scala.annotation.nowarn

@nowarn("msg=Unicode escapes in triple quoted strings are deprecated; use the literal character instead") // IIUC, it's what is tested
object ParserBugs extends Specification {
  "Unicode ffff is a valid char in string literal" in {
    parseOpt(""" {"x":"\uffff"} """).isDefined mustEqual true
  }

  "Does not allow colon at start of array (1039)" in {
    parseOpt("""[:"foo", "bar"]""") mustEqual None
  }

  "Does not allow colon instead of comma in array (1039)" in {
    parseOpt("""["foo" : "bar"]""") mustEqual None
  }

  "Solo quote mark should fail cleanly (not StringIndexOutOfBoundsException) (1041)" in {
    JsonParser.parse("\"", discardParser) must throwA[JsonParser.ParseException].like {
      case e => e.getMessage must startWith("unexpected eof")
    }
  }

  "Field names must be quoted" in {
    val json = JObject(List(JField("foo\nbar", JInt(1))))
    val s = compactRender(json)
    (s mustEqual """{"foo\nbar":1}""") and
      (parse(s) mustEqual json)
  }

  "Double in scientific notation with + can be parsed" in {
    val json = JObject(List(JField("t", JDouble(12.3))))
    val s = """{"t" : 1.23e+1}"""
    parse(s) mustEqual json
  }

  private val discardParser = (p : JsonParser.Parser) => {
     var token: JsonParser.Token = null
     do {
       token = p.nextToken
     } while (token != JsonParser.End)
   }
}
