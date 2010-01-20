/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
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
package json {

import _root_.org.scalacheck._
import _root_.org.scalacheck.Prop.forAll
import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}
import _root_.org.specs.ScalaCheck

class ParserTest extends Runner(ParserSpec) with JUnit
object ParserSpec extends Specification with JValueGen with ScalaCheck {
  import JsonAST._
  import JsonParser._

  "Any valid json can be parsed" in {
    val parsing = (json: JValue) => { parse(JsonDSL.pretty(render(json))); true }
    forAll(parsing) must pass
  }

  "All valid string escape characters can be parsed" in {
    parse("[\"abc\\\"\\\\\\/\\b\\f\\n\\r\\t\\u00a0\"]") must_== JArray(JString("abc\"\\/\b\f\n\r\t\u00a0")::Nil)
  }

  implicit def arbJValue: Arbitrary[JValue] = Arbitrary(genObject)
}

}
}
