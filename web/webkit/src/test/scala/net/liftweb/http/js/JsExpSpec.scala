/*
 * Copyright 2010-2011 WorldWide Conferencing, LLC
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
package http
package js

import common._
import json._
import JsonDSL._
import util.Helpers._
import org.specs._
import org.specs.runner._
import org.specs.Sugar._

class JsExpSpecTest extends Runner(JsExpSpec) with JUnit with Console
object JsExpSpec extends Specification {
  "JsExp" should {
    "Deal with lift-json" in {
      val json = ("a" -> 4) ~ ("b" -> 44)

      JE.JsArray(json, "dog").toJsCmd must_== (
        """[{"a":4,"b":44}, "dog"]""" + "\n")

      
    }
  }
}
