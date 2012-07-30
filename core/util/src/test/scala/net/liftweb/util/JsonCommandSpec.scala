/*
 * Copyright 2008-2011 WorldWide Conferencing, LLC
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
package util

import org.specs2.mutable.Specification

import json._


/**
 * Systems under specification for JsonCommand.
 */
object JsonCommandSpec extends Specification  {
  "JsonCommand Specification".title

  private def parse(in: String): JValue = JsonParser.parse(in)

  "The JsonCommand object" should {
    "return None for non-commands" in {
      JsonCommand.unapply(parse("""{"foo": "bar", "baz": false, "params": "moose"} """)) must_== None
    }

    "return None for non-params" in {
      JsonCommand.unapply(parse("""{"command": "frog", "foo": "bar", "baz": false} """)) must_== None
    }

    "Parse even if target missing" in {
      JsonCommand.unapply(parse("""{"command": "frog", "foo": "bar", "params": 99} """)) must_== Some(("frog", None, JInt(99)))
    }

    "Parse the whole thing" in {
      JsonCommand.unapply(parse("""{"command": "frog", "target": "spud", "foo": "bar", "params": 982, "baz": false} """)) must_==
      Some(("frog", Some("spud"), JInt(982)))
    }
  }
}
