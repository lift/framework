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

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

class ParserBugsTest extends Runner(ParserBugs) with JUnit
object ParserBugs extends Specification {
  import JsonParser.parseOpt

  "\uffff is a valid char in string literal" in {
    parseOpt(""" {"x":"\uffff"} """).isDefined mustEqual true
  }
}

}
}
