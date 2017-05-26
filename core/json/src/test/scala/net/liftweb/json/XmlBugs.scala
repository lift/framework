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

import scala.xml.quote._
import org.specs2.mutable.Specification


object XmlBugs extends Specification {
  import Xml._
  import scala.xml.{Group, Text}

  "HarryH's XML parses correctly" in {
    val xml1 = xml"<venue><id>123</id></venue>"
    val xml2 = xml"<venue> <id>${"1"}${"23"}</id> </venue>"
    Xml.toJson(xml1) must_== Xml.toJson(xml2)
  }

  "HarryH's XML with attributes parses correctly" in {
    val json = toJson(xml"""<tips><group type="Nearby"><tip><id>10</id></tip></group></tips>""")
    compactRender(json) mustEqual """{"tips":{"group":{"type":"Nearby","tip":{"id":"10"}}}}"""
  }

  "Jono's XML with attributes parses correctly" in {
    val example1 = xml"""<word term="example" self="http://localhost:8080/word/example" available="true">content</word>"""
    val expected1 = """{"word":"content","self":"http://localhost:8080/word/example","term":"example","available":"true"}"""

    val example2 = xml"""<word term="example" self="http://localhost:8080/word/example" available="true"></word>"""
    val expected2 = """{"self":"http://localhost:8080/word/example","term":"example","available":"true"}"""

    (toJson(example1) diff parse(expected1)) mustEqual Diff(JNothing, JNothing, JNothing)
    (toJson(example2) diff parse(expected2)) mustEqual Diff(JNothing, JNothing, JNothing)
  }

  "Nodes with attributes converted to correct JSON" in {
    val xml =
      xml"""<root>
        <n id="10" x="abc" />
        <n id="11" x="bcd" />
      </root>"""
    val expected    = """{"root":{"n":[{"x":"abc","id":"10"},{"x":"bcd","id":"11"}]}}"""
    val expected210 = """{"root":{"n":[{"id":"10","x":"abc"},{"id":"11","x":"bcd"}]}}"""
    val json = compactRender(toJson(xml))
    (json == expected || json == expected210) mustEqual true
  }

  "XML with empty node is converted correctly to JSON" in {
    val xml =
      xml"""<tips><group type="Foo"></group><group type="Bar"><tip><text>xxx</text></tip><tip><text>yyy</text></tip></group></tips>"""
    val expected = """{"tips":{"group":[{"type":"Foo"},{"type":"Bar","tip":[{"text":"xxx"},{"text":"yyy"}]}]}}"""
    compactRender(toJson(xml)) mustEqual expected
  }
}
