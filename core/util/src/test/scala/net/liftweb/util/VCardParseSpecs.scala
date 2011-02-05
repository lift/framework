/*
 * Copyright 2008-2010 WorldWide Conferencing, LLC
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
package util {

import _root_.org.specs._
import _root_.org.specs.runner._

class ToVCardParseTest extends JUnit4(VCardParseSpecs)
object VCardParseSpecs extends Specification {
  "VCard" should {
    "parse correctly" >> {

    var vcard = """
BEGIN:VCARD
VERSION:2.1
N:Gump;Forrest
FN:Forrest Gump
ORG:Bubba Gump Shrimp Co.
TITLE:Shrimp Man
TEL;WORK;VOICE:(111) 555-1212
TEL;HOME;VOICE:(404) 555-1212
END:VCARD"""
    
      val list = VCardParser.parse(vcard);
      import VCardParser._
      list match {
        case Left(l) => {
          l must_== List(
          VCardEntry(VCardKey("BEGIN",List()),List("VCARD")),
          VCardEntry(VCardKey("VERSION",List()),List("2.1")),
          VCardEntry(VCardKey("N",List()),List("Gump", "Forrest")),
          VCardEntry(VCardKey("FN",List()),List("Forrest Gump")),
          VCardEntry(VCardKey("ORG",List()),List("Bubba Gump Shrimp Co.")),
          VCardEntry(VCardKey("TITLE",List()),List("Shrimp Man")),
          VCardEntry(VCardKey("TEL",List(("WORK",""), ("VOICE", ""))),List("(111) 555-1212")),
          VCardEntry(VCardKey("TEL",List(("HOME", ""), ("VOICE", ""))),List("(404) 555-1212")),
          VCardEntry(VCardKey("END",List()),List("VCARD"))
          )
        }
        case Right(r) => fail(r toString)
      }
      
    }
  }
}

}
}
