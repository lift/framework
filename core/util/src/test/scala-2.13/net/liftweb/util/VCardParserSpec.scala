/*
 * Copyright 2008-2026 Lift Committers and Contributors
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

import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification


/**
 * Systems under specification for VCardParser.
 */
class VCardParserSpec extends Specification with XmlMatchers {
  "VCardParser Specification".title

  val vcard =
    """BEGIN:VCARD
      |VERSION:2.1
      |N:Gump;Forrest
      |FN:Forrest Gump
      |ORG:Bubba Gump Shrimp Co.
      |TITLE:Shrimp Man
      |TEL;WORK;VOICE:(111) 555-1212
      |TEL;HOME;VOICE:(404) 555-1212
      |END:VCARD""".stripMargin

  "VCard" should {
    "parse a basic VCard (2.1) correctly" in {
      val list = VCardParser.parse(vcard)
      list must beLike {
        case Left(l)  => {
          import VCardParser._
          l must_==
            List(
              VCardEntry(VCardKey("BEGIN", Nil), List("VCARD")),
              VCardEntry(VCardKey("VERSION", Nil), List("2.1")),
              VCardEntry(VCardKey("N", Nil), List("Gump", "Forrest")),
              VCardEntry(VCardKey("FN", Nil), List("Forrest Gump")),
              VCardEntry(VCardKey("ORG", Nil), List("Bubba Gump Shrimp Co.")),
              VCardEntry(VCardKey("TITLE", Nil), List("Shrimp Man")),
              VCardEntry(VCardKey("TEL", List(("WORK", ""), ("VOICE", ""))), List("(111) 555-1212")),
              VCardEntry(VCardKey("TEL", List(("HOME", ""), ("VOICE", ""))), List("(404) 555-1212")),
              VCardEntry(VCardKey("END", Nil), List("VCARD")))
        }
      }
    }

    "parse a basic Apple VCard (3.0) correctly" in {

      val appleIOS9DavidTaylorVCard3 =
        """BEGIN:VCARD
          |VERSION:3.0
          |PRODID:-//Apple Inc.//iPhone OS 9.3//EN
          |N:Taylor;David;;;
          |FN: David  Taylor
          |TEL;type=HOME;type=VOICE;type=pref:555-610-6679
          |item1.ADR;type=HOME;type=pref:;;1747 Steuart Street;Tiburon;CA;94920;USA
          |item1.X-ABADR:us
          |BDAY:1998-06-15
          |END:VCARD""".stripMargin

      val list = VCardParser.parse(appleIOS9DavidTaylorVCard3)
      list must beLike {
        case Left(l)  => {
          import VCardParser._
          l must_==
            List(
              VCardEntry(VCardKey("BEGIN", Nil), List("VCARD")),
              VCardEntry(VCardKey("VERSION", Nil), List("3.0")),
              VCardEntry(VCardKey("PRODID", Nil),List("-//Apple Inc.//iPhone OS 9.3//EN")),
              VCardEntry(VCardKey("N", Nil), List("Taylor", "David", "", "", "")),
              VCardEntry(VCardKey("FN", Nil), List(" David  Taylor")),
              VCardEntry(VCardKey("TEL", List(("type", "HOME"), ("type", "VOICE"), ("type", "pref"))), List("555-610-6679")),
              VCardEntry(VCardKey("ADR", List(("type","HOME"), ("type","pref"))),List("", "", "1747 Steuart Street", "Tiburon", "CA", "94920", "USA")),
              VCardEntry(VCardKey("X-ABADR",Nil),List("us")),
              VCardEntry(VCardKey("BDAY",Nil),List("1998-06-15")),
              VCardEntry(VCardKey("END", Nil), List("VCARD")))
        }
      }

    }

    "parse a more complex Apple VCard (3.0) correctly" in {

      val appleIOS9JohnAppleseedVCard3 =
        """BEGIN:VCARD
          |VERSION:3.0
          |PRODID:-//Apple Inc.//iPhone OS 9.3//EN
          |N:Appleseed;John;;;
          |FN: John  Appleseed
          |EMAIL;type=INTERNET;type=WORK;type=pref:John-Appleseed@mac.com
          |TEL;type=CELL;type=VOICE;type=pref:888-555-5512
          |TEL;type=HOME;type=VOICE:888-555-1212
          |item1.ADR;type=WORK;type=pref:;;3494 Kuhl Avenue;Atlanta;GA;30303;USA
          |item1.X-ABADR:ca
          |item2.ADR;type=HOME:;;1234 Laurel Street;Atlanta;GA;30303;USA
          |item2.X-ABADR:us
          |BDAY:1980-06-22
          |END:VCARD""".stripMargin

      val list = VCardParser.parse(appleIOS9JohnAppleseedVCard3)
      list must beLike {
        case Left(l)  => {
          import VCardParser._
          l must_==
            List(
              VCardEntry(VCardKey("BEGIN", Nil), List("VCARD")),
              VCardEntry(VCardKey("VERSION", Nil), List("3.0")),
              VCardEntry(VCardKey("PRODID", Nil),List("-//Apple Inc.//iPhone OS 9.3//EN")),
              VCardEntry(VCardKey("N", Nil), List("Appleseed", "John", "", "", "")),
              VCardEntry(VCardKey("FN", Nil), List(" John  Appleseed")),
              VCardEntry(VCardKey("EMAIL", List(("type","INTERNET"), ("type","WORK"), ("type","pref"))),List("John-Appleseed@mac.com")),
              VCardEntry(VCardKey("TEL", List(("type", "CELL"), ("type", "VOICE"), ("type", "pref"))), List("888-555-5512")),
              VCardEntry(VCardKey("TEL", List(("type", "HOME"), ("type", "VOICE"))), List("888-555-1212")),
              VCardEntry(VCardKey("ADR", List(("type","WORK"), ("type","pref"))),List("", "", "3494 Kuhl Avenue", "Atlanta", "GA", "30303", "USA")),
              VCardEntry(VCardKey("X-ABADR",Nil),List("ca")),
              VCardEntry(VCardKey("ADR", List(("type","HOME"))),List("", "", "1234 Laurel Street", "Atlanta", "GA", "30303", "USA")),
              VCardEntry(VCardKey("X-ABADR",Nil),List("us")),
              VCardEntry(VCardKey("BDAY",Nil),List("1980-06-22")),
              VCardEntry(VCardKey("END", Nil), List("VCARD")))
        }
      }

    }


  }

}
