/*
 * Copyright 2007-2026 Lift Committers and Contributors
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

import java.io.ByteArrayInputStream

import scala.xml.{ EntityRef, Text, Unparsed }

import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification


/**
 * Systems under specification for XmlParser, specifically PCDataMarkupParser.
 */
class XmlParserSpec extends Specification with XmlMatchers {
  "Xml Parser Specification".title

  "Multiple attributes with same name, but different namespace" should {
    "parse correctly" >> {
      val actual =
      <lift:surround with="base" at="body">
        <lift:Menu.builder  li_path:class="p" li_item:class="i"/>
      </lift:surround>

      val expected =
      <lift:surround with="base" at="body">
        <lift:Menu.builder  li_path:class="p" li_item:class="i"/>
      </lift:surround>

      val bis = new ByteArrayInputStream(actual.toString.getBytes("UTF-8"))
      val parsed = PCDataXmlParser(bis).openOrThrowException("Test")
      parsed must ==/(expected)
    }

  }

  "XML can contain PCData" in {
    val data = <foo>{
        PCData("Hello Yak")
      }</foo>

    val str = AltXML.toXML(data, false, true)

    str.indexOf("<![CDATA[") must be > -1
  }

  "XML can contain Unparsed" in {
    val data = <foo>{
        Unparsed("Hello & goodbye > <yak Yak")
      }</foo>

    val str = AltXML.toXML(data, false, true)

    str.indexOf("Hello & goodbye > <yak Yak") must be > -1
  }

  "XML cannot contain Control characters" in {
     val data =
     <foo>
      {
        '\u0085'
      }{
        Text("hello \u0000 \u0085 \u0080")
      }{
        "hello \u0000 \u0003 \u0085 \u0080"
      }{
        '\u0003'
      }
    </foo>

    val str = AltXML.toXML(data, false, true)

    def cntIllegal(in: Char): Int = in match {
      case '\u0085' => 1
      case c if (c >= '\u007f' && c <= '\u0095') => 1
      case '\n' => 0
      case '\r' => 0
      case '\t' => 0
      case c if c < ' ' => 1
      case _ => 0
    }

    str.toList.foldLeft(0)((a, b) => a + cntIllegal(b)) must_== 0
  }

  "AltXML" should {
    "render empty elements with self-close syntax" in {
      AltXML.toXML(<br/>, false, true) must contain("/>")
    }

    "render non-empty elements with open and close tags" in {
      val result = AltXML.toXML(<div>x</div>, false, true)
      result must contain("<div>")
      result must contain("</div>")
    }

    "preserve EntityRef nodes when convertAmp is false" in {
      val data = <p>{EntityRef("nbsp")}</p>
      val result = AltXML.toXML(data, false, false)
      result must contain("&nbsp;")
    }

    "convert high-codepoint EntityRef to character when convertAmp is true" in {
      // nbsp = 160, which is >= 128, so it converts to the actual char
      val data = <p>{EntityRef("nbsp")}</p>
      val result = AltXML.toXML(data, false, true)
      result must contain("\u00A0")
    }

    "preserve low-codepoint EntityRef as entity when convertAmp is true" in {
      // amp = 38, which is < 128, so it stays as &amp;
      val data = <p>{EntityRef("amp")}</p>
      val result = AltXML.toXML(data, false, true)
      result must contain("&amp;")
    }

    "apply legacy IE compatibility mode to br tags" in {
      // legacy mode: br gets />, no space
      val withLegacy = AltXML.toXML(<br/>, false, true, legacyIeCompatibilityMode = true)
      withLegacy must contain("/>")
      // without legacy mode: br is in inlineTags, gets " />"
      val withoutLegacy = AltXML.toXML(<br/>, false, true, legacyIeCompatibilityMode = false)
      withoutLegacy must contain(" />")
    }
  }

}
