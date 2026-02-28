/*
 * Copyright 2026 Lift Committers and Contributors
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

import scala.xml.Elem

import org.specs2.mutable.Specification


/**
 * Round-trip tests for HTML parsing and serialization pipelines.
 * These lock in the observable behavior of both parser paths so that
 * implementation changes can be validated against them.
 */
class HtmlRoundTripSpec extends Specification with Html5Parser with Html5Writer {
  "Html5 round-trip" should {
    "preserve element label and text content" in {
      val result = parse("<div>hello world</div>").openOrThrowException("Test")
      val output = toString(result)
      output must contain("<div>")
      output must contain("hello world")
      output must contain("</div>")
    }

    "preserve attribute values" in {
      val result = parse("""<a href="http://example.com" class="nav">link</a>""")
        .openOrThrowException("Test")
      val output = toString(result)
      output must contain("http://example.com")
      output must contain("nav")
      output must contain("link")
    }

    "preserve nested structure" in {
      val input = "<div><ul><li><a href=\"x\">text</a></li></ul></div>"
      val result = parse(input).openOrThrowException("Test")
      val output = toString(result)
      output must contain("<ul>")
      output must contain("<li>")
      output must contain("<a ")
      output must contain("text")
    }

    "preserve lift: namespace prefix" in {
      val result = parse("""<lift:surround with="default" at="content"><div/></lift:surround>""")
        .openOrThrowException("Test")
      val output = toString(result)
      output must contain("lift:surround")
      output must contain("""with="default"""")
    }

    "preserve unicode content" in {
      val result = parse("<p>café 日本語</p>").openOrThrowException("Test")
      val output = toString(result)
      output must contain("café")
      output must contain("日本語")
    }

    "represent nbsp as a non-breaking space character (not entity name)" in {
      // Html5 parser resolves &nbsp; to the U+00A0 character.
      // Html5Writer (with convertAmp=true, the toString default) does not convert it back.
      val result = parse("<p>&nbsp;</p>").openOrThrowException("Test")
      val output = toString(result)
      output must contain("\u00A0")
    }

    "write void tags without closing tag" in {
      val result = parse("<div><br/><img src=\"x\"/></div>").openOrThrowException("Test")
      val output = toString(result)
      output must contain("<br>")
      output must contain("<img ")
      output must not(contain("</br>"))
      output must not(contain("</img>"))
    }
  }

  "PCDataXmlParser + AltXML round-trip" should {
    "preserve element label and text content" in {
      val result = PCDataXmlParser("<div>hello world</div>").openOrThrowException("Test")
      val output = AltXML.toXML(result.head, false, true)
      output must contain("<div>")
      output must contain("hello world")
      output must contain("</div>")
    }

    "preserve attribute values" in {
      val result = PCDataXmlParser("""<a href="http://example.com">link</a>""")
        .openOrThrowException("Test")
      val output = AltXML.toXML(result.head, false, true)
      output must contain("http://example.com")
      output must contain("link")
    }

    "preserve nested structure" in {
      val input = "<div><ul><li><a href=\"x\">text</a></li></ul></div>"
      val result = PCDataXmlParser(input).openOrThrowException("Test")
      val output = AltXML.toXML(result.head, false, true)
      output must contain("<ul>")
      output must contain("<li>")
      output must contain("text")
    }

    "preserve CDATA content as CDATA" in {
      val result = PCDataXmlParser("<div><![CDATA[x < y & z]]></div>")
        .openOrThrowException("Test")
      val output = AltXML.toXML(result.head, false, true)
      output must contain("<![CDATA[")
      output must contain("x < y & z")
      output must contain("]]>")
    }

    "preserve HTML entities as characters with convertAmp=true" in {
      // With convertAmp=true, the U+00A0 char produced by parsing &nbsp;
      // is emitted as the character (not converted back to entity name)
      val result = PCDataXmlParser("<p>&nbsp;</p>").openOrThrowException("Test")
      val output = AltXML.toXML(result.head, false, true)
      output must contain("\u00A0")
    }

    "convert high-codepoint chars back to entity names with convertAmp=false" in {
      // With convertAmp=false, U+00A0 is reversed to &nbsp; via revMap
      val result = PCDataXmlParser("<p>&nbsp;</p>").openOrThrowException("Test")
      val output = AltXML.toXML(result.head, false, false)
      output must contain("&nbsp;")
    }

    "preserve lift: namespace prefix through both parsers" in {
      val result = PCDataXmlParser("""<lift:surround with="default" at="content"><div/></lift:surround>""")
        .openOrThrowException("Test")
      val output = AltXML.toXML(result.head, false, true)
      output must contain("lift:surround")
      output must contain("""with="default"""")
    }
  }

}
