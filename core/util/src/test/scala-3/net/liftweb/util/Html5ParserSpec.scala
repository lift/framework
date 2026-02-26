/*
 * Copyright 2006-2026 Lift Committers and Contributors
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

import scala.xml.{ Comment, Elem }

import org.specs2.mutable.Specification
import org.specs2.execute.PendingUntilFixed

import common._
import Helpers._


/**
 * Systems under specification for Html5 Parser.
 */
class Html5ParserSpec extends Specification with PendingUntilFixed with Html5Parser with Html5Writer {
  "Html5Parser Specification".title

  "Html5 Writer" should {
    "Write &" in {
      toString(<foo baz="&amp;dog"/>) === """<foo baz="&dog"></foo>"""
    }

    "ignore attributes that are null" in {
      toString(<foo id={None}/>) === """<foo></foo>"""
    }

    "render void tags without a closing tag" in {
      toString(<br/>) === "<br>"
    }

    "render void tags with attributes" in {
      toString(<img src="x" alt="y"/>) === """<img src="x" alt="y">"""
    }

    "render non-void empty tags with a closing tag" in {
      toString(<div/>) === "<div></div>"
    }

    "not escape content inside script tags" in {
      toString(<script>{"var x = 1 < 2 && true;"}</script>) === "<script>var x = 1 < 2 && true;</script>"
    }

    "not escape content inside style tags" in {
      toString(<style>{"p > span { color: red; }"}</style>) === "<style>p > span { color: red; }</style>"
    }

    "write PCData as CDATA section" in {
      toString(<div>{PCData("x < y & z")}</div>) === "<div><![CDATA[x < y & z]]></div>"
    }

    "write Comment nodes" in {
      toString(<div>{Comment("a comment")}</div>) === "<div><!--a comment--></div>"
    }

    "preserve namespace prefix on elements" in {
      toString(<lift:surround with="default" at="content"><div/></lift:surround>) ===
        """<lift:surround with="default" at="content"><div></div></lift:surround>"""
    }
  }

  "Html5 Parser" should {
    val pages = for {
      page1 <- tryo(readWholeStream(getClass.getResourceAsStream("Html5ParserSpec.page1.html"))).filter(_ ne null)
      page2 <- tryo(readWholeStream(getClass.getResourceAsStream("Html5ParserSpec.page2.html"))).filter(_ ne null)
      page3 <- tryo(readWholeStream(getClass.getResourceAsStream("Html5ParserSpec.page3.html"))).filter(_ ne null)
    } yield (page1, page2, page3)

    pages match {
      case Full(p) =>
        val (page1, page2, page3) = (new String(p._1), new String(p._2), new String(p._3))

        "parse valid page type1" in {
          val parsed = parse(page1).openOrThrowException("Test")
          (parsed \\ "script").length must beGreaterThanOrEqualTo(4)
        }

        "parse valid page type2" in {
          val parsed = parse(page2).openOrThrowException("Test")
          (parsed \\ "script").length must beGreaterThanOrEqualTo(4)
        }

        "fail to parse invalid page type3" in {
          val parsed = parse(page3)
          parsed must beAnInstanceOf[Failure]
        }.pendingUntilFixed

      case _ =>
        failure("Failed loading test files") // TODO: Improve error message
    }

    "change <lift:head> to <head>" in {
      val parsed = parse("<div><lift:head>123</lift:head></div>").openOrThrowException("Test")
      val heads = parsed \\ "head"
      heads.length === 1
      heads.text === "123"
      (heads(0).asInstanceOf[Elem].prefix == null) === true
    }.pendingUntilFixed

    "Parse stuff with lift: namespace" in {
      val parsed = parse("""<lift:surround with="dog"><div/></lift:surround>""")
      val e = parsed.openOrThrowException("Test").asInstanceOf[Elem]
      e.prefix === "lift"
      e.label === "surround"
      (parsed.openOrThrowException("Test") \ "@with").text === "dog"
    }

    "Parse stuff without lift: namespace" in {
      val parsed = parse("""<div with="dog"><div/></div>""")
      val e = parsed.openOrThrowException("Test").asInstanceOf[Elem]
      e.label === "div"
      (parsed.openOrThrowException("Test") \ "@with").text === "dog"
    }

    "unwrap a single fragment element via AutoInsertedBody" in {
      val result = parse("<div>hello</div>").openOrThrowException("Test")
      result.label === "div"
      result.text === "hello"
    }

    "not unwrap a full html document" in {
      val result = parse("<html><head><title>T</title></head><body><p>X</p></body></html>")
        .openOrThrowException("Test")
      result.label === "html"
    }

    "unwrap a single self-closing fragment element" in {
      val result = parse("<span/>").openOrThrowException("Test")
      result.label === "span"
    }

    "resolve standard HTML entities to their character values" in {
      val result = parse("<p>&nbsp;</p>").openOrThrowException("Test")
      result.text === "\u00A0"
    }

    "preserve script tag content verbatim (in head)" in {
      // nu.validator places bare <script> in <head>; AutoInsertedBody does not unwrap since head is non-empty
      val result = parse("<script>var x = 1 < 2 && true;</script>").openOrThrowException("Test")
      result.label === "html"
      (result \\ "script").text === "var x = 1 < 2 && true;"
    }

    "preserve style tag content verbatim (in head)" in {
      // nu.validator places bare <style> in <head>; AutoInsertedBody does not unwrap since head is non-empty
      val result = parse("<style>p > span { color: red; }</style>").openOrThrowException("Test")
      result.label === "html"
      (result \\ "style").text === "p > span { color: red; }"
    }

    "preserve data-* attributes" in {
      val result = parse("""<div data-foo="bar" data-baz="qux">x</div>""").openOrThrowException("Test")
      (result \ "@data-foo").text === "bar"
      (result \ "@data-baz").text === "qux"
    }

    "preserve Unicode content" in {
      val result = parse("<p>café naïve 日本語</p>").openOrThrowException("Test")
      result.text === "café naïve 日本語"
    }

    "return a Full result for empty string input" in {
      parse("") must beAnInstanceOf[Full[Elem]]
    }
  }

}

