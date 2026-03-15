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
      toString(<foo baz="&amp;dog"/>) must_== """<foo baz="&dog"></foo>"""
    }

    "ignore attributes that are null" in {
      toString(<foo id={None}/>) must_== """<foo></foo>"""
    }

    "render void tags without a closing tag" in {
      toString(<br/>) must_== "<br>"
    }

    "render void tags with attributes" in {
      toString(<img src="x" alt="y"/>) must_== """<img src="x" alt="y">"""
    }

    "render non-void empty tags with a closing tag" in {
      toString(<div/>) must_== "<div></div>"
    }

    "not escape content inside script tags" in {
      toString(<script>{"var x = 1 < 2 && true;"}</script>) must_== "<script>var x = 1 < 2 && true;</script>"
    }

    "not escape content inside style tags" in {
      toString(<style>{"p > span { color: red; }"}</style>) must_== "<style>p > span { color: red; }</style>"
    }

    "write PCData as CDATA section" in {
      toString(<div>{PCData("x < y & z")}</div>) must_== "<div><![CDATA[x < y & z]]></div>"
    }

    "write Comment nodes" in {
      toString(<div>{Comment("a comment")}</div>) must_== "<div><!--a comment--></div>"
    }

    "preserve namespace prefix on elements" in {
      toString(<lift:surround with="default" at="content"><div/></lift:surround>) must_==
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
          (parsed \\ "script").length must be >= 4
        }

        "parse valid page type2" in {
          val parsed = parse(page2).openOrThrowException("Test")
          (parsed \\ "script").length must be >= 4
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
      heads.length must_== 1
      heads.text must_== "123"
      (heads(0).asInstanceOf[Elem].prefix == null) must_== true
    }.pendingUntilFixed

    "Parse stuff with lift: namespace" in {
      val parsed = parse("""<lift:surround with="dog"><div/></lift:surround>""")
      val e = parsed.openOrThrowException("Test").asInstanceOf[Elem]
      e.prefix must_== "lift"
      e.label must_== "surround"
      (parsed.openOrThrowException("Test") \ "@with").text must_== "dog"
    }

    "Parse stuff without lift: namespace" in {
      val parsed = parse("""<div with="dog"><div/></div>""")
      val e = parsed.openOrThrowException("Test").asInstanceOf[Elem]
      e.label must_== "div"
      (parsed.openOrThrowException("Test") \ "@with").text must_== "dog"
    }

    "unwrap a single fragment element via AutoInsertedBody" in {
      val result = parse("<div>hello</div>").openOrThrowException("Test")
      result.label must_== "div"
      result.text must_== "hello"
    }

    "not unwrap a full html document" in {
      val result = parse("<html><head><title>T</title></head><body><p>X</p></body></html>")
        .openOrThrowException("Test")
      result.label must_== "html"
    }

    "unwrap a single self-closing fragment element" in {
      val result = parse("<span/>").openOrThrowException("Test")
      result.label must_== "span"
    }

    "resolve standard HTML entities to their character values" in {
      val result = parse("<p>&nbsp;</p>").openOrThrowException("Test")
      result.text must_== "\u00A0"
    }

    "preserve script tag content verbatim (in head)" in {
      // nu.validator places bare <script> in <head>; AutoInsertedBody does not unwrap since head is non-empty
      val result = parse("<script>var x = 1 < 2 && true;</script>").openOrThrowException("Test")
      result.label must_== "html"
      (result \\ "script").text must_== "var x = 1 < 2 && true;"
    }

    "preserve style tag content verbatim (in head)" in {
      // nu.validator places bare <style> in <head>; AutoInsertedBody does not unwrap since head is non-empty
      val result = parse("<style>p > span { color: red; }</style>").openOrThrowException("Test")
      result.label must_== "html"
      (result \\ "style").text must_== "p > span { color: red; }"
    }

    "preserve data-* attributes" in {
      val result = parse("""<div data-foo="bar" data-baz="qux">x</div>""").openOrThrowException("Test")
      (result \ "@data-foo").text must_== "bar"
      (result \ "@data-baz").text must_== "qux"
    }

    "preserve Unicode content" in {
      val result = parse("<p>café naïve 日本語</p>").openOrThrowException("Test")
      result.text must_== "café naïve 日本語"
    }

    "return a Full result for empty string input" in {
      parse("") must beAnInstanceOf[Full[Elem]]
    }
  }

}
