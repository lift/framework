/*
 * Copyright 2006-2011 WorldWide Conferencing, LLC
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

import xml.Elem

import org.specs2.mutable.Specification
import org.specs2.execute.PendingUntilFixed

import common._
import Helpers._


/**
 * Systems under specification for Html5 Parser.
 */
object Html5ParserSpec extends Specification with PendingUntilFixed with Html5Parser with Html5Writer {
  "Html5Parser Specification".title

  "Htm5 Writer" should {
    "Write &" in {
      toString(<foo baz="&amp;dog"/>) must_== """<foo baz="&dog"></foo>"""
    }

    "ignore attributes that are null" in {
      toString(<foo id={None}/>) must_== """<foo></foo>"""
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
  }

}

