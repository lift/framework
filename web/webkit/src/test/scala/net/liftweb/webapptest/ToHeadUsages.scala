/*
 * Copyright 2007-2011 WorldWide Conferencing, LLC
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
package webapptest

import org.specs.Specification

import util._
import http._


/**
 * System under specification for ToHeadUsages.
 */
object ToHeadUsages extends Specification("ToHeadUsages Specification") {

  val J = JettyTestServer

  doBeforeSpec(J.start())

  "lift <head> merger" should {
    "merge <head> from html fragment" in {
      J.browse(
        "/htmlFragmentWithHead", html =>
         html.getElementByXPath("/html/head/script[@id='fromFrag']") must notBeNull.when(J.running)
      )
    }

    "merge <head> from html fragment does not include head element in body" in {
      J.browse(
        "/htmlFragmentWithHead", html =>
         html.getElementsByXPath("/html/body/script[@id='fromFrag']").size must be_==(0).when(J.running)
      )
    }

    "merge <head> from snippet" in {
      J.browse(
        "/htmlSnippetWithHead", html =>
         html.getElementByXPath("/html/head/script[@src='snippet.js']") must notBeNull.when(J.running)
      )
    }

    "not merge for bodyless html" in {
      J.browse(
        "/basicDiv",html => {
          html.getElementById("fruit") must notBeNull.when(J.running)
          html.getElementById("bat")   must notBeNull.when(J.running)
        }
      )
    }

    "not merge for headless bodyless html" in {
      J.browse(
        "/h1",html => {
          html.getElementById("h1") must notBeNull.when(J.running)
        }
      )
    }

    "not merge for headless body html" in {
      J.browse(
        "/body_no_head",html => {
          // Note: The XPath expression "html/body/head/div" fails here with
          // HtmlUnit 2.5 since "head" is not recognized as a XHTML element
          // due to its incorrect position (under body instead of directly under html)
          html.getElementsByXPath("/html/body//div").size must be_==(1).when(J.running)
        }
      )
    }

    "not merge non-html" in {
      J.browse(
        "/non_html",html => {
          html.getElementById("frog") must notBeNull.when(J.running)
        }
      )
    }

  }

  "pages " should {
    "Template finder should recognize entities" in {
      val ns = TemplateFinder.findAnyTemplate(List("index")).open_!
      val str = AltXML.toXML(ns(0), false, false, false)

      val idx = str.indexOf("&mdash;")
      (idx >= 0) must beTrue.when(J.running)
    }

    "Template finder should not recognize entities" in {
      val ns = TemplateFinder.findAnyTemplate(List("index")).open_!
      val str = AltXML.toXML(ns(0), false, true, false)

      val idx = str.indexOf("&mdash;")
      (idx >= 0) must beFalse.when(J.running)
    }

    /*
    "round trip entities" in {
      JettyTestServer.browse(
        "/index",html => {
          val idx = html.getPageSource.indexOf("&mdash;")
          (idx >= 0) must_== true
        }
      )
    }
    */
  }

  "deferred snippets" should {
    "render" in {
      J.browse(
        "/deferred",html => {
          html.getElementById("second") must notBeNull.when(J.running)
        }
      )
    }

    "not deferred not in actor" in {
      J.browse(
        "/deferred",html => {
          html.getElementByXPath("/html/body/span[@id='whack1']/span[@id='actor_false']") must notBeNull.when(J.running)
        }
      )
    }

    "deferred in actor" in {
      J.browse(
        "/deferred",html => {
          html.getElementByXPath("/html/body/span[@id='whack2']/span[@id='actor_true']") must notBeNull.when(J.running)
        }
      )
    }

    "Exclude from context rewriting" in {
      val first = http.Req.fixHtml("/wombat",
        <span>
          <a href="/foo" id="foo">foo</a>
          <a href="/bar" id="bar">bar</a>
        </span>
      )

      def excludeBar(in: String): Boolean = in.startsWith("/bar")

      val second = LiftRules.excludePathFromContextPathRewriting.doWith(excludeBar _) {
        Req.fixHtml("/wombat",
          <span>
            <a href="/foo" id="foo">foo</a>
            <a href="/bar" id="bar">bar</a>
          </span>
        )
      }

      ((first \\ "a").filter(e => (e \ "@id").text == "foo") \ "@href").text must be_==("/wombat/foo").when(J.running)
      ((first \\ "a").filter(e => (e \ "@id").text == "bar") \ "@href").text must be_==("/wombat/bar").when(J.running)
      ((second \\ "a").filter(e => (e \ "@id").text == "foo") \ "@href").text must be_==("/wombat/foo").when(J.running)
      ((second \\ "a").filter(e => (e \ "@id").text == "bar") \ "@href").text must be_==("/bar").when(J.running)
    }
  }

  doAfterSpec(J.stop())

}
