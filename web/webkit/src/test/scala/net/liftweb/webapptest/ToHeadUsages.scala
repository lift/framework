/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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
package webapptest {

import _root_.org.specs._
import _root_.org.specs.runner.JUnit3
import _root_.org.specs.runner.ConsoleRunner
import _root_.net.sourceforge.jwebunit.junit.WebTester
import _root_.net.liftweb.http._
import _root_.net.liftweb.util._

class ToHeadUsagesTest extends JUnit3(ToHeadUsages)
object ToHeadUsagesRunner extends ConsoleRunner(ToHeadUsages)


object ToHeadUsages extends Specification {
  doBeforeSpec(JettyTestServer.start())
  doAfterSpec(JettyTestServer.stop())

  "lift <head> merger" should {
    "merge <head> from html fragment" >> {
      JettyTestServer.browse(
        "/htmlFragmentWithHead", html =>
         html.getElementByXPath("/html/head/script[@id='fromFrag']") must notBeNull
      )
    }

    "merge <head> from html fragment does not include head element in body" >> {
      JettyTestServer.browse(
        "/htmlFragmentWithHead", html =>
         html.getElementsByXPath("/html/body/script[@id='fromFrag']").size must_== 0
      )
    }

    "merge <head> from snippet" >> {
      JettyTestServer.browse(
        "/htmlSnippetWithHead", html =>
         html.getElementByXPath("/html/head/script[@src='snippet.js']") must notBeNull
      )
    }

    "not merge for bodyless html" >> {
      JettyTestServer.browse(
        "/basicDiv",html => {
          html.getElementById("fruit") must notBeNull
          html.getElementById("bat")   must notBeNull
        }
      )
    }

    "not merge for headless bodyless html" >> {
      JettyTestServer.browse(
        "/h1",html => {
          html.getElementById("h1") must notBeNull
        }
      )
    }

    "not merge for headless body html" >> {
      JettyTestServer.browse(
        "/body_no_head",html => {
          // Note: The XPath expression "html/body/head/div" fails here with
          // HtmlUnit 2.5 since "head" is not recognized as a XHTML element
          // due to its incorrect position (under body instead of directly under html)
          html.getElementsByXPath("/html/body//div").size must_== 1
        }
      )
    }

    "not merge non-html" >> {
      JettyTestServer.browse(
        "/non_html",html => {
          html.getElementById("frog") must notBeNull
        }
      )
    }

  }

  "pages " should {
    "Template finder should recognize entities" >> {
      val ns = TemplateFinder.findAnyTemplate(List("index")).open_!
      val str = AltXML.toXML(ns(0), false, false, false)

      val idx = str.indexOf("&mdash;")
      (idx >= 0) must_== true
    }

    "Template finder should not recognize entities" >> {
      val ns = TemplateFinder.findAnyTemplate(List("index")).open_!
      val str = AltXML.toXML(ns(0), false, true, false)

      val idx = str.indexOf("&mdash;")
      (idx >= 0) must_== false
    }

    /*
    "round trip entities" >> {
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
    "render" >> {
      JettyTestServer.browse(
        "/deferred",html => {
          html.getElementById("second") must notBeNull
        }
      )
    }

    "not deferred not in actor" >> {
      JettyTestServer.browse(
        "/deferred",html => {
          html.getElementByXPath("/html/body/span[@id='whack1']/span[@id='actor_false']") must notBeNull
        }
      )
    }

    "deferred in actor" >> {
      JettyTestServer.browse(
        "/deferred",html => {
          html.getElementByXPath("/html/body/span[@id='whack2']/span[@id='actor_true']") must notBeNull
        }
      )
    }

    "Exclude from context rewriting" >> {
      val first = net.liftweb.http.Req.fixHtml("/wombat",
        <span>
          <a href="/foo" id="foo">foo</a>
          <a href="/bar" id="bar">bar</a>
        </span>
      )

      def excludeBar(in: String): Boolean = in.startsWith("/bar")

      val second = net.liftweb.http.LiftRules.excludePathFromContextPathRewriting.doWith(excludeBar _) {
        net.liftweb.http.Req.fixHtml("/wombat",
          <span>
            <a href="/foo" id="foo">foo</a>
            <a href="/bar" id="bar">bar</a>
          </span>
        )
      }

      ((first \\ "a").filter(e => (e \ "@id").text == "foo") \ "@href").text must_== "/wombat/foo"
      ((first \\ "a").filter(e => (e \ "@id").text == "bar") \ "@href").text must_== "/wombat/bar"
      ((second \\ "a").filter(e => (e \ "@id").text == "foo") \ "@href").text must_== "/wombat/foo"
      ((second \\ "a").filter(e => (e \ "@id").text == "bar") \ "@href").text must_== "/bar"
    }
  }

}

}
}
