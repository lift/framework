/*
 * Copyright 2015 WorldWide Conferencing, LLC
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
package builtin.snippet

import scala.xml._
import org.specs2.mutable.Specification

import common._
import http._
import mockweb._
import MockWeb._
import mocks._
import sitemap.{Menu => _, _}
import util.Helpers.randomString

class MenuSpec extends Specification {
  "Menu Specification".title

  sequential

  case class Param(s: String)

  def mockSiteMap[T](f: (SiteMap => T)): T = {
    val siteMap = SiteMap(
      sitemap.Menu.i("foobar") / "foo" / "bar",
      sitemap.Menu.i("foobaz") / "foo" / "baz",
      sitemap.Menu.param[Param]("foobiz", "foobiz", s => Full(Param(s)), p => p.s) / "foo" / "biz" / *
    )

    f(siteMap)
  }

  def testSiteMap[T](uri: String)(f: => T): T = {
    mockSiteMap { siteMap =>
      val mockReq = new MockHttpServletRequest(uri)

      testS(mockReq) {
        val rules = new LiftRules()
        rules.setSiteMap(siteMap)
        LiftRulesMocker.devTestLiftRulesInstance.doWith(rules) {
          f
        }
      }
    }
  }

  "The built-in Menu snippet" should {
    "Properly render a menu item with default link text" in {
      testSiteMap("http://test.com/foo/baz") {
        S.withAttrs(new UnprefixedAttribute("name", "foobar", Null)) {
          Menu.item(NodeSeq.Empty).toString === """<a href="/foo/bar">foobar</a>"""
        }
      }
    }
    "Properly render a menu item with passed in link text" in {
      testSiteMap("http://test.com/foo/baz") {
        S.withAttrs(new UnprefixedAttribute("name", "foobar", Null)) {
          Menu.item(Text("Foo")).toString === """<a href="/foo/bar">Foo</a>"""
        }
      }
    }
    "Hide item when on same page by default" in {
      testSiteMap("http://test.com/foo/baz") {
        S.withAttrs(new UnprefixedAttribute("name", "foobaz", Null)) {
          Menu.item(NodeSeq.Empty).toString === ""
        }
      }
    }

    "Show text only when on same page using the 'donthide' attribute" in {
      testSiteMap("http://test.com/foo/baz") {
        val donthide = new UnprefixedAttribute("donthide", "true", Null)
        S.withAttrs(new UnprefixedAttribute("name", "foobaz", donthide)) {
          Menu.item(NodeSeq.Empty).toString === "foobaz"
        }
      }
    }
    "Show full item when on same page using the 'linkToSelf' attribute" in {
      testSiteMap("http://test.com/foo/baz") {
        val linkToSelf = new UnprefixedAttribute("linkToSelf", "true", Null)
        S.withAttrs(new UnprefixedAttribute("name", "foobaz", linkToSelf)) {
          Menu.item(NodeSeq.Empty).toString === """<a href="/foo/baz">foobaz</a>"""
        }
      }
    }

    // I wasn't able to figure out a way to mock a Req where S.originalRequest gets set - TN
    // "Properly render a menu item in an ajax request" in {
    //   mockSiteMap { siteMap =>
    //     val session = new LiftSession("", randomString(20), Empty)
    //     val origReq = new MockHttpServletRequest("http://test.com/foo/bar")
    //     testS(origReq, Full(session)) {
    //       LiftRules.setSiteMap(siteMap)
    //       val mockReq = new MockHttpServletRequest("http://test.com/ajax/abc123")
    //       mockReq.headers += "X-Requested-With" -> List("XMLHttpRequest")

    //       testS(mockReq, Full(session)) {
    //         println(s"S.ajax_?: ${S.request.map(_.ajax_?)}")

    //         S.withAttrs(new UnprefixedAttribute("name", "foobar", Null)) {
    //           Menu.item(NodeSeq.Empty).toString === """<a href="/foo/bar">foobar</a>"""
    //         }
    //       }
    //     }
    //   }
    // }
  }
}

