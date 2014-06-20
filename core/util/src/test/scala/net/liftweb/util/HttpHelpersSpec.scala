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

import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification

import HttpHelpers._


/**
 * Systems under specification for HttpHelpers.
 */
object HttpHelpersSpec extends Specification with HttpHelpers with ListHelpers with StringHelpers with XmlMatchers {
  "HttpHelpers Specification".title

  "Http helpers" should {
    "urlEncode and urlDecode functions" >> {
      urlDecode(urlEncode("hello world")) must_== "hello world"
      urlEncode(urlDecode("hello+world")) must_== "hello+world"
    }
    "a paramsToUrlParams function to translate a map of parameters to a URL query" in {
      paramsToUrlParams(List(("firstname", "hello"), ("lastname", "world"))) must_== "firstname=hello&lastname=world"
    }
    "an appendParams function to add parameters to a URL query" in {
      "creating the param list with ? if there are no existing params" in {
        appendParams("www.helloworld.com/params", List(("firstname", "hello"), ("lastname", "world"))) must_==
        "www.helloworld.com/params?firstname=hello&lastname=world"
      }
      "appending the param list with & if there are some already" in {
        appendParams("www.helloworld.com/params?firstname=hello", List(("lastname", "world"))) must_==
        "www.helloworld.com/params?firstname=hello&lastname=world"
      }
      "returning the url if no param list is passed" in {
        appendParams("www.helloworld.com/params", Nil) must_== "www.helloworld.com/params"
      }
    }
    "a couldBeHtml function" in {
      "returning true if there is a pair (Content-Type, text/html)" in {
        couldBeHtml(Map(("Content-Type", "text/html"))) must beTrue
      }
      "returning true if there is a pair (Content-Type, Application/xhtml+xml). The check is case insensitive" in {
        couldBeHtml(Map(("Content-Type", "Application/xhtml+XML"))) must beTrue
      }
      "returning false if the content type is something else (Content-Type, application/jpeg)" in {
        couldBeHtml(Map(("Content-Type", "application/jpeg"))) must beFalse
      }
      "returning true if there is no a pair starting with Content-Type" in {
        couldBeHtml(Map(("no content type", "text/html"))) must beTrue
      }
    }
    "a noHtmlTag" in {
      "returning true if a xml node doesn't contain the html tag" in {
        noHtmlTag(<a><b></b></a>) must beTrue
      }
      "returning false if a xml node contains the html tag" in {
        noHtmlTag(<a><html></html></a>) must beFalse
      }
    }
    "a toHashMap function transforming a Map to a mutable HashMap" in {
      toHashMap(Map(1 -> 2, 3 -> 4)) must haveClass[scala.collection.mutable.HashMap[Int, Int]]
    }
    "an insureField function" in {
      "checking that the appropriate fields are in the header" in {
        insureField(List(("name", "hello")), List(("name", "hello"))) must_== List(("name", "hello"))
      }
      "checking that the appropriate fields are in the header, adding them if necessary" in {
        insureField(List(("name2", "hello")), List(("name", "hello"))) must_== List(("name", "hello"), ("name2", "hello"))
      }
    }
    "an implicit definition to transform a pair to an UnprefixedAttribute" in {
      pairToUnprefixed(("value", 1)).apply("value").toString must_== "1"
    }
    "a findOrAddId function" in {
      "returning an element and its id if found" in { findOrAddId(<a id="1"></a>) must_== (<a id="1"></a>, "1") }
      "returning an element with a random id if not found" in {
        val (e, id) = findOrAddId(<a></a>)
        e must \("@id")
        // id must beMatching("R\\[a-zA-Z0-9]*")
      }
    }
  }
 // currentSus is no longer part of Specification in 1.6  def provide(e: =>Example) = { currentSus.verb += " provide"; e }
}

