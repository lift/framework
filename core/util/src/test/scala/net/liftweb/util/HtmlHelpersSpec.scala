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
package util

import xml._

import org.specs2.mutable.Specification

import common._

/**
 * Systems under specification for HtmlHelpers.
 */
object HtmlHelpersSpec extends Specification with HtmlHelpers {
  "HtmlHelpers Specification".title

  "findBox" should {
    "find an id" in {
      val xml = <foo><bar/>Dog<b><woof id="3"/></b></foo>

      findBox(xml) {
        e => e.attribute("id").
        filter(_.text == "3").
        map(i => e)
      }.openOrThrowException("Test") must ==/ (<woof id="3"/>)
    }

    "not find an ide" in {
      val xml = <foo><bar/>Dog<b><woof ide="3"/></b></foo>

      findBox(xml) {
        e => e.attribute("id").
        filter(_.text == "3").map(i => e)
      } must_== Empty
    }


    "not find a the wrong id" in {
      val xml = <foo><bar/>Dog<b><woof ide="4"/></b></foo>

      findBox(xml) {
        e => e.attribute("id").
        filter(_.text == "3").map(i => e)
      } must_== Empty
    }
  }

  "findOption" should {
    "find an id" in {
      val xml = <foo><bar/>Dog<b><woof id="3"/></b></foo>

      findOption(xml) {
        e => e.attribute("id").
        filter(_.text == "3").map(i => e)
      }.get must ==/ (<woof id="3"/>)
    }

    "not find an ide" in {
      val xml = <foo><bar/>Dog<b><woof ide="3"/></b></foo>

      findOption(xml) {
        e => e.attribute("id").
        filter(_.text == "3").map(i => e)
      } must_== None
    }


    "not find a the wrong id" in {
      val xml = <foo><bar/>Dog<b><woof ide="4"/></b></foo>

      findOption(xml) {
        e => e.attribute("id").
        filter(_.text == "3").map(i => e)
      } must_== None
    }
  }

  "findId" should {
    val xml =
      <whoa>
        <thing id="boom">Boom</thing>
        <other-thing id="other-boom">Other boom</other-thing>
      </whoa>

    "find the element with a requested id in a NodeSeq" in {
      findId(xml, "boom") must beLike {
        case Some(element) =>
          element must ==/(<thing id="boom">Boom</thing>)
      }
    }

    "provide a None if a requested id is not found in the NodeSeq" in {
      findId(xml, "explode") must beNone
    }

    "find the first id in a NodeSeq when no id is requested" in {
      findId(xml) must_== Full("boom")
    }

    "provide an Empty if no ide is foud in a NodeSeq when no id is requested" in {
      findId(<test />) must_== Empty
    }
  }

  "head removal" should {
    "remove <head>" in {
      Helpers.stripHead(<head><i>hello</i></head>) must ==/(<i>hello</i>)
    }

    "ignore non-head" in {
      Helpers.stripHead(<head3><i>hello</i></head3>) must ==/(<head3><i>hello</i></head3>)
    }

    "String subhead" in {
      Helpers.stripHead(<head3><i><head>hello</head></i></head3>) must ==/(<head3><i>hello</i></head3>)
    }
  }

  "removeAttribute" should {
    val element = <boom attribute="hello" otherAttribute="good-bye" />

    "remove the specified attribute from a provided element" in {
      val removed = removeAttribute("attribute", element)

      removed must ==/(<boom otherAttribute="good-bye" />)
    }

    "remove the specified attribute from a provided MetaData list" in {
      val removed = removeAttribute("attribute", element.attributes)

      (removed("attribute") must_== null) and
      (removed("otherAttribute") must_== Text("good-bye"))
    }
  }

  "addCssClass" should {
    "add a new attribute if no class attribute exists" in {
      (addCssClass("foo", <b/>) \ "@class").text must_== "foo"
    }

    "append to an existing class attribute if it already exists" in {
      (addCssClass("foo", <b class="dog"/>) \ "@class").text must_== "dog foo"
    }

  }

}


