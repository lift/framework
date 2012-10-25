/*
 * Copyright 2011 WorldWide Conferencing, LLC
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
package http

import mockweb._


import scala.xml.NodeSeq

import common.{ Box, Empty, Full }

/**
* This only exists to keep the WebSpecSpec clean. Normally,
* you could just use "() => bootstrap.Boot.boot".
*/
object HtmlPropertiesSpecBoot {
  def boot() {
    LiftRules.htmlProperties.default.set((_: Req) match {
      case r @ Req("html5" :: _, _, _) =>
        println("Html5 request: " + r)
        Html5Properties(r.userAgent)
      case r                           =>
        println("other request: " + r)
        OldHtmlProperties(r.userAgent)
    })
  }
}

class HtmlPropertiesSpec extends WebSpec(HtmlPropertiesSpecBoot.boot _) {
  sequential

  "LiftRules.htmlProperties.default function" should {
    val testUrl1 = "http://example.com/html5/something"
    val testUrl2 = "http://example.com/anotherurl"

    val session1 = MockWeb.testS(testUrl1)(S.session)
    val session2 = MockWeb.testS(testUrl2)(S.session)

    "set S.htmlProperties to html5 when that is the first request" withSFor(testUrl1, session1) in {
      S.htmlProperties must haveClass[Html5Properties]
    }
    "set S.htmlProperties to xhtml when that is not the first request" withSFor(testUrl2, session1) in {
      S.htmlProperties must haveClass[OldHtmlProperties]
    }
    "set S.htmlProperties to xhtml when that is the first request" withSFor(testUrl2, session2) in {
      S.htmlProperties must haveClass[OldHtmlProperties]
    }
    "set S.htmlProperties to html5 when that is not the first request" withSFor(testUrl1, session2) in {
      S.htmlProperties must haveClass[Html5Properties]
    }
  }
}
