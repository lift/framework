/*
 * Copyright 2010-2011 WorldWide Conferencing, LLC
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

import java.io.ByteArrayInputStream

import scala.xml.XML

import org.specs2.matcher.XmlMatchers

import org.mockito.Mockito._

import org.specs2.mutable.Specification
import org.specs2.mock.Mockito
import org.specs2.specification.Scope

import common._
import json.JsonDSL._
import json.JsonParser
import util.Helpers.tryo

import provider._

/**
 * System under specification for Req.
 */
object ReqSpec extends Specification with XmlMatchers with Mockito {
  "Req Specification".title

  private val iPhoneUserAgents = 
    List("Mozilla/5.0 (iPhone Simulator; U; CPU iPhone OS 3_0 like Mac OS X; en-us) AppleWebKit/528.18 (KHTML, like Gecko) Version/4.0 Mobile/7A341 Safari/528.16",
         "Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_2_1 like Mac OS X; en-us) AppleWebKit/533.17.9 (KHTML, like Gecko) Version/5.0.2 Mobile/8C148 Safari/6533.18.5")

  private val iPadUserAgents = 
    List("Mozilla/5.0 (iPad; U; CPU OS 3_2 like Mac OS X; en-us) AppleWebKit/531.21.10 (KHTML, like Gecko) Version/4.0.4 Mobile/7B367 Safari/531.21.10",
         "Mozilla/5.0 (iPad; U; CPU OS 4_2_1 like Mac OS X; en-us) AppleWebKit/533.17.9 (KHTML, like Gecko) Version/5.0.2 Mobile/8C148 Safari/6533.18.5")

  private val ieUserAgents =
    "Mozilla/5.0 (Windows; U; MSIE 6.0; Windows NT 5.1; SV1; .NET CLR 2.0.50727)" ::
    "Mozilla/5.0 (Windows; U; MSIE 7.0; Windows NT 6.0; en-US)" ::
    "Mozilla/5.0 (compatible; MSIE 8.0; Windows NT 6.1; Trident/4.0; GTB7.4; InfoPath.2; SV1; .NET CLR 3.3.69573; WOW64; en-US)" ::
    "Mozilla/5.0 (Windows; U; MSIE 9.0; WIndows NT 9.0; en-US))" ::
    "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; WOW64; Trident/6.0)" ::
    "Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; rv:11.0) like Gecko" ::
    Nil

  "Req" should {
    "recognize safari 5" in {
      val uac = new UserAgentCalculator {
        def userAgent = Full("Mozilla/5.0 (Windows; U; Windows NT 6.1; zh-HK) AppleWebKit/533.18.1 (KHTML, like Gecko) Version/5.0.2 Safari/533.18.5")
      }
      uac.safariVersion.openOrThrowException("legacy code") must_== 5
    }

    "Do the right thing with iPhone" in {
      iPhoneUserAgents map {
        agent => {
          val uac = new UserAgentCalculator {
            def userAgent = Full(agent)
          }
          uac.isIPhone must_== true
          uac.isIPad must_== false
        }
      }

      success
    }

    "Do the right thing with iPad" in {
      iPadUserAgents map {
        agent => {
          val uac = new UserAgentCalculator {
            def userAgent = Full(agent)
          }
          uac.isIPhone must_== false
          uac.isIPad must_== true
        }
      }

      success
    }

    "Correctly recognize IE versions 6-11" in {
      val ieVersions = ieUserAgents.flatMap { ieUserAgent =>
        val userAgentCalculator = new UserAgentCalculator {
          def userAgent = Full(ieUserAgent)
        }

        userAgentCalculator.ieVersion
      }

      ieVersions must_== List(6, 7, 8, 9, 10, 11)
    }

    trait mockReq extends Scope {
      val mockHttpRequest = mock[HTTPRequest]
      def paramCalcInfo = ParamCalcInfo(Nil, Map.empty, Nil, Full(BodyOrInputStream(new ByteArrayInputStream(bodyBytes))))

      def bodyBytes: Array[Byte]

      def req(contentType: String) = {
        new Req(
          Req.NilPath, "/", GetRequest,
          Full(contentType),
          mockHttpRequest,
          0l, 1l, true,
          () => paramCalcInfo,
          Map.empty
        )
      }
    }

    class mockJsonReq(jsonString: String = """{ "booyan": "shazam", "booyak": 5, "bazam": 2.5 }""") extends mockReq {
      val testJson = jsonString
      val parsedJson = tryo(JsonParser.parse(jsonString)) openOr json.JsonAST.JNothing

      def bodyBytes = {
        testJson.getBytes("UTF-8")
      }
    }

    class mockXmlReq(xmlString: String = """<boom><slam attribute="do it">Oh yeah</slam></boom>""") extends mockReq {
      val testXml = xmlString
      val parsedXml = tryo(XML.loadString(xmlString)) openOr "totally failed"

      def bodyBytes = {
        testXml.getBytes("UTF-8")
      }
    }

    "when trying to JSON parse the request body" in {
      "with an invalid Content-Type should return a Failure" in new mockJsonReq {
        req("text/plain").json should beAnInstanceOf[Failure]
      }

      "with an application/json Content-Type should return the result of parsing the JSON" in new mockJsonReq {
        req("application/json").json should_== Full(parsedJson)
      }

      "with a text/json Content-Type should return the result of parsing the JSON" in new mockJsonReq {
        req("text/json").json should_== Full(parsedJson)
      }

      "with invalid JSON and a text/json Content-Type should return a Failure" in new mockJsonReq("epic fail") {
        req("text/json").json should beAnInstanceOf[Failure]
      }
    }

    "when forcing a request body JSON parse with forcedBodyAsJson" in {
      "with an invalid Content-Type should return the result of parsing the JSON" in new mockJsonReq {
        req("text/plain").forcedBodyAsJson should_== Full(parsedJson)
      }

      "with an application/json Content-Type should return the result of parsing the JSON" in new mockJsonReq {
        req("application/json").forcedBodyAsJson should_== Full(parsedJson)
      }

      "with a text/json Content-Type should return the result of parsing the JSON" in new mockJsonReq {
        req("text/json").forcedBodyAsJson should_== Full(parsedJson)
      }

      "with invalid JSON should return a Failure" in new mockJsonReq("epic fail") {
        req("text/json").json should beAnInstanceOf[Failure]
      }
    }

    "when trying to XML parse the request body" in {
      "with an invalid Content-Type should return a Failure" in new mockXmlReq {
        req("text/plain").xml should beAnInstanceOf[Failure]
      }

      "with an application/xml Content-Type should return the result of parsing the JSON" in new mockXmlReq {
        req("application/xml").xml should_== Full(parsedXml)
      }

      "with a text/xml Content-Type should return the result of parsing the JSON" in new mockXmlReq {
        req("text/xml").xml should_== Full(parsedXml)
      }

      "with invalid XML and a text/xml Content-Type should return a Failure" in new mockXmlReq("epic fail") {
        req("text/xml").forcedBodyAsXml should beAnInstanceOf[Failure]
      }
    }

    "when forcing a request body XML parse with forcedBodyAsXml" in {
      "with an invalid Content-Type should return the result of parsing the JSON" in new mockXmlReq {
        req("text/plain").forcedBodyAsXml should_== Full(parsedXml)
      }

      "with an application/json Content-Type should return the result of parsing the JSON" in new mockXmlReq {
        req("application/xml").forcedBodyAsXml should_== Full(parsedXml)
      }

      "with a text/json Content-Type should return the result of parsing the JSON" in new mockXmlReq {
        req("text/xml").forcedBodyAsXml should_== Full(parsedXml)
      }

      "with invalid XML should return a Failure" in new mockXmlReq("epic fail") {
        req("text/palin").forcedBodyAsXml should beAnInstanceOf[Failure]
        req("text/xml").forcedBodyAsXml should beAnInstanceOf[Failure]
      }
    }
  }
}

