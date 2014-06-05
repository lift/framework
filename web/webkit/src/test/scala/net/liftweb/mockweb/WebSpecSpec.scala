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
package mockweb


import common.Full
import http._
import http.rest._
import json._
import json.JsonDSL._
import mocks.MockHttpServletRequest

/**
* This only exists to keep the WebSpecSpec clean. Normally,
* you could just use "() => bootstrap.Boot.boot".
*/
object WebSpecSpecBoot {
  def boot() {
    // Add this so that withTemplateFor test works
    LiftRules.addToPackages("net.liftweb.mockweb")


    LiftRules.statelessRewrite.append {
      case RewriteRequest(ParsePath(List("test", "stateless"), _, _, _), _, _) => {
        RewriteResponse(List("stateless", "works"))
      }
    }

    LiftRules.statefulRewrite.append {
      case RewriteRequest(ParsePath(List("test", "stateful"), _, _, _), _, _) => {
        RewriteResponse(List("stateful", "works"))
      }
    }
  }
}

/**
 * A test RestHelper to show usage.
 */
object WebSpecSpecRest extends RestHelper  {
  serve {
    case "api" :: "info" :: Nil JsonGet req => {
      ("version" -> "1.0") ~ ("name" -> "WebSpec")
    }
  }
}

/**
 * This spec does double duty as both a spec against the
 * WebSpec trait as well as an example of how to use it.
 */
class WebSpecSpec extends WebSpec(WebSpecSpecBoot.boot _) {
  sequential  // This is important for using SessionVars, etc.

  "WebSpec" should {
    val testUrl = "http://foo.com/test/stateless"

    val testReq = 
      new MockHttpServletRequest("http://foo.com/test/this?foo=bar", "/test")

    // Create a new session for use in the tests
    val testSession = MockWeb.testS(testUrl) {
      S.session
    }

    object TestVar extends SessionVar[String]("Empty")

    "properly set up S with a String url" withSFor(testUrl) in {
      S.request match {
        case Full(req) => req.path.partPath must_== List("stateless", "works")
        case _ =>         failure("No request in S")
      }
    }

    "properly set up S with a String url and session" withSFor(testUrl, testSession) in {
      TestVar("foo!")
      TestVar.is must_== "foo!"
    }

    "properly re-use a provided session" withSFor(testUrl, testSession) in {
      TestVar.is must_== "foo!"
    }      

    "properly set up S with a HttpServletRequest" withSFor(testReq) in {
      S.uri must_== "/this"
      S.param("foo") must_== Full("bar")
    }

    "properly set up a Req with a String url" withReqFor(testUrl) in {
      _.path.partPath must_== List("stateless", "works")
    }

    "properly set up a Req with a String url and context path" withReqFor(testUrl, "/test") in {
      _.path.partPath must_== List("stateless")
    }

    "properly set up a Req with a HttpServletRequest" withReqFor(testReq) in {
      _.uri must_== "/this"
    }

    "properly set a plain text body" withReqFor(testUrl) withPost("This is a test") in {
      req =>
        req.contentType must_== Full("text/plain")
        req.post_? must_== true
        req.body match {
          case Full(body) => (new String(body)) must_== "This is a test"
          case _ =>          failure("No body set")
        }
    }

    "properly set a JSON body" withReqFor(testUrl) withPut(("name" -> "Joe")) in {
      req =>
        req.json_? must_== true
        req.put_? must_== true
        req.json match {
          case Full(jval) => jval must_== JObject(List(JField("name", JString("Joe"))))
          case _ =>          failure("No body set")
        }
    }

    "properly set an XML body" withSFor(testUrl) withPost(<test/>) in {
      S.request match {
        case Full(req) =>
          req.xml_? must_== true
          req.post_? must_== true
          req.xml must_== Full(<test/>)
        case _ => failure("No request found in S")
      }
    }

    "properly mutate the request" withSFor(testUrl) withMods(_.contentType = "application/xml") in {
      (S.request.map(_.xml_?) openOr false) must_== true
    }

    "process a JSON RestHelper Request" withReqFor("http://foo.com/api/info.json") in { req =>
      (WebSpecSpecRest(req)() match {
        case Full(JsonResponse(_, _, _, 200)) => success
        case other =>                            failure("Invalid response : " + other)
      })
    }

    "properly process a template" withTemplateFor("http://foo.com/net/liftweb/mockweb/webspecspectemplate") in {
      case Full(template) => template.toString.contains("Hello, WebSpec!") must_== true
      case other =>          failure("Error on template : " + other)
    }
  }
}
