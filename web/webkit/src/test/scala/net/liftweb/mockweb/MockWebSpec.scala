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

import javax.servlet.http.Cookie

import scala.xml.{Null,Text,UnprefixedAttribute}

import common._
import http._
import http.provider.servlet.HTTPRequestServlet
import mocks.MockHttpServletRequest
import util._

import org.specs._
import org.specs.runner._

class MockWebSpecTest extends Runner(MockWebSpec) with JUnit with Console

/**
 * This spec does double duty as both a spec against the
 * MockWeb object as well as an example of how to use it.
 */
object MockWebSpec extends Specification {
  import MockWeb._

// TODO : Uncomment this code when LiftRules can be scoped
//  // Global LiftRules setup
//  LiftRules.statelessRewrite.append {
//    case RewriteRequest(ParsePath(List("test", "stateless"), _, _, _), _, _) => {
//      RewriteResponse(List("stateless", "works"))
//    }
//  }
//
//  LiftRules.statefulRewrite.append {
//    case RewriteRequest(ParsePath(List("test", "stateful"), _, _, _), _, _) => {
//      RewriteResponse(List("stateful", "works"))
//    }
//  }
//
//  LiftRules.early.append {
//    req => 
//      req match {
//        case httpReq : HTTPRequestServlet => {
//          httpReq.req match {
//            case mocked : MockHttpServletRequest => {
//              mocked.remoteAddr = "1.2.3.4"
//            }
//            case _ => println("Not a mocked request?")
//          }
//        }
//        case _ => println("Not a servlet request?")
//      }
//  }

  "MockWeb" should {
    shareVariables() // Avoid setting up LiftRules multiple times

    "provide a Req corresponding to a string url" in {
      testReq("http://foo.com/test/this?a=b&a=c", "/test") {
        req => 
          req.uri must_== "/this"
          req.params("a") must_== List("b","c")
      }
    }

    "provide a Req corresponding to a HttpServletRequest" in {
      val mockReq = 
        new MockHttpServletRequest("http://foo.com/test/this", "/test")

      mockReq.method = "POST"

      import json.JsonDSL._

      mockReq.body = ("name" -> "joe") ~ ("age" -> 35)

      testReq(mockReq) { 
        req =>
          req.json_? must_== true
      }
    }

// TODO : Uncomment this code when LiftRules can be scoped
//    "process LiftRules.early when configured" in {
//      useLiftRules.doWith(true) {
//        testReq("http://foo.com/test/this") {
//          req => req.remoteAddr must_== "1.2.3.4"
//        }
//      }
//    }
//
//    "process LiftRules stateless rewrites when configured" in {
//      useLiftRules.doWith(true) {
//        testReq("http://foo.com/test/stateless") {
//          req => req.path.partPath must_== List("stateless", "works")
//        }
//      }
//    }

    "initialize S based on a string url" in {
      testS("http://foo.com/test/that?a=b&b=c") {
        S.param("b") must_== Full("c")
      }
    }

    "initialize S based on a HttpServletRequest" in {
      val mockReq = 
        new MockHttpServletRequest("http://foo.com/test/this?foo=bar", "/test")
      
      testS(mockReq) {
        S.param("foo") must_== Full("bar")

        S.uri must_== "/this"
      }
    }

// TODO : Uncomment this code when LiftRules can be scoped
//    "process S with stateless rewrites" in {
//      useLiftRules.doWith(true) {
//        testS("http://foo.com/test/stateless") {
//          S.request.foreach(_.path.partPath must_== List("stateless", "works"))
//        }
//      }      
//    }
//
//    "process S with stateful rewrites" in {
//      useLiftRules.doWith(true) {
//        testS("http://foo.com/test/stateful") {
//          S.request.foreach(_.path.partPath must_== List("stateful", "works"))
//        }
//      }      
//    }

    "emulate a snippet invocation" in {
        testS("http://foo.com/test/stateful") {
          withSnippet("MyWidget.foo", new UnprefixedAttribute("bar", Text("bat"), Null)) {
            S.currentSnippet must_== Full("MyWidget.foo")
            S.attr("bar") must_== Full("bat")
          }
        }
    }


    "simplify shared sessions" in {
      object testVar extends SessionVar[String]("Empty")
   
      val session = testS("http://foo.com/test") {
        testVar("Foo!")
        S.session // returns the current session
      }

      // A second test
      testS("http://foo.com/test2", session) {
        testVar.is must_== "Foo!"
      }
    }

  }
}
