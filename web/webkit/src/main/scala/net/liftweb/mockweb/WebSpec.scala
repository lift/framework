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

import javax.servlet.http.HttpServletRequest

import scala.xml.NodeSeq

import org.specs2.matcher.XmlMatchers
import org.specs2.mutable._
import org.specs2.execute.Result

import common.{Box,Empty,Full}
import http._
import net.liftweb.json.JsonAST._
import mocks.MockHttpServletRequest


/**
 * This trait provides Lift-specific extensions to the Specification
 * base trait to simplify unit testing of your code. In addition to
 * the Scaladoc, Please see the
 * source to WebSpecSpec.scala for an example of how to use this. This class
 * allows you to optionally configure LiftRules for your spec. LiftRules rules
 * are always used, so if you don't want that to happen you can just use MockWeb directly
 * in your own custom spec and simply omit any LiftRules setup.
 *
 * @param boot defines a method that is called prior to
 * testing to set up the Lift environment. This is where you'll
 * initialize LiftRules, Mapper, etc. The simplest approach
 * is to just point this at your Boostrap.boot method.
 */
abstract class WebSpec(boot : () => Any = () => {}) extends Specification with XmlMatchers {
  /**
   * This is our private spec instance of Liftrules. Everything we run will
   * use this instance instead of the global instance
   */
  private val liftRules = new LiftRules()

  LiftRulesMocker.devTestLiftRulesInstance.doWith(liftRules) {
    boot()
  }

  /**
   * A class that bridges between the description string
   * and classes that provide the Lift-specific wrapping
   * of the Expectation code. The method names here
   * should be self-explanatory. For methods that take a
   * Box[LiftSession], an Empty Box will result in the
   * creation of a new Session.
   */
  class WebSpecBridge (description : String) {
    def withSFor (url : String, session : Box[LiftSession] = Empty, contextPath : String = "") =
      new SessionSpecification(description, url, session, contextPath)

    def withSFor (url : String, session : LiftSession) : SessionSpecification =
      new SessionSpecification(description, url, Box.!!(session), "")

    def withSFor (req : HttpServletRequest) =
      new SessionSpecification(description, req, Empty)

    def withSFor (req : HttpServletRequest, session : Box[LiftSession])  =
      new SessionSpecification(description, req, session)

    def withSFor (req : HttpServletRequest, session : LiftSession) =
      new SessionSpecification(description, req, Box.!!(session))

    def withReqFor (url : String, contextPath : String = "") =
      new ReqSpecification(description, url, contextPath)

    def withReqFor (req : HttpServletRequest) = 
      new ReqSpecification(description, req)

    def withTemplateFor (url : String, session : Box[LiftSession] = Empty, contextPath : String = "") =
      new TemplateSpecification(description, url, session, contextPath)

    def withTemplateFor (url : String, session : LiftSession) =
      new TemplateSpecification(description, url, Box.!!(session), "")

    def withTemplateFor (req : HttpServletRequest) =
      new TemplateSpecification(description, req, Empty)

    def withTemplateFor (req : HttpServletRequest, session : Box[LiftSession])  =
      new TemplateSpecification(description, req, session)

    def withTemplateFor (req : HttpServletRequest, session : LiftSession) =
      new TemplateSpecification(description, req, Box.!!(session))
  }

  /**
   * Converts a String description into a WebSpecBridge that can
   * then be used to set up either an S or Req instance.
   */
  implicit def strToWebSpecBridge (description : String) =
    new WebSpecBridge(description)

  /**
   * A comon trait to provide utility methods for mutating the
   * underlying HttpServletRequest.
   */
  trait ModifiableRequest [T <: ModifiableRequest[T]] {
    // Make sure that our return values are for the supertype, not ModifiableRequest
    self : T =>

    val req : HttpServletRequest

    /**
     * Modifies the request to POST the given request body text. Optionally,
     * you can set the content type (defaults to "text/plain")
     */
    def withPost (text : String, contentType : String = "text/plain") : T =
      withMods { mockReq =>
        mockReq.body = text
        mockReq.contentType = contentType
        mockReq.method = "POST"
      }

    /**
     * Modifies the request to POST the given request body JSON.
     */
    def withPost (jval : JValue) = withMods { mockReq =>
      mockReq.body = jval
      mockReq.method = "POST"
    }

    /**
     * Modifies the request to POST the given request body XML.
     */    
    def withPost (node : NodeSeq) = withMods { mockReq =>
      mockReq.body = node
      mockReq.method = "POST"
    }

    /**
     * Modifies the request to PUT the given request body text. Optionally,
     * you can set the content type (defaults to "text/plain")
     */
    def withPut (text : String, contentType : String = "text/plain") =
      withMods { mockReq =>
        mockReq.body = text
        mockReq.contentType = contentType
        mockReq.method = "PUT"
      }

    /**
     * Modifies the request to PUT the given request body JSON.
     */
    def withPut (jval : JValue) = withMods { mockReq =>
      mockReq.body = jval
      mockReq.method = "PUT"
    }

    /**
     * Modifies the request to PUT the given request body XML.
     */    
    def withPut (node : NodeSeq) = withMods { mockReq =>
      mockReq.body = node
      mockReq.method = "PUT"
    }

    /**
     * Allows you to specify your own modification function for the servlet request
     * prior to initialization.
     */
    def withMods (f : MockHttpServletRequest => Unit) : T = req match {
      case r : MockHttpServletRequest => f(r); this
      case _ => throw new IllegalArgumentException("We can only mutate MockHttpServletRequest instances")
    }
  }

  /**
   * This class provides a wrapper to test methods that require an
   * initialized S.
   */
  class SessionSpecification (description : String, 
                              val req : HttpServletRequest, 
                              session : Box[LiftSession]) extends ModifiableRequest[SessionSpecification] {
    def this (description : String, url : String, session : Box[LiftSession], contextPath : String) =
      this(description, new MockHttpServletRequest(url, contextPath), session)

    def in(expectations : => Result) =
      exampleFactory newExample(description, {
        LiftRulesMocker.devTestLiftRulesInstance.doWith(liftRules) {
          MockWeb.useLiftRules.doWith(true) {
            MockWeb.testS(req, session) {
              expectations
            }
          }
        }
      })
  }

  /**
   * This class provides a wrapper to test methods that require an
   * initialized Req.
   */
  class ReqSpecification (description : String, 
                          val req : HttpServletRequest) extends ModifiableRequest[ReqSpecification] {
    def this (description : String, url : String, contextPath : String) =
      this(description, new MockHttpServletRequest(url, contextPath))

    def in(expectations : Req => Result) =
      exampleFactory newExample(description, {
        LiftRulesMocker.devTestLiftRulesInstance.doWith(liftRules) {
          MockWeb.useLiftRules.doWith(true) {
            MockWeb.testReq(req)(expectations)
          }
        }
      })
  }

  /**
   * This class provides a wrapper to test methods that require
   * a processed template.
   */
  class TemplateSpecification (description : String,
                               val req : HttpServletRequest,
                               session : Box[LiftSession]) extends ModifiableRequest[TemplateSpecification] {
    def this (description : String, url : String, session : Box[LiftSession], contextPath : String) =
      this(description, new MockHttpServletRequest(url, contextPath), session)

    def in(expectations : Box[NodeSeq] => Result) =
      exampleFactory.newExample(description, {
        LiftRulesMocker.devTestLiftRulesInstance.doWith(liftRules) {
          MockWeb.useLiftRules.doWith(true) {
            MockWeb.testS(req, session) {
              S.request match {
                case Full(sReq) => expectations(S.runTemplate(sReq.path.partPath))
                case other => failure("Error: withTemplateFor call did not result in " +
                  "request initialization (S.request = " + other + ")")
              }
            }
          }
        }
      })
  }
}
