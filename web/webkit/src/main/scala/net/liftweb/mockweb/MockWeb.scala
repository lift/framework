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

// Don't lose the braces here, we use nested packages in this source
package net.liftweb {
package mockweb {

import javax.servlet.http.HttpServletRequest

import common.{Box,Empty,Full}
import http.{LiftRules,LiftSession,Req,S}
import util.ThreadGlobal
import util.Helpers._
import http.provider.servlet.HTTPRequestServlet
import net.liftweb.mocks.MockHttpServletRequest

import scala.xml.{MetaData,Null}

import org.specs2.mutable._

/**
 * The MockWeb object contains various methods to simplify
 * unit testing in Lift outside of the full-blown testkit
 * stack.
 *
 * There is partial support for configuration defined in the
 * LiftRules object. This includes:
 *
 * <ul>
 *   <li>early</li>
 *   <li>statelessRewrite</li>
 *   <li>statelessTest</li>
 *   <li>statefulRewrite</li>
 * </ul>
 *   
 */
object MockWeb {
  /**
   * Setting this var to <code>true</code>
   * will force all tests to use LiftRules. See
   * useLiftRules for more granular control.
   */
  var useLiftRulesGlobally = false

  object useLiftRules extends ThreadGlobal[Boolean]

  private def liftRulesEnabled = useLiftRulesGlobally || useLiftRules.box == Full(true)

  private def withLiftRules [T] (f : => T) = {
    if (liftRulesEnabled) {
      f
    }
  }


  /**
   * Executes a given function against a new Req constructed
   * from the given url/path String and contextPath. See MockHttpServletRequest.processUrl
   * for details on the url String format, and see
   * testReq(HttpServletRequest) for more details on
   * how the Req is processed.
   */
  def testReq [T](url : String, contextPath : String = "")(f : Req => T) : T = {
    testReq(new MockHttpServletRequest(url, contextPath))(f)
  }
  
  /**
   * Executes a given function against a new Req constructed
   * from the given HttpServletRequest. If useLiftRules
   * is set to true, then LiftRules.early, LiftRules.statelessRewrite,
   * and LiftRules.statelessTest rules are applied.
   */
  def testReq [T](request : HttpServletRequest)(f : Req => T) : T = {
    // TODO : Confirm that we can pass in a null provider without issue
    val req = new HTTPRequestServlet(request, null)
    
    withLiftRules {
      tryo {
        LiftRules.early.toList.foreach(_(req))
      }
    }

    val r =
      if(liftRulesEnabled) {
        // Apply stateless rewrites
        Req(req, LiftRules.statelessRewrite.toList,
            Nil,
            LiftRules.statelessReqTest.toList, System.nanoTime)
      } else {
        Req(req, Nil, System.nanoTime)
      }
    
    f(r)
  }

  /**
   * Sets up S based on the provided url, contextPath
   * and session. You can provide your own session if you
   * want to simulate sharing a session across multiple
   * requests. For example:
   *
   * <pre name="code" class="scala">
   * object testVar extends SessionVar[String]("Empty")
   * 
   * val testSession = testS("http://foo.com/test") {
       testVar("Foo!")
       S.session // returns the current session
     }

     // A second test
     testS("http://foo.com/test2", session = testSession) {
       testVar.is must_== "Foo!"
     }
   * </pre>
   *
   * @param url The url to use for this request. Can either be a
   * full URL, or just the path and queryString. See MockHttpServletRequest.processUrl
   * for more details
   * 
   * @param session The LiftSession to use for this request. If you don't provide
   * one a new one will be created for you
   * 
   * @param contextPath The servlet context path for this request
   * 
   * @param testFunc The function to be executed in the scope of a new S
   */
  def testS [T](url : String, 
                session : Box[LiftSession] = Empty,
                contextPath : String = "")(testFunc : => T) : T =
    testReq(url, contextPath)(realTestS(session)(() => testFunc))

  /**
   * Sets up S based on the provided request
   * and session. You can use this method if you
   * want to do special setup on the request outside
   * of what is handled by the MockHttpServletRequest
   * constructor, or if you want to use a different
   * mock impl.
   *
   * You can provide your own session if you
   * want to simulate sharing a session across multiple
   * requests. See testS(String,String,Box[LiftSession])
   * for an example of this use.
   *
   * Note that if useLiftRules is set to true, then rules like LiftRules.early,
   * LiftRules.statelessTest, etc, will be applied.
   *
   * @param request The request to be used for this test
   * @param session The LiftSession to use for this request. Passing Empty
   * will force creation of a new session
   * @param testFunc The function to be executed in the scope of a new S
   */
  def testS [T](request : HttpServletRequest,
                session : Box[LiftSession])(testFunc : => T) : T = 
    testReq(request)(realTestS(session)(() => testFunc))

  /**
   * Sets up S based on the provided request
   * and a new session.
   */
  def testS [T](request : HttpServletRequest)(testFunc : => T) : T = 
    testReq(request)(realTestS(Empty)(() => testFunc))

  /**
   * This is the common delegate for the testS methods to avoid
   * code duplication.
   */
  private def realTestS [T](newSession : Box[LiftSession])(f : () => T)(req : Req) : T = {
    val session = newSession openOr LiftSession(req)
    S.init(req, session) {
      f()
    }    
  }

  /**
   * This is a utility method to allow you to set the
   * S.currentSnippet method for testing.
   *
   * @param name The snippet name to be tested. For example, &lt;lift:MyWidget.foo/> has a
   * name of MyWidget.foo. You can retrieve this via <code>S.currentSnippet</code> or
   * <code>S.invokedAs</code>
   *
   * @param attrs The list of snippet tag attributes. Defaults to Null. See <code>S.attrs</code>
   * for more details
   *
   * @param f The function to execute in the context of the emulated snippet
   * 
   */
  def withSnippet[T](name : String, attrs : MetaData = Null)(f : => T) : T =
    S.withAttrs(attrs) {
      http.httpPackageProxy.doSnippet(name)(f)
    }
}
}

// This is a bridge to allow use to use the http-private
// S.doSnippet method
package http {
  private[liftweb] object httpPackageProxy {
    def doSnippet[T](name : String)(f : => T) : T = {
      S.doSnippet(name)(f)
    }
  }
}

} // end package net.liftweb

