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
package http 
package rest 

import scala.collection.mutable.{HashMap, ListBuffer}
import net.liftweb.common._
import net.liftweb.actor._
import net.liftweb.util._

final private[http] case class ContinuationException(req: Req, session: Box[LiftSession], f: ((=> LiftResponse) => Unit) => Unit) extends LiftFlowOfControlException("Continuation")

/**
 * Provides a generic way of sending asynchronous response to HTTP clients. If
 * the underlying web container does not support continuations the asynchronous
 * nature is achieved using locks.
 */
object RestContinuation {

  /**
   * Process a request asynchronously.  If your web container supports
   * Async calls/Continuations (e.g., Jetty 6, Jetty 7, and Servlet 3.0
   * containers including Jetty 8 and Glassfish), the thread will not
   * block until there's a response.  The parameter is a function
   * that takes a function as it's parameter.  The function is invoked
   * when the calculation response is ready to be rendered:
   * <code class="scala"><pre>
   * RestContinuation.async {
   *   reply => {
   *     myActor ! DoCalc(123, answer => reply{XmlResponse(<i>{answer}</i>)})
   *   }
   * }
   *
   * class MyActor {
   *   def lowPriority = {
   *     case DoCalc(value, whenDone) => whenDone(value * 10)
   *   }
   * }
   * </pre></code>
   *
   * Alternatively, from RestHelper:
   * <code class="scala"><pre>
   * serve {
   *   case "api" :: id _ Get _ => RestContinuation.async {
   *      reply => for {i <- longCalc(id)} reply(<x>{i}</x>)
   *   }
   * }
   * </pre></code>
   *
   * The body of the function will be executed on a separate thread.
   * When the answer is ready, apply the reply function... the function
   * body will be executed in the scope of the current request (the
   * current session and the current Req object).
   */
  def async(f: ((=> LiftResponse) => Unit) => Unit): Nothing = {
    throw new ContinuationException(CurrentReq.value, S.session, f)
  }


}


object ContinuationsStore extends 
  SessionVar[HashMap[ContinuationKey, Continuation]](new HashMap[ContinuationKey, Continuation])

trait Continuation {
  def tryRespond: Box[LiftResponse]
}
case class ContinuationKey(path: ParsePath, reqType: RequestType)

