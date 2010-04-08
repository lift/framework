/*
 * Copyright 2010 WorldWide Conferencing, LLC
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

package net.liftweb {
package http {
package provider {
package servlet {
package containers {

import _root_.javax.servlet.http.HttpServletRequest

import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.http.provider.servlet._
import _root_.net.liftweb.util._
import Helpers._

/**
 * Jetty7AsyncProvider
 *
 * Implemented by using Jetty 7 Continuation API
 *
 */
class Jetty7AsyncProvider(req: HTTPRequest) extends ServletAsyncProvider {

  private val servletReq = (req.asInstanceOf[HTTPRequestServlet]).req

  private val (hasContinuations_?,
               contSupport,
               getContinuation,
               getAttribute,
               setAttribute,
               suspend,
               setTimeout,
               resume,
               isExpired,
               isResumed) = {
    try {
      val cc = Class.forName("org.eclipse.jetty.continuation.ContinuationSupport")
      val meth = cc.getMethod("getContinuation", classOf[javax.servlet.ServletRequest])
      val cci = Class.forName("org.eclipse.jetty.continuation.Continuation")
      val getAttribute = cci.getMethod("getAttribute", classOf[String])
      val setAttribute = cci.getMethod("setAttribute", classOf[String], classOf[AnyRef])
      val suspend = cci.getMethod("suspend")
      val setTimeout = cci.getMethod("setTimeout", _root_.java.lang.Long.TYPE)
      val resume = cci.getMethod("resume")
      val isExpired = cci.getMethod("isExpired")
      val isResumed = cci.getMethod("isResumed")
      (true, (cc), (meth), (getAttribute), (setAttribute), (suspend), setTimeout, resume, isExpired, isResumed)
    } catch {
      case e =>
        (false, null, null, null, null, null, null, null, null, null)
    }
  }

  def suspendResumeSupport_? : Boolean = hasContinuations_?

  def resumeInfo: Option[Any] =
    if (!hasContinuations_?) None
    else if (Props.inGAE) None
    else {
      val cont = getContinuation.invoke(contSupport, servletReq)
      val ret = getAttribute.invoke(cont, "__liftCometState")
      try {
        setAttribute.invoke(cont, "__liftCometState", null)
        Some(ret)
      }
      catch {
        case e: Exception => None
      }
   }

  def suspend(timeout: Long): RetryState.Value = {
    val cont = getContinuation.invoke(contSupport, servletReq)

    val expired = isExpired.invoke(cont).asInstanceOf[Boolean]
    val resumed = isResumed.invoke(cont).asInstanceOf[Boolean]

    if (expired)
      RetryState.TIMED_OUT
    else if (resumed)
      RetryState.RESUMED
    else {
      setTimeout.invoke(cont, new _root_.java.lang.Long(timeout))
      suspend.invoke(cont)
      RetryState.SUSPENDED
    }
    
  }

  def resume(what: AnyRef): Boolean = {
    val cont = getContinuation.invoke(contSupport, servletReq)
    try {
      setAttribute.invoke(cont, "__liftCometState", what)
      resume.invoke(cont)
      true
    } catch {
      case e => setAttribute.invoke(cont, "__liftCometState", null)
                false
    }
  }


}
}
}
}
}
}
