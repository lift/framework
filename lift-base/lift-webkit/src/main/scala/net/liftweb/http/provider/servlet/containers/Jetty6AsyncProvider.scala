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
 * Jetty6AsyncProvider
 *
 * Implemented by using Jetty 6 Continuation API
 *
 */
class Jetty6AsyncProvider(req: HTTPRequest) extends ServletAsyncProvider with Loggable {

  private val servletReq = (req.asInstanceOf[HTTPRequestServlet]).req

  private val (hasContinuations_?,
               contSupport,
               getContinuation,
               getObject,
               setObject,
               suspend,
               resume,
               isPending) = {
    try {
      val cc = Class.forName("org.mortbay.util.ajax.ContinuationSupport")
      val meth = cc.getMethod("getContinuation", classOf[HttpServletRequest], classOf[AnyRef])
      val cci = Class.forName("org.mortbay.util.ajax.Continuation")
      val getObj = cci.getMethod("getObject")
      val setObj = cci.getMethod("setObject", classOf[AnyRef])
      val suspend = cci.getMethod("suspend", _root_.java.lang.Long.TYPE)
      val resume = cci.getMethod("resume")
      val isPending = cci.getMethod("isPending")
      (true, (cc), (meth), (getObj), (setObj), (suspend), resume, isPending)
    } catch {
      case e => (false, null, null, null, null, null, null, null)
    }
  }

  def suspendResumeSupport_? : Boolean = hasContinuations_?

  def resumeInfo: Option[Any] =
    if (!hasContinuations_?) None
    else if (Props.inGAE) None
    else {
      val cont = getContinuation.invoke(contSupport, servletReq, LiftRules)
      val ret = getObject.invoke(cont)
      try {
        setObject.invoke(cont, null)
        Some(ret)
      }
      catch {
        case e: Exception => None
      }
   }


  def suspend(timeout: Long): RetryState.Value = {
    try {
      val cont = getContinuation.invoke(contSupport, servletReq, LiftRules)
      logger.trace("About to suspend continuation")
      val b = suspend.invoke(cont, new _root_.java.lang.Long(timeout)).asInstanceOf[Boolean]
      if (!b) RetryState.TIMED_OUT else RetryState.RESUMED
    } catch {
      case e: _root_.java.lang.reflect.InvocationTargetException if e.getCause.getClass.getName.endsWith("RetryRequest") =>
        throw e.getCause
    }
  }

  def resume(what: AnyRef): Boolean = {
    val cont = getContinuation.invoke(contSupport, servletReq, LiftRules)
    cont.synchronized {
      val pending = isPending.invoke(cont).asInstanceOf[Boolean]
      if (pending) {
       setObject.invoke(cont, what)
       resume.invoke(cont)
      }
      pending
    }
  }


}
}
}
}
}
}
