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
package provider 
package servlet 
package containers 

import javax.servlet.http.HttpServletRequest

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.provider._
import net.liftweb.http.provider.servlet._
import net.liftweb.util._
import Helpers._


object Jetty6AsyncProvider extends AsyncProviderMeta {
  // contSupport below gets inferred as a Class[?0] existential.
  import scala.language.existentials

  private lazy val (hasContinuations_?,
                    contSupport,
                    getContinuation,
                    getObject,
                    setObject,
                    suspendMeth,
                    resumeMeth,
                    isPending) = {
    try {
      val cc = Class.forName("org.mortbay.util.ajax.ContinuationSupport")
      val meth = cc.getMethod("getContinuation", classOf[HttpServletRequest], classOf[AnyRef])
      val cci = Class.forName("org.mortbay.util.ajax.Continuation")
      val getObj = cci.getMethod("getObject")
      val setObj = cci.getMethod("setObject", classOf[AnyRef])
      val suspend = cci.getMethod("suspend", java.lang.Long.TYPE)
      val resume = cci.getMethod("resume")
      val isPending = cci.getMethod("isPending")
      (true, (cc), (meth), (getObj), (setObj), (suspend), resume, isPending)
    } catch {
      case e: Exception => (false, null, null, null, null, null, null, null)
    }
  }

  def suspendResumeSupport_? : Boolean = hasContinuations_?

  /**
   * return a function that vends the ServletAsyncProvider
   */
  def providerFunction: Box[HTTPRequest => ServletAsyncProvider] =
    Full(req => new Jetty6AsyncProvider(req)).
  filter(i => suspendResumeSupport_?)


}

/**
 * Jetty6AsyncProvider
 *
 * Implemented by using Jetty 6 Continuation API
 *
 */
class Jetty6AsyncProvider(req: HTTPRequest) extends ServletAsyncProvider with Loggable {

  import Jetty6AsyncProvider._

  private lazy val servletReq = (req.asInstanceOf[HTTPRequestServlet]).req

  def suspendResumeSupport_? : Boolean = hasContinuations_?

  def resumeInfo: Option[(Req, LiftResponse)] =
    if (!hasContinuations_?) None
    else if (Props.inGAE) None
    else {
      val cont = getContinuation.invoke(contSupport, servletReq, LiftRules)
      val ret = getObject.invoke(cont)
      try {
        setObject.invoke(cont, null)
        ret match {
          case (r: Req, lr: LiftResponse) => Some(r -> lr)
          case _ => None
        }
      }
      catch {
        case e: Exception => None
      }
   }


  def suspend(timeout: Long): RetryState.Value = {
    try {
      val cont = getContinuation.invoke(contSupport, servletReq, LiftRules)
      logger.trace("About to suspend continuation")
      val b = suspendMeth.invoke(cont, new java.lang.Long(timeout)).asInstanceOf[Boolean]
      if (!b) RetryState.TIMED_OUT else RetryState.RESUMED
    } catch {
      case e: java.lang.reflect.InvocationTargetException if e.getCause.getClass.getName.endsWith("RetryRequest") =>
        throw e.getCause
    }
  }

  def resume(what: (Req, LiftResponse)): Boolean = {
    val cont = getContinuation.invoke(contSupport, servletReq, LiftRules)
    cont.synchronized {
      logger.trace("In resume on Jetty 6")
      val pending = isPending.invoke(cont).asInstanceOf[Boolean]
      if (pending) {
       setObject.invoke(cont, what)
       resumeMeth.invoke(cont)
      }
      pending
    }
  }
}
