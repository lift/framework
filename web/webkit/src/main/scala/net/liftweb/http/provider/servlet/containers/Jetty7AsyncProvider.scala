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


object Jetty7AsyncProvider extends AsyncProviderMeta {
  // contSupport below gets inferred as a Class[?0] existential.
  import scala.language.existentials

  private val (hasContinuations_?,
               contSupport,
               getContinuation,
               getAttribute,
               setAttribute,
               suspendMeth,
               setTimeout,
               resumeMeth,
               isExpired,
               isResumed) = {
    try {
      val cc = Class.forName("org.eclipse.jetty.continuation.ContinuationSupport")
      val meth = cc.getMethod("getContinuation", classOf[javax.servlet.ServletRequest])
      val cci = Class.forName("org.eclipse.jetty.continuation.Continuation")
      val getAttribute = cci.getMethod("getAttribute", classOf[String])
      val setAttribute = cci.getMethod("setAttribute", classOf[String], classOf[AnyRef])
      val suspend = cci.getMethod("suspend")
      val setTimeout = cci.getMethod("setTimeout", java.lang.Long.TYPE)
      val resume = cci.getMethod("resume")
      val isExpired = cci.getMethod("isExpired")
      val isResumed = cci.getMethod("isResumed")
      (true, (cc), (meth), (getAttribute), (setAttribute), (suspend), setTimeout, resume, isExpired, isResumed)
    } catch {
      case e: Exception =>
        (false, null, null, null, null, null, null, null, null, null)
    }
  }

  def suspendResumeSupport_? : Boolean = hasContinuations_?


  /**
   * return a function that vends the ServletAsyncProvider
   */
  def providerFunction: Box[HTTPRequest => ServletAsyncProvider] =
    Full(req => new Jetty7AsyncProvider(req)).
  filter(i => suspendResumeSupport_?)
}

/**
 * Jetty7AsyncProvider
 *
 * Implemented by using Jetty 7 Continuation API
 *
 */
class Jetty7AsyncProvider(req: HTTPRequest) extends ServletAsyncProvider {

  import Jetty7AsyncProvider._

  private val servletReq = (req.asInstanceOf[HTTPRequestServlet]).req


  def suspendResumeSupport_? : Boolean = hasContinuations_?

  def resumeInfo: Option[(Req, LiftResponse)] =
    if (!hasContinuations_?) None
    else if (Props.inGAE) None
    else {
      val cont = getContinuation.invoke(contSupport, servletReq)
      val ret = getAttribute.invoke(cont, "__liftCometState")
      try {
        setAttribute.invoke(cont, "__liftCometState", null)
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
    val cont = getContinuation.invoke(contSupport, servletReq)

    val expired = isExpired.invoke(cont).asInstanceOf[Boolean]
    val resumed = isResumed.invoke(cont).asInstanceOf[Boolean]

    if (expired)
      RetryState.TIMED_OUT
    else if (resumed)
      RetryState.RESUMED
    else {
      setTimeout.invoke(cont, new java.lang.Long(timeout))
      suspendMeth.invoke(cont)
      RetryState.SUSPENDED
    }
    
  }

  def resume(what: (Req, LiftResponse)): Boolean = {
    val cont = getContinuation.invoke(contSupport, servletReq)
    try {
      setAttribute.invoke(cont, "__liftCometState", what)
      resumeMeth.invoke(cont)
      true
    } catch {
      case e: Exception => setAttribute.invoke(cont, "__liftCometState", null)
                false
    }
  }
}
