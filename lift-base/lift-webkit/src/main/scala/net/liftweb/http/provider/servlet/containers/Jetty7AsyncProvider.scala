package net.liftweb.http.provider.servlet.containers

import _root_.javax.servlet.http.HttpServletRequest

import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
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
               resume) = {
    try {
      val cc = Class.forName("org.eclipse.jetty.continuation.ContinuationSupport")
      val meth = cc.getMethod("getContinuation", classOf[javax.servlet.ServletRequest])
      val cci = Class.forName("org.eclipse.jetty.continuation.Continuation")
      val getAttribute = cci.getMethod("getAttribute", classOf[String])
      val setAttribute = cci.getMethod("setAttribute", classOf[String], classOf[AnyRef])
      val suspend = cci.getMethod("suspend")
      val setTimeout = cci.getMethod("setTimeout", _root_.java.lang.Long.TYPE)
      val resume = cci.getMethod("resume")
      (true, (cc), (meth), (getAttribute), (setAttribute), (suspend), setTimeout, resume)
    } catch {
      case e => 
        (false, null, null, null, null, null, null, null)
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

  def suspend(timeout: Long): Any = {
    val cont = getContinuation.invoke(contSupport, servletReq)
    setTimeout.invoke(cont, new _root_.java.lang.Long(timeout))
    suspend.invoke(cont)
  }

  def resume(what: AnyRef) {
    val cont = getContinuation.invoke(contSupport, servletReq)
    setAttribute.invoke(cont, "__liftCometState", what)
    resume.invoke(cont)
  }


}
