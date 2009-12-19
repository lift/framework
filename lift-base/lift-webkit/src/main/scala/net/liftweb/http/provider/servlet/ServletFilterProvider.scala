package net.liftweb.http.provider.servlet

import _root_.javax.servlet._
import _root_.javax.servlet.http._

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import Helpers._

trait ServletFilterProvider extends Filter with HTTPProvider {
  var ctx: HTTPContext = _

  //We need to capture the ServletContext on init
  def init(config: FilterConfig) {
    ctx = new HTTPServletContext(config.getServletContext)

    LiftRules.setContext(ctx)

    bootLift(Box.legacyNullTest(config.getInitParameter("bootloader")))

  }

  //And throw it away on destruction
  def destroy {
    ctx = null
    terminate
  }

  def context: HTTPContext = ctx

  /**
   * Executes the Lift filter component.
   */
  def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain) = {
    TransientRequestVarHandler(Empty,
    RequestVarHandler(Empty,
      (req, res) match {
        case (httpReq: HttpServletRequest, httpRes: HttpServletResponse) =>
          val httpRequest = new HTTPRequestServlet(httpReq)
          val httpResponse = new HTTPResponseServlet(httpRes)

          service(httpRequest, httpResponse) {
            chain.doFilter(req, res)
          }
        case _ => chain.doFilter(req, res)
      }))
  }


}
