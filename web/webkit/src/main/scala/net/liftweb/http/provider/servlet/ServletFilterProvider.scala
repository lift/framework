/*
 * Copyright 2009-2011 WorldWide Conferencing, LLC
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

import javax.servlet._
import javax.servlet.http._

import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.http._
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
   * Wrap the loans around the incoming request
   */
  private def handleLoanWrappers[T](f: => T): T = {
    val wrappers = LiftRules.allAround.toList

    def handleLoan(lst: List[LoanWrapper]): T = lst match {
      case Nil => f
      case x :: xs => x(handleLoan(xs))
    }

    handleLoan(wrappers)
  }

  /**
   * Executes the Lift filter component.
   */
  def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain) = {
    if (LiftRules.ending) chain.doFilter(req, res)
    else {
      LiftRules.reqCnt.incrementAndGet()
      try {
        TransientRequestVarHandler(Empty,
                                   RequestVarHandler(Empty,

                                                     (req, res) match {
              case (httpReq: HttpServletRequest, httpRes: HttpServletResponse) =>
                val httpRequest = new HTTPRequestServlet(httpReq, this)
                val httpResponse = new HTTPResponseServlet(httpRes)

                handleLoanWrappers(service(httpRequest, httpResponse) {
                  chain.doFilter(req, res)
                })
              case _ => chain.doFilter(req, res)
            }))
      } finally {LiftRules.reqCnt.decrementAndGet()}
    }
  }
}
