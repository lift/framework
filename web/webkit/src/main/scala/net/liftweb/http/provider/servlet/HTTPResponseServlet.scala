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

import scala.collection.mutable.{ListBuffer}
import java.io.{OutputStream}
import javax.servlet.http.{HttpServletResponse, Cookie}
import net.liftweb.common._
import net.liftweb.util._
import Helpers._

class HTTPResponseServlet(resp: HttpServletResponse) extends HTTPResponse {
  private var _status = 0;
 
  def addCookies(cookies: List[HTTPCookie]) = cookies.foreach {
    case c =>
      val cookie = new javax.servlet.http.Cookie(c.name, c.value openOr null)
      c.domain map (cookie.setDomain(_))
      c.path map (cookie.setPath(_))
      c.maxAge map (cookie.setMaxAge(_))
      c.version map (cookie.setVersion(_))
      c.secure_? map (cookie.setSecure(_))
      c.httpOnly.foreach { bv =>
        import scala.language.reflectiveCalls

        try {
          val cook30 = cookie.asInstanceOf[{def setHttpOnly(b: Boolean): Unit}]
          cook30.setHttpOnly(bv)
        } catch {
          case e: Exception => // swallow.. the exception will be thrown for Servlet 2.5 containers but work for servlet
          // 3.0 containers
        }
      }
      resp.addCookie(cookie)
  }

  private val shouldEncodeUrl = LiftRules.encodeJSessionIdInUrl_?

  /**
   * Encode the JSESSIONID in the URL if specified by LiftRules
   */
  def encodeUrl(url: String): String = 
    if (shouldEncodeUrl) {
      resp encodeURL url
    } else {
      url
    }

  def addHeaders(headers: List[HTTPParam]) {
    val appearOnce = Set(LiftRules.overwrittenReponseHeaders.vend.map(_.toLowerCase): _*)
    for (h <- headers;
         value <- h.values) {
      if (appearOnce.contains(h.name.toLowerCase)) resp.setHeader(h.name, value)
      else
        resp.addHeader(h.name, value)
    }
  }

  def setStatus(status: Int) = {
    _status = status
    resp setStatus status
  }

  def getStatus = _status
 
  def setStatusWithReason(status: Int, reason: String) = {
    _status = status
    resp sendError  (status, reason)
  }

  def outputStream: OutputStream = resp.getOutputStream
}

