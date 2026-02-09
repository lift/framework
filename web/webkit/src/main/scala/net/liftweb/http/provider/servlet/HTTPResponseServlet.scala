/*
 * Copyright 2009-2011 Lift Committers and Contributors
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
import jakarta.servlet.http.{HttpServletResponse, Cookie}
import net.liftweb.http.provider.encoder.CookieEncoder
import net.liftweb.common._
import net.liftweb.util._
import Helpers._

class HTTPResponseServlet(resp: HttpServletResponse) extends HTTPResponse {
  private var _status = 0;

  private val SET_COOKIE_HEADER = "Set-Cookie"
 
  def addCookies(cookies: List[HTTPCookie]) = cookies.foreach {
    case cookie =>
      resp.addHeader(SET_COOKIE_HEADER, CookieEncoder.encode(cookie))
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

  def addHeaders(headers: List[HTTPParam]): Unit = {
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

