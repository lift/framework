/*
 * Copyright 2008-2010 WorldWide Conferencing, LLC
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
package mocks {

import _root_.scala.collection.mutable.HashMap
import _root_.java.io.PrintWriter
import _root_.java.io.StringReader
import _root_.java.io.BufferedReader
import _root_.java.io.ByteArrayOutputStream
import _root_.java.io.ByteArrayInputStream
import _root_.java.io.FileInputStream
import _root_.java.io.InputStream
import _root_.java.io.StringBufferInputStream
import _root_.java.io.File
import _root_.java.util.Arrays
import _root_.java.util.Date
import _root_.java.util.Locale
import _root_.java.util.Vector
import _root_.javax.servlet._
import _root_.javax.servlet.http._
import _root_.net.liftweb.util.Helpers
import scala.collection.JavaConversions._

/**
 * A Mock ServletRequest. Change it's state to to create the request you are
 * interested in. At the very least, you will need to change method and path.
 *
 * @author Steve Jenson (stevej@pobox.com)
 */
class MockHttpServletRequest extends HttpServletRequest {
  var session : HttpSession = new MockHttpSession
  var queryString: String = ""
  var contextPath = ""
  var path = ""
  var method = "GET"
  private val headers: HashMap[String, String] = new HashMap()
  private val attr: HashMap[String, Object] = new HashMap()
  private var cookies: List[Cookie] = Nil
  var authType = null
  var localPort = 0
  var localAddr = null
  var localName = null
  var remotePort = 0
  var remoteHost = null
  var remoteAddr = null
  var locale = Locale.getDefault
  var reader: BufferedReader = new BufferedReader(new StringReader(method + " " + path +  "/\r\n\r\n"))
  var serverPort = 0
  var serverName = null
  var scheme = "http"
  var protocol = "http 1.0"
  var parameterMap: HashMap[String, Array[String]] = new HashMap()
  val sbis = new StringBufferInputStream("")
  var inputStream: ServletInputStream = new MockServletInputStream(sbis)
  var contentType = null
  var contentLength = 0
  var charEncoding = "ISO-8859-1" // HTTP's default encoding

  def isRequestedSessionIdFromURL = false
  def isRequestedSessionIdFromUrl = false
  def isRequestedSessionIdFromCookie = false
  def isRequestedSessionIdValid = false
  def getSession(p: Boolean) = {
    session
  }
  def getSession = getSession(false)
  def getServletPath = ""
  def getRequestURL = new StringBuffer(path)
  def getRequestURI = path
  def getRequestedSessionId = null
  def getUserPrincipal = null
  def isUserInRole(user: String): Boolean = false
  def getRemoteUser = ""
  def getQueryString = queryString
  def getContextPath = contextPath
  def getPathTranslated = path
  def getPathInfo = path
  def getMethod = method
  def getIntHeader(h: String): Int = {
    Helpers.toInt(headers(h))
  }
  def getHeaderNames: java.util.Enumeration[String] = {
    new Vector[String](asCollection(headers.keySet)).elements
  }
  def getHeaders = headers
  def getHeaders(s: String): java.util.Enumeration[Object] = {
    val v = new Vector[AnyRef]()
    v.add(headers(s))
    v.elements
  }
  def getHeader(h: String) = headers.get(h) match {
    case Some(v) => v
    case None => null
  }
  def getDateHeader(h: String): Long = {
    Helpers.toLong(headers(h))
  }
  def setDateHeader(s: String, l: Long) {
    headers(s) = l.toString
  }
  def getCookies = cookies.toArray
  def getAuthType = authType
  def getLocalPort = localPort
  def getLocalAddr = localAddr
  def getLocalName = localName
  def getRemotePort = remotePort
  def getRealPath(s: String) = s
  def getRequestDispatcher(s: String): RequestDispatcher = null
  def isSecure = false
  type ZZ = Q forSome {type Q}
  def getLocales: java.util.Enumeration[Object] = new Vector[Object](Arrays.asList(Locale.getAvailableLocales : _*)).elements
  def getLocale = locale
  def removeAttribute(key: String) = attr -= key
  def setAttribute(key: String, value: Object) = attr(key) = value
  def getRemoteHost = remoteHost
  def getRemoteAddr = remoteAddr
  def getReader = reader
  def getServerPort = serverPort
  def getServerName = serverName
  def getScheme = scheme
  def getProtocol = protocol
  def getParameterMap: java.util.Map[String, Array[String]] = parameterMap
  def getParameterValues(key: String) =
    parameterMap.get(key) match {
    case Some(v) => v
    case _ => null
    }   	 

  def getParameterNames: java.util.Enumeration[Object] = new Vector[Object](parameterMap.keySet).elements
  def getParameter(key: String) = parameterMap(key).apply(0)
  def getInputStream = inputStream
  def getContentType = contentType
  def getContentLength = contentLength
  def getCharacterEncoding = charEncoding
  def setCharacterEncoding(enc: String) = charEncoding = enc
  def getAttributeNames: java.util.Enumeration[Object] = new Vector[Object](attr.keySet).elements
  def getAttribute(key: String) = attr(key).asInstanceOf[Object]
}

}
}
