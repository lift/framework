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
import _root_.java.util.{Enumeration => JEnum}
import _root_.java.util.{HashMap => JHash}
import _root_.javax.servlet._
import _root_.javax.servlet.http._
import _root_.net.liftweb.util.Helpers

/**
 * A Mock ServletRequest. Change it's state to to create the request you are
 * interested in. At the very least, you will need to change method and path.
 *
 * @author Steve Jenson (stevej@pobox.com)
 */
class MockHttpServletRequest extends HttpServletRequest {
  private implicit def itToEnum[T <: Object](it: Iterator[T]): JEnum[Object] =
    new JEnum[Object] {
      def hasMoreElements() = it.hasNext
      def nextElement(): Object = it.next
    }

  private implicit def mapToMap[K <: Object, V <: Object](in: Seq[(K, V)]):
  java.util.Map[Object, Object] = {
    val ret = new JHash[Object, Object]

    in.foreach {
      case (k, v) => ret.put(k,v)
    }

    ret
  }


  var session : HttpSession = new MockHttpSession
  var queryString: String = ""
  var contextPath: String = ""
  var path: String = ""
  var method: String = "GET"
  var headers: Map[String, List[String]] = Map()
  var attr: Map[String, Object] = Map()
  var cookies: List[Cookie] = Nil
  var authType: String = null
  var localPort = 0
  var localAddr: String = null
  var localName: String = null
  var remotePort = 0
  var remoteHost: String = null
  var remoteAddr: String = null
  var locale = Locale.getDefault
  var reader: BufferedReader = new BufferedReader(new StringReader(method + " " + path +  "/\r\n\r\n"))
  var serverPort = 0
  var serverName: String = null
  var scheme = "http"
  var protocol = "HTTP 1.0"
  var parameterMap: Map[String, List[String]] = Map()
  var sbis: InputStream = new StringBufferInputStream("")
  var inputStream: ServletInputStream = new MockServletInputStream(sbis)
  var contentType: String = null
  var contentLength: Int = 0
  var charEncoding: String = "ISO-8859-1" // HTTP's default encoding

  def isRequestedSessionIdFromURL(): Boolean = false
  def isRequestedSessionIdFromUrl(): Boolean = false
  def isRequestedSessionIdFromCookie(): Boolean = false
  def isRequestedSessionIdValid(): Boolean = false
  def getSession(p: Boolean): HttpSession = {
    session
  }
  def getSession(): HttpSession = getSession(false)
  def getServletPath(): String = ""
  def getRequestURL(): StringBuffer = new StringBuffer(path)
  def getRequestURI(): String = path
  def getRequestedSessionId(): String = null
  def getUserPrincipal(): java.security.Principal = null
  def isUserInRole(user: String): Boolean = false
  def getRemoteUser(): String = ""
  def getQueryString(): String = queryString
  def getContextPath(): String = contextPath
  def getPathTranslated(): String = path
  def getPathInfo(): String = path
  def getMethod(): String = method
  def getIntHeader(h: String): Int = {
    headers.get(h).flatMap(_.headOption).map(_.toInt) getOrElse -1
  }
  def getHeaderNames(): JEnum[Object] = headers.keys.iterator
  def getHeaders(s: String): JEnum[Object] =
    headers.getOrElse(s, Nil).elements

  def getHeader(h: String): String = headers.get(h) match {
    case Some(v :: _) => v
    case _ => null
  }
  def getDateHeader(h: String): Long = {
    headers.get(h).flatMap(_.headOption.map(_.toLong)) getOrElse -1L
  }
  def setDateHeader(s: String, l: Long) {
    headers += (s -> List(new java.util.Date(l).toString))
  }
  def getCookies(): Array[Cookie] = cookies.toArray
  def getAuthType(): String = authType
  def getLocalPort(): Int = localPort
  def getLocalAddr(): String = localAddr
  def getLocalName(): String = localName
  def getRemotePort(): Int = remotePort
  def getRealPath(s: String): String = s
  def getRequestDispatcher(s: String): RequestDispatcher = null
  def isSecure = false
  type ZZ = Q forSome {type Q}
  def getLocales(): JEnum[Object] = Locale.getAvailableLocales.elements
  def getLocale(): Locale = locale
  def removeAttribute(key: String): Unit = attr -= key
  def setAttribute(key: String, value: Object): Unit = attr += (key -> value)
  def getRemoteHost(): String = remoteHost
  def getRemoteAddr(): String = remoteAddr
  def getReader(): java.io.BufferedReader = reader
  def getServerPort(): Int = serverPort
  def getServerName(): String = serverName
  def getScheme(): String = scheme
  def getProtocol(): String = protocol
  def getParameterMap(): java.util.Map[Object, Object] = {
    parameterMap.map{
      case (key, value) => key -> value.toArray
    }.toSeq
  }

  def getParameterValues(key: String): Array[String] =
    parameterMap.get(key).map(_.toArray) getOrElse null

  def getParameterNames(): JEnum[Object] = parameterMap.keys.iterator
  def getParameter(key: String): String = parameterMap.get(key) match {
    case Some(x :: _) => x
    case _ => null
  }

  def getInputStream(): ServletInputStream = inputStream
  def getContentType(): String = contentType
  def getContentLength(): Int = contentLength
  def getCharacterEncoding(): String = charEncoding
  def setCharacterEncoding(enc: String): Unit = charEncoding = enc
  def getAttributeNames(): JEnum[Object] = attr.keys.iterator
  def getAttribute(key: String): Object = attr.get(key).getOrElse(null)
}

}
}
