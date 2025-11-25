/*
 * Copyright 2008-2011 WorldWide Conferencing, LLC
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
package mocks

import scala.collection.mutable.HashMap
import scala.jdk.CollectionConverters._
import java.io.PrintWriter
import java.io.StringReader
import java.io.BufferedReader
import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream
import java.io.InputStream
import java.io.StringBufferInputStream
import java.io.File
import java.util.Collection
import java.util.Arrays
import java.util.Date
import java.util.Locale
import java.util.Vector
import jakarta.servlet._
import jakarta.servlet.http._

/**
 * A Mock HttpServletResponse. Take a peek at it's writer or
 * outputStream to see what lift has written in response to your request
 *
 * @param writer a PrintWriter that the response will be written with
 * @param outputStream an OutputStream that the response will be written to.
 *
 * @author Steve Jenson (stevej@pobox.com)
 */
class MockHttpServletResponse(var writer: PrintWriter, var outputStream: ServletOutputStream)
  extends HttpServletResponse {
  protected var statusCode : Int = 200
  protected var statusString : String = "OK"
  protected var contentType = "text/html"
  protected var contentLength: Int = 0
  protected var headers: Map[String, List[String]] = Map()
  protected var cookies: List[Cookie] = Nil
  protected var locale: Locale = Locale.getDefault
  protected var bufferSize: Int = 0
  protected var charEncoding = "ISO-8859-1" // yes, that's HTTP's default

  def setStatus(i: Int, s: String): Unit = {
    statusCode = i
    statusString = s
  }

  def setStatus(i: Int): Unit = {
    statusCode = i
  }
  def getStatus = statusCode

  def addIntHeader(s: String, i: Int): Unit = {
    addHeader(s, i.toString)
  }
  def setIntHeader(s: String, i: Int): Unit = {
    setHeader(s, i.toString)
  }
  def addHeader(s1: String, s2: String): Unit = {
    headers += (s1 -> (headers.getOrElse(s1, Nil) ::: List(s2)))
  }
  def setHeader(s1: String, s2: String): Unit = {
    headers += (s1 -> List(s2))
  }

  def getHeader(name: String): String = {
    headers.get(name).flatMap(_.headOption).getOrElse("")
  }
  def getHeaders(name: String): Collection[String] = {
    headers.get(name).getOrElse(Nil).asJava
  }
  def getHeaderNames(): Collection[String] = {
    headers.keySet.toSeq.asJava
  }

  def addDateHeader(s: String, l: Long): Unit = {
    addHeader(s, (new Date(l)).toString)
  }
  def setDateHeader(s: String, l: Long): Unit = {
    setHeader(s, (new Date(l)).toString)
  }

  override def sendRedirect(uri: String): Unit = {
    // Send back a 301 to the URL mentioned
    statusCode = 301
    addHeader("Location", uri)
  }

  def sendError(code: Int): Unit = {
    statusCode = code
  }

  def sendError(code: Int, s: String): Unit = {
    sendError(code)
    statusString = s
  }

  def encodeRedirectURL(url: String): String = encodeRedirectUrl(url)
  def encodeRedirectUrl(url: String): String = {
    // do something fancy encoding on uri, return that.
    url
  }
  def encodeURL(url: String): String = encodeUrl(url)
  def encodeUrl(url: String): String = {
    // use the same encoder as encodeRedirectUrl
    url
  }
  def containsHeader(header: String): Boolean = {
    headers.contains(header)
  }
  def addCookie(cookie: Cookie) = {
    cookies =  cookie :: cookies
  }
  def getLocale: Locale = locale
  def setLocale(l: Locale) = locale = l
  def reset: Unit = {
    // well, reset all the state to it's original values. yikes. later.
  }
  def isCommitted = false
  def resetBuffer: Unit = {
    // reset the buffer.
  }
  def flushBuffer: Unit = {
    // flush the buffer
  }
  def getBufferSize(): Int = bufferSize
  def setBufferSize(i: Int): Unit = bufferSize = i
  def setContentType(t: String): Unit = contentType = t
  def setContentLength(l: Int): Unit = contentLength = l
  def setCharacterEncoding(e: String): Unit = charEncoding = e
  def getWriter(): PrintWriter = writer
  def getOutputStream(): ServletOutputStream = outputStream
  def getContentType(): String = contentType
  def getCharacterEncoding(): String = charEncoding
  def setContentLengthLong(l: Long): Unit = contentType = l.toString

  override def sendRedirect(location: String, sc: Int, clearBuffer: Boolean): Unit = ???
}
