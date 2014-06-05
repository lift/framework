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

import java.io.{InputStream}
import java.util.{Locale}
import javax.servlet.http.{HttpServletRequest}
import org.apache.commons.fileupload.servlet._
import org.apache.commons.fileupload.ProgressListener
import net.liftweb.common._
import net.liftweb.util._
import Helpers._

class HTTPRequestServlet(val req: HttpServletRequest, val provider: HTTPProvider) extends HTTPRequest {
  private lazy val ctx = {
    new HTTPServletContext(req.getSession.getServletContext)
  }

  lazy val cookies: List[HTTPCookie] = {
    req.getSession(false) // do this to make sure we capture the JSESSIONID cookie
    (Box !! req.getCookies).map(_.toList.map(c => HTTPCookie(c.getName,
      Box !! (c.getValue),
      Box !! (c.getDomain),
      Box !! (c.getPath),
      Box !! (c.getMaxAge),
      Box !! (c.getVersion),
      Box !! (c.getSecure)))) openOr Nil
  }

  lazy val authType: Box[String] = Box !! req.getAuthType

  def headers(name: String): List[String] =
    for {
      h <- (Box !! req.getHeaders(name)).asA[java.util.Enumeration[String]].toList
      li <- enumToList[String](h) if null != li
    } yield li

  lazy val headers: List[HTTPParam] =
    for {
      hne <- (Box !! req.getHeaderNames).asA[java.util.Enumeration[String]].toList
      n <- enumToList[String](hne) if null != n
      hl <- Full(headers(n)) if !hl.isEmpty
    } yield HTTPParam(n, hl)

  def contextPath: String = req.getContextPath

  def context: HTTPContext = ctx

  def contentType = Box !! req.getContentType

  // don't cache... allow multiple sessions for the request
  // necessary for session destruction on login
  def session = new HTTPServletSession(req.getSession)

  def uri = req.getRequestURI

  def url = req.getRequestURL.toString

  lazy val queryString: Box[String] = Box !! req.getQueryString

  def param(name: String): List[String] = req.getParameterValues(name) match {case null => Nil case x => x.toList}

  lazy val params: List[HTTPParam] = enumToList[String](req.getParameterNames.asInstanceOf[java.util.Enumeration[String]]).
          map(n => HTTPParam(n, param(n)))

  lazy val paramNames: List[String] = params map (_.name)

  def remoteAddress: String = req.getRemoteAddr

  /**
   * The User-Agent of the request
   */
  lazy val userAgent: Box[String] =  headers find (_.name equalsIgnoreCase "user-agent") flatMap (_.values.headOption)

  def remotePort: Int = req.getRemotePort

  def remoteHost: String = req.getRemoteHost

  def serverName = req.getServerName

  def scheme: String = req.getScheme

  def serverPort = req.getServerPort

  def method: String = req.getMethod

  def locale: Box[Locale] = Box !! req.getLocale

  def inputStream: InputStream = req.getInputStream

  def multipartContent_? = ServletFileUpload.isMultipartContent(req)

  /**
   * Destroy the underlying servlet session
   */
  def destroyServletSession() {
    for{
      httpSession <- Box !! req.getSession(false)
    } yield httpSession.invalidate()
  }

  /**
   * @return the sessionID (if there is one) for this request.  This will *NOT* create
   * a new session if one does not already exist
   */
  def sessionId: Box[String] =
    for{
      httpSession <- Box !! req.getSession(false)
      id <- Box !! httpSession.getId
    } yield id

  def extractFiles: List[ParamHolder] = (new Iterator[ParamHolder] {
    val mimeUpload = (new ServletFileUpload)
    mimeUpload.setProgressListener(new ProgressListener {
      lazy val progList: (Long, Long, Int) => Unit = S.session.flatMap(_.progressListener) openOr LiftRules.progressListener

      def update(a: Long, b: Long, c: Int) {progList(a, b, c)}
    })

    mimeUpload.setSizeMax(LiftRules.maxMimeSize)
    mimeUpload.setFileSizeMax(LiftRules.maxMimeFileSize)
    val what = mimeUpload.getItemIterator(req)

    def hasNext = what.hasNext

    import scala.collection.JavaConversions._

    def next = what.next match {
      case f if (f.isFormField) => NormalParamHolder(f.getFieldName, new String(readWholeStream(f.openStream), "UTF-8"))
      case f => {
        val headers = f.getHeaders()
        val names: List[String] = if (headers eq null) Nil else headers.getHeaderNames().asInstanceOf[java.util.Iterator[String]].toList
        val map: Map[String, List[String]] = Map(names.map(n => n -> headers.getHeaders(n).asInstanceOf[java.util.Iterator[String]].toList) :_*)
        LiftRules.withMimeHeaders(map) {
          LiftRules.handleMimeFile(f.getFieldName, f.getContentType, f.getName, f.openStream)
        }
      }
    }
  }).toList


  def setCharacterEncoding(encoding: String) = req.setCharacterEncoding(encoding)

  def snapshot: HTTPRequest  = new OfflineRequestSnapshot(this, provider)

  private lazy val asyncProvider: Box[ServletAsyncProvider] =
    LiftRules.theServletAsyncProvider.map(_(this))

  def resumeInfo : Option[(Req, LiftResponse)] = asyncProvider.flatMap(_.resumeInfo)

  
  def suspend(timeout: Long): RetryState.Value = asyncProvider.openOrThrowException("open_! is bad, but presumably, the suspendResume support was checked").suspend(timeout)

  def resume(what: (Req, LiftResponse)): Boolean = asyncProvider.openOrThrowException("open_! is bad, but presumably, the suspendResume support was checked").resume(what)

  lazy val suspendResumeSupport_? = {
    LiftRules.asyncProviderMeta.
    map(_.suspendResumeSupport_? && 
        (asyncProvider.map(_.suspendResumeSupport_?) openOr
         false)) openOr false
  }
}

private class OfflineRequestSnapshot(req: HTTPRequest, val provider: HTTPProvider) extends HTTPRequest {

  private val _cookies = List(req.cookies :_*)

  private val _headers = List(req.headers :_*)

  private val _params = List(req.params :_*)


  def cookies: List[HTTPCookie] = _cookies

  val authType: Box[String] = req.authType

  def headers(name: String): List[String] = _headers.filter(_.name == name).map(_.name)

  def headers: List[HTTPParam] = _headers

  val contextPath: String = req.contextPath

  val context: HTTPContext = req.context

  val contentType: Box[String] = req.contentType

  val uri: String = req.uri

  val url: String = req.url

  val queryString: Box[String] = req.queryString

  def param(name: String): List[String] = _params.filter(_.name == name).map(_.name)

  def params: List[HTTPParam] = _params

  def paramNames: List[String] = _params.map(_.name)

  /**
   * Destroy the underlying servlet session... null for offline requests
   */
  def destroyServletSession() {
    // do nothing here
  }

  val session: HTTPSession = req.session

  val sessionId: Box[String] = req.sessionId

  val remoteAddress: String = req.remoteAddress

  val remotePort: Int = req.remotePort

  val remoteHost: String = req.remoteHost

  val serverName: String = req.serverName

  val scheme: String = req.scheme

  lazy val serverPort: Int = req.serverPort match {
    case 80 => headers("X-SSL").flatMap(Helpers.asBoolean _).filter(a => a).map(a => 443).headOption getOrElse 80
    case x => x
  }

  val method: String = req.method

  val resumeInfo : Option[(Req, LiftResponse)] = req.resumeInfo

  def suspend(timeout: Long): RetryState.Value =
    throw new UnsupportedOperationException("Cannot suspend a snapshot")

  def resume(what: (Req, LiftResponse)): Boolean =
    throw new UnsupportedOperationException("Cannot resume a snapshot")

  def suspendResumeSupport_? = false

  def inputStream: InputStream =
    throw new UnsupportedOperationException("InputStream is not available")

  val multipartContent_? : Boolean = req.multipartContent_?

  def extractFiles: List[ParamHolder] =
    throw new UnsupportedOperationException("It is unsafe to extract files")

  val locale: Box[Locale] = req.locale

  def setCharacterEncoding(encoding: String) =
    throw new UnsupportedOperationException("It is unsafe to set the character encoding ")

  def snapshot = this

  /**
   * The User-Agent of the request
   */
  lazy val userAgent: Box[String] =  headers find (_.name equalsIgnoreCase "user-agent") flatMap (_.values.headOption)

}
