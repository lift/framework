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

import java.io.InputStream
import java.util.Locale
import jakarta.servlet.http.HttpServletRequest
import org.apache.commons.fileupload2.jakarta.servlet6.JakartaServletFileUpload
import org.apache.commons.fileupload2.core.ProgressListener
import net.liftweb.common._
import net.liftweb.util._
import Helpers._

class HTTPRequestServlet(@transient val req: HttpServletRequest, @transient val provider: HTTPProvider) extends HTTPRequest {
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

  def multipartContent_? = JakartaServletFileUpload.isMultipartContent(req)

  /**
   * Destroy the underlying servlet session
   */
  def destroyServletSession(): Unit = {
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
    val mimeUpload = new JakartaServletFileUpload
    mimeUpload.setProgressListener(new ProgressListener {
      lazy val progList: (Long, Long, Int) => Unit = S.session.flatMap(_.progressListener) openOr LiftRules.progressListener

      def update(a: Long, b: Long, c: Int): Unit = {progList(a, b, c)}
    })

    mimeUpload.setSizeMax(LiftRules.maxMimeSize)
    mimeUpload.setFileSizeMax(LiftRules.maxMimeFileSize)
    val what = mimeUpload.getItemIterator(req)

    def hasNext = what.hasNext

    import scala.jdk.CollectionConverters._

    def next() = what.next match {
      case f if (f.isFormField) => NormalParamHolder(f.getFieldName, new String(readWholeStream(f.getInputStream), "UTF-8"))
      case f => {
        val headers = f.getHeaders()
        val names: List[String] = if (headers eq null) Nil else headers.getHeaderNames().asInstanceOf[java.util.Iterator[String]].asScala.toList
        val map: Map[String, List[String]] = Map(names.map(n => n -> headers.getHeaders(n).asInstanceOf[java.util.Iterator[String]].asScala.toList) :_*)
        LiftRules.withMimeHeaders(map) {
          LiftRules.handleMimeFile(f.getFieldName, f.getContentType, f.getName, f.getInputStream)
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
