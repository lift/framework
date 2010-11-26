/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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
package http {
package provider {

import _root_.java.io.{InputStream}
import _root_.java.util.{Locale}
import _root_.net.liftweb.common.{Box}


object RetryState extends Enumeration {
  val SUSPENDED, TIMED_OUT, RESUMED = Value
}

/**
 * The representation of a HTTP request state
 */
trait HTTPRequest {

  /**
   * @return - cookies from this request. Nil if there are no cookies.
   */
  def cookies: List[HTTPCookie]

  /**
   * @return - HTTP authentication scheme: BASIC, DIGEST etc.
   *           Empty if there is auth header.
   */
  def authType: Box[String]

  /**
   * Return the header value by the given name
   *
   * @param name - the header name
   * @return - the header value. Empty if there is no such header
   */
  def header(name: String): Box[String] =
    headers.filter(_.name.equalsIgnoreCase(name)).headOption.flatMap(_.values.headOption)

  /**
   * Return the header values by the given name.
   *
   * @param name - the header name
   * @return - List[String] or Nil if there is no such header
   */
  def headers(name: String): List[String]

  /**
   * @return - all header parameters present in this request. Nil if no
   *           headers are used.
   */
  def headers: List[HTTPParam]

  /**
   * @return - the context path. Similar with HttpServletRequest.getContextPath.
   *           Return "" empty string if your implementation does not support the contept of
   *           context path
   */
  def contextPath: String

  /**
   * @return - the HTTPContext of this service. It does not guarantee that it returns the same
   *           HTTPContext reference upon each invocation but it must guarantee that HTTPContext
   *           reference contains the same information.
   */
  def context: HTTPContext

  /**
   * @return - the MIME type of the body of the request. Empty if this is unknonwn.
   */
  def contentType: Box[String]

  /**
   * @return - the request URI
   */
  def uri: String

  /**
   * @return - the request URL
   */
  def url: String

  /**
   * @return - the entire query string. Empty if the requst contains no query string
   */
  def queryString: Box[String]

  /**
   * @param name - the parameter name
   * @return - the list of values associated with this name
   */
  def param(name: String): List[String]

  /**
   * @return - all request parameters
   */
  def params: List[HTTPParam]

  /**
   * @return - request parameter names
   */
  def paramNames: List[String]

  /**
   * @return - the HTTP session associated with this request
   */
  def session: HTTPSession

  /**
   * Destroy the underlying servlet session
   */
  def destroyServletSession(): Unit

  /**
   * @return the sessionID (if there is one) for this request.  This will *NOT* create
   * a new session if one does not already exist
   */
  def sessionId: Box[String]

  /**
   * @return - the remote address of the client or the last seen proxy.
   */
  def remoteAddress: String

  /**
   * @return - the source port of the client or last seen proxy.
   */
  def remotePort: Int

  /**
   * @return - the fully qualified name of the client host or last seen proxy
   */
  def remoteHost: String

  /**
   * @return - the host name of the server
   */
  def serverName: String

  /**
   * @return - the name of the scheme of this request: http, https etc.
   */
  def scheme: String

  /**
   * @return - the server port
   */
  def serverPort: Int

  /**
   * @return - the HTTP method: GET, POST etc.
   */
  def method: String

  /**
   * @return true if the underlying container supports suspend/resume idiom.
   */
  def suspendResumeSupport_? : Boolean

  /**
   * @return - Some[Any] if this is a resumed request, return the state
   *           associated with it.
   */
  def resumeInfo : Option[Any]

  /**
   * Suspend the curent request and resume it after a given timeout
   */
  def suspend(timeout: Long): RetryState.Value

  /**
   * Resume this request
   * @return false if this continuation cannot be resumed
   *         as it is not in pending state.
   */
  def resume(what: AnyRef): Boolean

  /**
   * @return - the input stream for the request body
   */
  def inputStream: InputStream

  /**
   * @return true - if the request content is multipart
   */
  def multipartContent_? : Boolean

  /**
   * @return - the files uploaded
   */
  def extractFiles: List[ParamHolder]

  /**
   * @return - the locale forthis request. Empty if there is not language information.
   */
  def locale: Box[Locale]

  /**
   * Sets the character encoding that will be used for request body read
   *
   * @param encoding - the encoding that will be used (e.g. UTF-8)
   */
  def setCharacterEncoding(encoding: String)

  /**
   * Creates a new HTTPRequest instance as a copy of this one. It is used when
   * snapshots of the current request context is created in order for this request object
   * to be used on different threads (such as asynchronous template fragments processing).
   * The new instance must not keep any reference to the container' instances.
   */
  def snapshot: HTTPRequest

  /**
  * The User-Agent of the request
  */
  def userAgent: Box[String]
}

}
}
}
