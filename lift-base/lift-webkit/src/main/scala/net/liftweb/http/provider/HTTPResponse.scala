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

import _root_.java.io.{OutputStream}

/**
 * Represents the HTTP response that will be send to the client
 */
trait HTTPResponse {

  /**
   * Add cookies to the response
   *
   * @param cookies - the list of response cookies
   */
  def addCookies(cookies: List[HTTPCookie])

  /**
   * Encodes the URL such that it adds the session ID if it is necessary to the URL.
   * It is implementation specific detail how/if to add the session informations. This will be used
   * for URL rewriting purposes.
   *
   * @param url - the URL that needs to be analysed
   */
  def encodeUrl(url: String): String

  /**
   * Add a list of header parameters to the response object
   *
   * @param headers - the list of headers
   */
  def addHeaders(headers: List[HTTPParam])

  /**
   * Sets the HTTP response status
   *
   * @param status - the HTTP status
   */
  def setStatus(status: Int)

  /**
   * Sets the HTTP response status
   *
   * @param status - the HTTP status
   * @param reason - the HTTP reason phrase
   */
  def setStatusWithReason(status: Int, reason: String)

  /**
   * @return - the OutputStream that can be used to send down o the client the response body.
   */
  def outputStream: OutputStream
}

}
}
}
