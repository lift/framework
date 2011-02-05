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

/**
 * The representation of a HTTP session
 */
trait HTTPSession {

  /**
   * @return - the HTTP session ID
   */
  def sessionId: String

  /**
   * Links a LiftSession with this HTTP session. Hence when the HTTP session
   * terminates or times out LiftSession will be destroyed as well.
   *
   * @param liftSession - the LiftSession
   */
  def link(liftSession: LiftSession)

  /**
   * The opposite of the <i>link</i>. Hence the LiftSession and the HTTP session no
   * longer needs to be related. It is called when LiftSession is explicitelly terminated.
   *
   * @param liftSession - the LiftSession
   */
  def unlink(liftSession: LiftSession)

  /**
   * @returns - the maximim interval in seconds between client request and the time when
   *            the session will be terminated
   *
   */
  def maxInactiveInterval: Long

  /**
   * Sets the maximim interval in seconds between client request and the time when
   * the session will be terminated
   *
   * @param interval - the value in seconds
   *
   */
  def setMaxInactiveInterval(interval: Long)

  /**
   * @return - the last time server receivsd a client request for this session
   */
  def lastAccessedTime: Long

  /**
   * Sets a value associated with a name for this session
   *
   * @param name - the attribute name
   * @param value - any value
   */
  def setAttribute(name: String, value: Any)

  /**
   * @param name - the attribute name
   * @return - the attribute value associated with this name
   */
  def attribute(name: String): Any

  /**
   * Removes the session attribute having this name
   *
   * @param name - the attribute name
   */
  def removeAttribute(name: String): Unit

  /**
   * Terminates this session
   */
  def terminate: Unit
}

}
}
}
