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

import jakarta.servlet.http._
import net.liftweb.common._
import net.liftweb.util._

class HTTPServletSession(session: HttpSession) extends HTTPSession {
  private[this] val servletSessionIdentifier = LiftRules.servletSessionIdentifier

  def sessionId: String = session.getId

  def link(liftSession: LiftSession) = session.setAttribute(servletSessionIdentifier, SessionToServletBridge(liftSession.underlyingId))

  def unlink(liftSession: LiftSession) = session.removeAttribute(servletSessionIdentifier)

  def maxInactiveInterval: Long = session.getMaxInactiveInterval

  def setMaxInactiveInterval(interval: Long) = session.setMaxInactiveInterval (interval.toInt)

  def lastAccessedTime: Long = session.getLastAccessedTime

  def setAttribute(name: String, value: Any) = session.setAttribute(name, value)

  def attribute(name: String): Any = session.getAttribute(name)

  def removeAttribute(name: String) = session.removeAttribute(name)

  def terminate = session.invalidate
}

/**
 * Represents the "bridge" between HttpSession and LiftSession
 */
case class SessionToServletBridge(uniqueId: String) extends HttpSessionBindingListener with HttpSessionActivationListener {
  override def sessionDidActivate(se: HttpSessionEvent) = {
    SessionMaster.getSession(uniqueId, Empty).foreach(ls =>
            LiftSession.onSessionActivate.foreach(_(ls)))
  }

  override def sessionWillPassivate(se: HttpSessionEvent) = {
    SessionMaster.getSession(uniqueId, Empty).foreach(ls =>
            LiftSession.onSessionPassivate.foreach(_(ls)))
  }

  override def valueBound(event: HttpSessionBindingEvent): Unit = {
  }

  /**
   * When the session is unbound the the HTTP session, stop us
   */
  override def valueUnbound(event: HttpSessionBindingEvent): Unit = {
    SessionMaster.sendMsg(RemoveSession(uniqueId))
  }

}
