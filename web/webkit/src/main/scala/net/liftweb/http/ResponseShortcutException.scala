/*
 * Copyright 2007-2011 WorldWide Conferencing, LLC
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

import net.liftweb.common._
import net.liftweb.util._

/**
 * This exception is used by LiftSession.destroySessionAndContinueInNewSession
 * to unwind the stack so that the session can be destroyed and a new
 * session can be created and have the balance of the continuation executed
 * in the context of the new session.
 */
class ContinueResponseException(val continue: () => Nothing) extends LiftFlowOfControlException("Continue in new session")

object ContinueResponseException {
  def unapply(in: Throwable): Option[ContinueResponseException] = in match {
    case null => None
    case cre: ContinueResponseException => Some(cre)
    case e: Exception => unapply(e.getCause)
    case _ => None
  }
    
}


final case class ResponseShortcutException(_response: () => LiftResponse, redirectTo: Box[String], doNotices: Boolean) extends LiftFlowOfControlException("Shortcut") {
  lazy val response = _response()

  def this(resp: => LiftResponse, doNot: Boolean) = this(() => resp, Empty, doNot)
  def this(resp: => LiftResponse) = this (() => resp, Empty, false)
}

object ResponseShortcutException {
  def shortcutResponse(responseIt: => LiftResponse) =
    new ResponseShortcutException(responseIt, true)

  def redirect(to: String): ResponseShortcutException =
    new ResponseShortcutException(() => RedirectResponse(to, S.responseCookies: _*), Full(to), true)

  def redirect(to: String, func: () => Unit): ResponseShortcutException =
    S.session match {
      case Full(liftSession) => redirect(liftSession.attachRedirectFunc(to, Full(func)))
      case _ => redirect(to)
    }

  def seeOther(to: String): ResponseShortcutException =
    new ResponseShortcutException(() => SeeOtherResponse(to, S.responseCookies: _*), Full(to), true)

  def seeOther(to: String, func: () => Unit): ResponseShortcutException =
    S.session match {
      case Full(liftSession) => seeOther(liftSession.attachRedirectFunc(to, Full(func)))
      case _ => seeOther(to)
    }
}

