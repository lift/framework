package net.liftweb.http.provider.servlet

import _root_.javax.servlet.http._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._

class HTTPServletSession(session: HttpSession) extends HTTPSession {
  private val LiftMagicID = "$lift_magic_session_thingy$"

  def sessionId: String = session getId

  def link(liftSession: LiftSession) = session.setAttribute(LiftMagicID, SessionToServletBridge(liftSession.uniqueId))

  def unlink(liftSession: LiftSession) = session.removeAttribute(LiftMagicID)

  def maxInactiveInterval: Long = session getMaxInactiveInterval

  def setMaxInactiveInterval(interval: Long) = session setMaxInactiveInterval (interval.toInt)

  def lastAccessedTime: Long = session getLastAccessedTime

  def setAttribute(name: String, value: Any) = session setAttribute (name, value)

  def attribute(name: String): Any = session getAttribute name

  def removeAttribute(name: String) = session removeAttribute name

  def terminate = session invalidate
}

/**
 * Represents the "bridge" between HttpSession and LiftSession
 */
@serializable
case class SessionToServletBridge(uniqueId: String) extends HttpSessionBindingListener with HttpSessionActivationListener {
  def sessionDidActivate(se: HttpSessionEvent) = {
    SessionMaster.getSession(uniqueId, Empty).foreach(ls =>
            LiftSession.onSessionActivate.foreach(_(ls)))
  }

  def sessionWillPassivate(se: HttpSessionEvent) = {
    SessionMaster.getSession(uniqueId, Empty).foreach(ls =>
            LiftSession.onSessionPassivate.foreach(_(ls)))
  }

  def valueBound(event: HttpSessionBindingEvent) {
  }

  /**
   * When the session is unbound the the HTTP session, stop us
   */
  def valueUnbound(event: HttpSessionBindingEvent) {
    SessionMaster.sendMsg(RemoveSession(uniqueId))
  }

}
