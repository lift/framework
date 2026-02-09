/*
 * Copyright 2007-2015 Lift Committers and Contributors
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

import java.util.concurrent.ConcurrentHashMap

import common._
import actor._
import util._
  import Helpers._
import provider._
import scala.jdk.CollectionConverters._

private[http] case class AddSession(session: LiftSession)

private[http] case class RemoveSession(sessionId: String)

case class SessionWatcherInfo(sessions: Map[String, SessionInfo])

/**
 * Information about sessions
 */
case class SessionInfo(session: LiftSession, userAgent: Box[String], ipAddress: Box[String], requestCnt: Int, lastAccess: Long)

/**
 * Manages LiftSessions because the servlet container is less than optimal at
 * timing sessions out.
 */
object SessionMaster extends LiftActor with Loggable {
  private val nsessions: ConcurrentHashMap[String, SessionInfo] = new ConcurrentHashMap()
  private val killedSessions: ConcurrentHashMap[String, Long] = new ConcurrentHashMap()

  private object CheckAndPurge

  /**
   * If you have a rule other than <pre>Box !! req.request.remoteAddress</pre>
   * for calculating the remote address, change this function
   */
  @volatile var getIpFromReq: Req => Box[String] = req => Box !! req.request.remoteAddress

  /**
   * A list of functions that are run every 10 seconds.  The first param is
   * map containing the session ID and the sessions.  The second param is a function
   * to call to destroy the session.
   */
  @volatile var sessionCheckFuncs: List[(Map[String, SessionInfo], SessionInfo => Unit) => Unit] =
    ((ses: Map[String, SessionInfo], destroyer: SessionInfo => Unit) => {
      val now = millis

      for ((id, info@SessionInfo(session, _, _, _, _)) <- ses.iterator) {
        if (now - session.lastServiceTime > session.inactivityLength || session.markedForTermination) {
          logger.info(" Session " + id + " expired")
          destroyer(info)
        } else {
          session.doCometActorCleanup()
          session.cleanupUnseenFuncs()
        }
      }
    }) :: Nil

  def getSession(req: Req, otherId: Box[String]): Box[LiftSession] = {
    val dead = otherId.map(killedSessions.containsKey(_)) openOr false

    if (dead) Failure("dead session", Empty, Empty) else {
    val ret = this.synchronized {
      otherId.flatMap(a => Box !! nsessions.get(a)) match {
        case Full(session) => lockAndBump(Full(session))
        // for stateless requests, vend a stateless session if none is found
        case _ if req.stateless_? =>
          lockAndBump {
            req.sessionId.flatMap(a => Box !! nsessions.get(a))
          } or Full(LiftRules.statelessSession.vend.apply(req))
        case _ => getSession(req.request, otherId)
      }
    }

    ret
    }
  }


  /**
   * End comet long polling for all sessions. This allows a clean reload of Nginx
   * because Nginx children stick around for long polling.
   */
  def breakOutAllComet(): Unit = {
    import scala.jdk.CollectionConverters._

    val ses = lockRead(nsessions)
    ses.asScala.valuesIterator.foreach {
      _.session.breakOutComet()
    }
  }

  def getSession(id: String, otherId: Box[String]): Box[LiftSession] = lockAndBump {
    val dead = killedSessions.containsKey(id) || (otherId.map(killedSessions.containsKey(_)) openOr false)

    if (dead) (Failure("Dead session", Empty, Empty)) else {
    otherId.flatMap(a => Box !! nsessions.get(a)) or (Box !! nsessions.get(id))
    }
  }

  /**
   * Put an Actor in this list and the Actor will receive a message
   * every 10 seconds with the current list of sessions:
   * SessionWatcherInfo
   */
  @volatile var sessionWatchers: List[LiftActor] = Nil

  /**
   * Returns a LiftSession or Empty if not found
   */
  def getSession(httpSession: => HTTPSession, otherId: Box[String]): Box[LiftSession] =
    lockAndBump {
      otherId.flatMap(a => Box !! nsessions.get(a)) or (Box !! nsessions.get(httpSession.sessionId))
    }

  /**
   * Returns a LiftSession or Empty if not found
   */
  def getSession(req: HTTPRequest, otherId: Box[String]): Box[LiftSession] =
    lockAndBump {
      otherId.flatMap(a => Box !! nsessions.get(a)) or req.sessionId.flatMap(id => Box !! nsessions.get(id))
    }

  /**
   * Increments the count and last access time for the session
   */
  private def lockAndBump(f: => Box[SessionInfo]): Box[LiftSession] = this.synchronized {
    f.map {
      s =>
        nsessions.put(s.session.underlyingId, SessionInfo(s.session, s.userAgent, s.ipAddress, s.requestCnt + 1, millis))

        s.session
    }
  }

  private def lockRead[T](f: => T): T = this.synchronized {
    f
  }

  private def lockWrite[T](f: => T): T = this.synchronized {
    f
  }

  /**
   * Adds a new session to SessionMaster
   */
  def addSession(liftSession: LiftSession, req: Req,
                 userAgent: Box[String], ipAddress: Box[String]): Unit = {
    lockAndBump {
      Full(SessionInfo(liftSession, userAgent, ipAddress, -1, 0L)) // bumped twice during session creation.  Ticket #529 DPP
    }
    S.init(Box !! req, liftSession) {
      liftSession.startSession()
      LiftSession.afterSessionCreate.foreach(_(liftSession, req))
    }

    liftSession.httpSession.foreach(_.link(liftSession))
  }

  protected def messageHandler = reaction

  /**
   * Shut down all sessions
   */
  private[http] def shutDownAllSessions(): Unit = {
    import scala.jdk.CollectionConverters._

    val ses = lockRead(nsessions)
    ses.asScala.foreach {
      case (key, sess) =>
        if (!sess.session.markedForShutDown_?) {
          sess.session.markedForShutDown_? = true
          this ! RemoveSession(key)
        }
    }

    while (true) {
      val s2 = lockRead(nsessions)
      if (s2.size == 0) return
      Thread.sleep(50)
    }
  }

  def isDead(sessionId: String): Boolean = killedSessions.containsKey(sessionId)

  private val reaction: PartialFunction[Any, Unit] = {
    case RemoveSession(sessionId) if sessionId != null =>

      val ses = lockRead(nsessions)
      (Box !! ses.get(sessionId)).foreach {
        case SessionInfo(s, _, _, _, _) =>
          killedSessions.put(s.underlyingId, Helpers.millis)
          s.markedForShutDown_? = true
          Schedule.schedule(() => {
            try {
              s.doShutDown()
              try {
                s.httpSession.foreach(_.unlink(s))
              } catch {
                case e: Exception => // ignore... sometimes you can't do this and it's okay
              }
            } catch {
              case e: Exception => logger.warn("Failure in remove session", e)

            }
          }, 0.seconds)
          lockWrite {
            nsessions.remove(sessionId)
          }
      }

    case CheckAndPurge =>
      import scala.jdk.CollectionConverters._

    /* remove dead sessions that are more than 45 minutes old */
    val now = Helpers.millis - 45.minutes

    val removeKeys: Iterable[String] = killedSessions.asScala.filter(_._2 < now).map(_._1)
    removeKeys.foreach(s => killedSessions.remove(s))

      val ses = Map(lockRead {
        nsessions
      }.asScala.toList :_*)

      for {
        f <- sessionCheckFuncs
      } {
        if (Props.inGAE) {
          f(ses, shutDown => {
            if (!shutDown.session.markedForShutDown_?) {
              shutDown.session.markedForShutDown_? = true
              this.sendMsg(RemoveSession(shutDown.session.underlyingId))
            }
          })
        } else {
          Schedule.schedule(() => f(ses,
            shutDown => {
              if (!shutDown.session.markedForShutDown_?) {
                shutDown.session.
                  markedForShutDown_? = true

                this ! RemoveSession(shutDown.
                  session.
                  underlyingId)
              }
            }
          ), 0.seconds)
        }
      }

      if (!Props.inGAE) {
        sessionWatchers.foreach(_ ! SessionWatcherInfo(ses))
        doPing()
      }
  }


  private[http] def sendMsg(in: Any): Unit =
    if (!Props.inGAE) this ! in
    else {
      lockWrite {
        tryo {
          if (reaction.isDefinedAt(in)) reaction.apply(in)
        }
      }
    }

  private def doPing(): Unit = {
    if (!Props.inGAE) {
      try {
        Schedule.schedule(this, CheckAndPurge, 10.seconds)
      } catch {
        case e: Exception => logger.error("Couldn't start SessionMaster ping", e)
      }
    }
  }

  doPing()
}
