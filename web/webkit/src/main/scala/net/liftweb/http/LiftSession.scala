/*
 * Copyright 2007-2012 WorldWide Conferencing, LLC
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

import java.lang.reflect.{Method}

import collection.mutable.{HashMap, ListBuffer}
import xml._

import common._
import Box._
import actor._
import util._
import Helpers._
import http.js.{JsCmd, AjaxInfo}
import builtin.snippet._
import js._
import provider._


object LiftSession {

  /**
   * Returns a reference to a LiftSession dictated by LiftRules#sessionCreator function.
   */
  def apply(session: HTTPSession, contextPath: String) =
    LiftRules.sessionCreator(session, contextPath)

  def apply(request: Req): LiftSession =
    if (request.stateless_?) LiftRules.statelessSession.vend.apply(request)
    else this.apply(request.request.session, request.request.contextPath)

  /**
   * Holds user's functions that will be called when the session is activated
   */
  var onSessionActivate: List[LiftSession => Unit] = Nil

  /**
   * Holds user's functions that will be called when the session is passivated
   */
  var onSessionPassivate: List[LiftSession => Unit] = Nil

  /**
   * Holds user's functions that will be called when the session is setup
   */
  var onSetupSession: List[LiftSession => Unit] = Nil

  /**
   * Holds user's functions that will be called when the session is about to be terminated
   */
  var onAboutToShutdownSession: List[LiftSession => Unit] = Nil

  /**
   * Holds user's functions that will be called when the session is terminated
   */
  var onShutdownSession: List[LiftSession => Unit] = Nil

  /**
   * Holds user's functions that will be called when a stateful request is about to be processed
   */
  var onBeginServicing: List[(LiftSession, Req) => Unit] = Nil

  /**
   * After the session is created, if you need to do anything within
   * the context of the session (like set SessionVars, etc),
   * add the function to this list
   */
  var afterSessionCreate: List[(LiftSession, Req) => Unit] = Nil

  /**
   * Holds user's functions that will be called when a stateful request has been processed
   */
  var onEndServicing: List[(LiftSession, Req, Box[LiftResponse]) => Unit] = Nil



  @volatile private var constructorCache: Map[(Class[_], Box[Class[_]]), Box[ConstructorType]] = Map()

  private[http] def constructFrom[T](session: LiftSession, pp: Box[ParamPair], clz: Class[T]): Box[T] = {
    def calcConstructor(): Box[ConstructorType] = {
      val const = clz.getDeclaredConstructors()

      def nullConstructor(): Box[ConstructorType] =
        const.find(_.getParameterTypes.length == 0).map(const => UnitConstructor(const))

      pp match {
        case Full(ParamPair(value, clz)) =>
          const.find {
            cp => {
              cp.getParameterTypes.length == 2 &&
                cp.getParameterTypes().apply(0).isAssignableFrom(clz) &&
                cp.getParameterTypes().apply(1).isAssignableFrom(classOf[LiftSession])
            }
          }.
            map(const => PAndSessionConstructor(const)) orElse
            const.find {
              cp => {
                cp.getParameterTypes.length == 1 &&
                  cp.getParameterTypes().apply(0).isAssignableFrom(clz)
              }
            }.
              map(const => PConstructor(const)) orElse nullConstructor()

        case _ =>
          nullConstructor()
      }
    }

    (if (Props.devMode) {
      // no caching in dev mode
      calcConstructor()
    } else {
      val key = (clz -> pp.map(_.clz))
      constructorCache.get(key) match {
        case Some(v) => v
        case _ => {
          val nv = calcConstructor()
          constructorCache += (key -> nv)
          nv
        }
      }
    }).map {
      case uc: UnitConstructor => uc.makeOne
      case pc: PConstructor => pc.makeOne(pp.openOrThrowException("It's ok").v)
      case psc: PAndSessionConstructor => psc.makeOne(pp.openOrThrowException("It's ok").v, session)
    }
  }

  /**
   * Check to see if the template is marked designer friendly
   * and lop off the stuff before the first surround
   */
  @deprecated("Use Templates.checkForContentId", "2.3")
  def checkForContentId(in: NodeSeq): NodeSeq =
    Templates.checkForContentId(in)


}


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
  private var sessions: Map[String, SessionInfo] = Map.empty

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
    this.synchronized {
      otherId.flatMap(sessions.get) match {
        case Full(session) => lockAndBump(Full(session))
        // for stateless requests, vend a stateless session if none is found
        case _ if req.stateless_? =>
          lockAndBump {
            req.sessionId.flatMap(sessions.get)
          } or Full(LiftRules.statelessSession.vend.apply(req))
        case _ => getSession(req.request, otherId)
      }
    }
  }


  /**
   * End comet long polling for all sessions. This allows a clean reload of Nginx
   * because Nginx children stick around for long polling.
   */
  def breakOutAllComet() {
    val ses = lockRead(sessions)
    ses.valuesIterator.foreach {
      _.session.breakOutComet()
    }
  }

  def getSession(id: String, otherId: Box[String]): Box[LiftSession] = lockAndBump {
    otherId.flatMap(sessions.get) or Box(sessions.get(id))
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
      otherId.flatMap(sessions.get) or Box(sessions.get(httpSession.sessionId))
    }

  /**
   * Returns a LiftSession or Empty if not found
   */
  def getSession(req: HTTPRequest, otherId: Box[String]): Box[LiftSession] =
    lockAndBump {
      otherId.flatMap(sessions.get) or req.sessionId.flatMap(id => sessions.get(id))
    }

  /**
   * Increments the count and last access time for the session
   */
  private def lockAndBump(f: => Box[SessionInfo]): Box[LiftSession] = this.synchronized {
    f.map {
      s =>
        sessions += s.session.uniqueId -> SessionInfo(s.session, s.userAgent, s.ipAddress, s.requestCnt + 1, millis)

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
                 userAgent: Box[String], ipAddress: Box[String]) {
    lockAndBump {
      Full(SessionInfo(liftSession, userAgent, ipAddress, -1, 0L)) // bumped twice during session creation.  Ticket #529 DPP
    }
    S.init(req, liftSession) {
      liftSession.startSession()
      LiftSession.afterSessionCreate.foreach(_(liftSession, req))
    }

    liftSession.httpSession.foreach(_.link(liftSession))
  }

  protected def messageHandler = reaction

  /**
   * Shut down all sessions
   */
  private[http] def shutDownAllSessions() {
    val ses = lockRead(sessions)
    ses.foreach {
      case (key, sess) =>
        if (!sess.session.markedForShutDown_?) {
          sess.session.markedForShutDown_? = true
          this ! RemoveSession(key)
        }
    }

    while (true) {
      val s2 = lockRead(sessions)
      if (s2.size == 0) return
      Thread.sleep(50)
    }
  }

  private val reaction: PartialFunction[Any, Unit] = {
    case RemoveSession(sessionId) =>
      val ses = lockRead(sessions)
      ses.get(sessionId).foreach {
        case SessionInfo(s, _, _, _, _) =>
          s.markedForShutDown_? = true
          Schedule.schedule(() => {
            try {
              s.doShutDown
              try {
                s.httpSession.foreach(_.unlink(s))
              } catch {
                case e: Exception => // ignore... sometimes you can't do this and it's okay
              }
            } catch {
              case e: Exception => logger.warn("Failure in remove session", e)

            }
          }, 0 seconds)
          lockWrite {
            sessions = sessions - sessionId
          }
      }

    case CheckAndPurge =>
      val ses = lockRead {
        sessions
      }

      for {
        f <- sessionCheckFuncs
      } {
        if (Props.inGAE) {
          f(ses, shutDown => {
            if (!shutDown.session.markedForShutDown_?) {
              shutDown.session.markedForShutDown_? = true
              this.sendMsg(RemoveSession(shutDown.session.uniqueId))
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
                  uniqueId)
              }
            }
          ), 0 seconds)
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

  private def doPing() {
    if (!Props.inGAE) {
      try {
        Schedule.schedule(this, CheckAndPurge, 10 seconds)
      } catch {
        case e: Exception => logger.error("Couldn't start SessionMaster ping", e)
      }
    }
  }

  doPing()
}


object PageName extends RequestVar[String]("")

/**
 * Information about the page garbage collection
 */
private[http] object RenderVersion {

  private object ver extends RequestVar(Helpers.nextFuncName)

  def get: String = ver.is

  def doWith[T](v: String)(f: => T): T = {
    val ret: Box[T] =
      for {
        sess <- S.session
        func <- sess.findFunc(v).collect {
          case f: S.PageStateHolder => f
        }
      } yield {
        val tret = ver.doWith(v) {
          val ret = func.runInContext(f)


          if (S.functionMap.size > 0) {
            sess.updateFunctionMap(S.functionMap, this.get, millis)
            S.clearFunctionMap
          }
          ret
        }
        tret
      }

    ret openOr ver.doWith(v)(f)
  }
}

/**
 * A trait defining how stateful the session is
 */
trait HowStateful {
  private val howStateful = new ThreadGlobal[Boolean]

  /**
   * Test the statefulness of this session.
   */
  def stateful_? = howStateful.box openOr true

  /**
   * There may be cases when you are allowed container state (e.g.,
   * migratory session, but you're not allowed to write Lift
   * non-migratory state, return true here.
   */
  def allowContainerState_? = howStateful.box openOr true

  /**
   * Within the scope of the call, this session is forced into
   * statelessness.  This allows for certain URLs in on the site
   * to be stateless and not generate a session, but if a valid
   * session is presented, they have the scope of that session/User
   */
  def doAsStateless[A](f: => A): A =
    howStateful.doWith(false)(f)
}

/**
 * Sessions that include this trait will not be retained past the current
 * request and will give notifications of failure if stateful features
 * of Lift are accessed
 */
trait StatelessSession extends HowStateful {
  self: LiftSession =>

  override def stateful_? = false

  override def allowContainerState_? = false
}

/**
 * Sessions that include this trait will only have access to the container's
 * state via ContainerVars.  This mode is "migratory" so that a session
 * can migrate across app servers.  In this mode, functions that
 * access Lift state will give notifications of failure if stateful features
 * of Lift are accessed
 */
trait MigratorySession extends HowStateful {
  self: LiftSession =>

  override def stateful_? = false
}

/**
 * Keeps information around about what kinds of functions are run
 * at the end of page rendering.  The results of these functions will be
 * appended to the bottom of the page.
 *
 * @param renderVersion -- the page ID (aka the RenderVersion)
 * @param functionCount -- the number of functions in the collection
 * @param lastSeen -- page of the page-level GC
 * @param functions -- the list of functions to run
 */
private final case class PostPageFunctions(renderVersion: String,
                                           functionCount: Int,
                                           longLife: Boolean,
                                           lastSeen: Long,
                                           functions: List[() => JsCmd]) {
  /**
   * Create a new instance based on the last seen time
   */
  def updateLastSeen = new PostPageFunctions(renderVersion,
    functionCount,
    longLife,
    Helpers.millis,
    functions)


}

/**
 * The responseFuture will be satisfied by the original request handling
 * thread when the response has been calculated. Retries will wait for the
 * future to be satisfied in order to return the proper response.
 */
private[http] final case class AjaxRequestInfo(requestVersion: Long,
                                               responseFuture: LAFuture[Box[LiftResponse]],
                                               lastSeen: Long)

/**
 * The LiftSession class containg the session state information
 */
class LiftSession(private[http] val _contextPath: String, val uniqueId: String,
                  val httpSession: Box[HTTPSession]) extends LiftMerge with Loggable with HowStateful {
  def sessionHtmlProperties = LiftRules.htmlProperties.session.is.make openOr LiftRules.htmlProperties.default.is.vend

  val requestHtmlProperties: TransientRequestVar[HtmlProperties] =
    new TransientRequestVar[HtmlProperties](sessionHtmlProperties(S.request openOr Req.nil)) {}

  @volatile
  private[http] var markedForTermination = false

  @volatile
  private var _running_? = false

  /**
   * Was this session marked for shutdown... if so,
   * don't remark
   */
  @volatile private[http] var markedForShutDown_? = false

  private val fullPageLoad = new ThreadGlobal[Boolean] {
    def ? = this.box openOr false
  }

  /**
   *  ****IMPORTANT**** when you access messageCallback, it *MUST*
   * be in a block that's synchronized on the owner LiftSession
   */
  private var messageCallback: HashMap[String, S.AFuncHolder] = new HashMap

  private[http] var notices: Seq[(NoticeType.Value, NodeSeq, Box[String])] = Nil

  private var asyncComponents = new HashMap[(Box[String], Box[String]), LiftCometActor]()

  private var asyncById = new HashMap[String, LiftCometActor]()

  private var myVariables: Map[String, Any] = Map.empty

  private var onSessionEnd: List[LiftSession => Unit] = Nil

  private val sessionVarSync = new Object

  /**
  * Cache the value of allowing snippet attribute processing
  */
  private object allowAttributeProcessing extends TransientRequestVar(LiftRules.allowAttributeSnippets.vend())

  /**
   * A mapping between pages denoted by RenderVersion and
   * functions to execute at the end of the page rendering
   */
  private var postPageFunctions: Map[String, PostPageFunctions] = Map()

  /**
   * A list of AJAX requests that may or may not be pending for this
   * session. There is an entry for every AJAX request we don't *know*
   * has completed successfully or been discarded by the client.
   *
   * See LiftServlet.handleAjax for how we determine we no longer need
   * to hold a reference to an AJAX request.
   */
  private var ajaxRequests = scala.collection.mutable.Map[String,List[AjaxRequestInfo]]()

  private[http] def withAjaxRequests[T](fn: (scala.collection.mutable.Map[String, List[AjaxRequestInfo]]) => T) = {
    ajaxRequests.synchronized { fn(ajaxRequests) }
  }

  /**
   * The synchronization lock for the postPageFunctions
   */
  private val postPageLock = new Object

  @volatile
  private[http] var lastServiceTime = millis

  @volatile
  private[http] var inactivityLength: Long =
    LiftRules.sessionInactivityTimeout.vend openOr ((30 minutes): Long)

  private[http] var highLevelSessionDispatcher = new HashMap[String, LiftRules.DispatchPF]()
  private[http] var sessionRewriter = new HashMap[String, LiftRules.RewritePF]()


  private object snippetMap extends RequestVar[Map[String, AnyRef]](Map())

  private[http] object deferredSnippets extends RequestVar[HashMap[String, Box[NodeSeq]]](new HashMap)

  private object cometSetup extends SessionVar[List[((Box[String], Box[String]), Any)]](Nil)


  private[http] def startSession(): Unit = {
    _running_? = true
    for (sess <- httpSession) {
      // calculate the inactivity length.  If the length is
      // defined in LiftRules and it's less than the container's length
      // then use the Lift length.  Why not use it if the Lift length is
      // longer?  Well, the container's just going to time you out, so
      // why bother.
      inactivityLength =
        (sess.maxInactiveInterval * 1000L,
          LiftRules.sessionInactivityTimeout.vend) match {
          case (container, Full(lift)) if lift < container => lift
          case (container, _) => container
        }
    }

    lastServiceTime = millis
    LiftSession.onSetupSession.foreach(_(this))
    sessionHtmlProperties // cause the properties to be calculated
  }

  def running_? = _running_?

  private var cometList: List[(LiftActor, Req)] = Nil

  private[http] def breakOutComet(): Unit = {
    val cl = synchronized {
      cometList
    }
    cl.foreach(_._1 ! BreakOut())
  }

  // Returns a 2-tuple: _1 is a list of valid (LiftActor, Req) pairs for
  // this session that match the given hostAndPath, while _2 is a list
  // of invalid (LiftActor, Req) pairs.
  //
  // Invalid pairs are pairs where the hostAndPath lookup for the
  // associated Req fails by throwing an exception. Typically this
  // happens on overloaded containers that leave Reqs with underlying
  // HttpServletRequests that have expired; these will then throw
  // NullPointerExceptions when their server name or otherwise are
  // accessed.
  private[http] def cometForHost(hostAndPath: String): (List[(LiftActor, Req)], List[(LiftActor, Req)]) =
    synchronized {
      cometList
    }.foldLeft((List[(LiftActor, Req)](), List[(LiftActor, Req)]())) {
      (soFar, current) =>
        (soFar, current) match {
          case ((valid, invalid), pair @ (_, r)) =>
            try {
              if (r.hostAndPath == hostAndPath)
                (pair :: valid, invalid)
              else
                soFar
            } catch {
              case exception =>
                (valid, pair :: invalid)
            }
        }
    }

  private[http] def enterComet(what: (LiftActor, Req)): Unit = synchronized {
    cometList = what :: cometList
  }

  private[http] def exitComet(what: LiftActor): Unit = synchronized {
    cometList = cometList.filterNot(_._1 eq what)
  }

  private case class RunnerHolder(name: String, func: S.AFuncHolder, owner: Box[String])

  object ieMode extends SessionVar[Boolean](LiftRules.calcIEMode()) {
    override private[liftweb] def magicSessionVar_? = true
  }

  def terminateHint {
    if (_running_?) {
      markedForTermination = true;
    }
  }


  /**
   * Find a function in the function lookup table.  You probably never need to do this, but
   * well, you can look them up.
   */
  def findFunc(funcName: String): Option[S.AFuncHolder] =
  synchronized {
    messageCallback.get(funcName)
  }

  /**
   * Executes the user's functions based on the query parameters
   */
  def runParams(state: Req): List[Any] = {

    val toRun = {
      // get all the commands, sorted by owner,
      (state.uploadedFiles.map(_.name) ::: state.paramNames).distinct.
        flatMap {
        n => synchronized {
          messageCallback.get(n)
        }.map(mcb => RunnerHolder(n, mcb, mcb.owner))
      }.
        sortWith {
        case (RunnerHolder(_, _, Full(a)), RunnerHolder(_, _, Full(b))) if a < b => true
        case (RunnerHolder(_, _, Full(a)), RunnerHolder(_, _, Full(b))) if a > b => false
        case (RunnerHolder(an, _, Full(a)), RunnerHolder(bn, _, Full(b))) if a == b => an < bn
        case (RunnerHolder(_, _, Full(_)), _) => false
        case (_, RunnerHolder(_, _, Full(_))) => true
        case (RunnerHolder(a, _, _), RunnerHolder(b, _, _)) => a < b
        case _ => false
      }
    }

    def buildFunc(i: RunnerHolder): () => Any = i.func match {
      case bfh if bfh.supportsFileParams_? =>
        () => state.uploadedFiles.filter(_.name == i.name).map(v => bfh(v))
      case normal =>
        () => normal(state.params.getOrElse(i.name,
          state.uploadedFiles.filter(_.name == i.name).map(_.fileName)))
    }

    val ret = toRun.map(_.owner).distinct.flatMap {
      w =>
        val f = toRun.filter(_.owner == w)
        w match {
          // if it's going to a CometActor, batch up the commands
          case Full(id) if asyncById.contains(id) => asyncById.get(id).toList.flatMap(a =>
            a.!?(ActionMessageSet(f.map(i => buildFunc(i)), state)) match {
              case li: List[_] => li
              case other => Nil
            })
          case _ => f.map(i => buildFunc(i).apply())
        }
    }

    ret
  }

  /**
   * Updates the internal functions mapping
   */
  def updateFunctionMap(funcs: Map[String, S.AFuncHolder], uniqueId: String, when: Long): Unit = synchronized {
    funcs.foreach {
      case (name, func) =>
        messageCallback(name) =
          if (func.owner == Full(uniqueId)) func else func.duplicate(uniqueId)
    }
  }

  def removeFunction(name: String) = synchronized {
    messageCallback -= name
  }

  /**
   * Set your session-specific progress listener for mime uploads
   *     pBytesRead - The total number of bytes, which have been read so far.
   *    pContentLength - The total number of bytes, which are being read. May be -1, if this number is unknown.
   *    pItems - The number of the field, which is currently being read. (0 = no item so far, 1 = first item is being read, ...)
   */
  var progressListener: Box[(Long, Long, Int) => Unit] = Empty

  /**
   * Called just before the session exits.  If there's clean-up work, override this method
   */
  private[http] def cleanUpSession() {
    messageCallback = HashMap.empty
    notices = Nil
    asyncComponents.clear
    asyncById = HashMap.empty
    myVariables = Map.empty
    onSessionEnd = Nil
    postPageFunctions = Map()
    highLevelSessionDispatcher = HashMap.empty
    sessionRewriter = HashMap.empty
  }

  private[http] def fixSessionTime(): Unit = synchronized {
    for (httpSession <- this.httpSession) {
      lastServiceTime = millis // DO NOT REMOVE THIS LINE!!!!!
      val diff = lastServiceTime - httpSession.lastAccessedTime
      val maxInactive = httpSession.maxInactiveInterval.toInt
      val togo: Int = maxInactive - (diff / 1000L).toInt
      // if we're within 2 minutes of session timeout and
      // the Servlet session doesn't seem to have been updated,
      // extends the lifespan of the HttpSession
      if (diff > 1000L && togo < 120) {
        httpSession.setMaxInactiveInterval(maxInactive + 120)
      }
    }
  }

  private[http] def doCometActorCleanup(): Unit = {
    val acl = synchronized {
      this.asyncComponents.values.toList
    }
    acl.foreach(_ ! ShutdownIfPastLifespan)
  }

  /**
   * Adds a cleanup function that will be executed when session is terminated
   */
  def addSessionCleanup(f: LiftSession => Unit): Unit = synchronized {
    onSessionEnd = f :: onSessionEnd
  }

  /**
   * Destroy this session and the underlying container session.
   */
  def destroySession() {
    S.request.foreach(_.request.session.terminate)
    this.doShutDown()
  }

  private[http] def doShutDown() {
    if (running_?) {
      // only deal with comet on stateful sessions
      // stateless temporary sessions bar comet use
      if (stateful_?) {
        val cl = synchronized {
          cometList
        }
        if (cl.length > 0) {
          this.breakOutComet()
          Thread.sleep(100)
        }
      }
      this.shutDown()
    }
  }

  /**
   * Puts the correct thread locking around access to postPageFunctions
   */
  private def accessPostPageFuncs[T](f: => T): T = {
    postPageLock.synchronized {
      f
    }
  }

  private[http] def cleanupUnseenFuncs(): Unit = {
    if (LiftRules.enableLiftGC && stateful_?) {
      val now = millis

      accessPostPageFuncs {
        for {
          (key, pageInfo) <- postPageFunctions
        } if (!pageInfo.longLife &&
          (now - pageInfo.lastSeen) > LiftRules.unusedFunctionsLifeTime) {
          postPageFunctions -= key
        }
      }

      withAjaxRequests { currentAjaxRequests =>
        for {
          (version, requestInfos) <- currentAjaxRequests
        } {
          val remaining =
            requestInfos.filter { info =>
              (now - info.lastSeen) <= LiftRules.unusedFunctionsLifeTime
            }

          if (remaining.length > 0)
            currentAjaxRequests += (version -> remaining)
          else
            currentAjaxRequests -= version
        }
      }

      synchronized {
        messageCallback.foreach {
          case (k, f) =>
            if (!f.sessionLife &&
              f.owner.isDefined &&
              (now - f.lastSeen) > LiftRules.unusedFunctionsLifeTime) {
              messageCallback -= k
            }
        }
      }
    }
  }

  /**
   * Clear the PostPage JavaScript functions for the current page.
   * This is used by CometActor to remove the PostPage JavaScript
   * functions from the given component during redraw.
   */
  def clearPostPageJavaScriptForThisPage() {
    testStatefulFeature {
      accessPostPageFuncs {
        val rv: String = RenderVersion.get

        postPageFunctions -= rv
      }
    }
  }

  /**
   * Associate a function that renders JavaScript with the current page.
   * This function will be run and the resulting JavaScript will be appended
   * to any rendering associated with this page... the normal page render,
   * Ajax calls, and even Comet calls for this page.
   *
   * @param func -- the function that returns JavaScript to be appended to
   * responses associated with this page
   */
  def addPostPageJavaScript(func: () => JsCmd) {
    testStatefulFeature {
      accessPostPageFuncs {
        // The page or cometactor that the functions are associated with
        val rv: String = RenderVersion.get

        val old =
          postPageFunctions.getOrElse(rv,
            PostPageFunctions(rv,
              0,
              S.currentCometActor.
                isDefined,
              Helpers.millis,
              Nil))

        val updated = PostPageFunctions(old.renderVersion,
          old.functionCount + 1,
          old.longLife,
          Helpers.millis,
          func :: old.functions)

        postPageFunctions += (rv -> updated)
      }
    }
  }

  def postPageJavaScript(rv: String): List[JsCmd] = {
    def org = accessPostPageFuncs {
      val ret = postPageFunctions.get(rv)
      ret.foreach {
        r => postPageFunctions += (rv -> r.updateLastSeen)
      }
      ret
    }

    org match {
      case None => Nil
      case Some(ppf) => {
        val lb = new ListBuffer[JsCmd]

        def run(count: Int, funcs: List[() => JsCmd]) {
          funcs.reverse.foreach(f => lb += f())
          val next = org.get // safe to do get here because we know the
          // postPageFunc is defined

          val diff = next.functionCount - count

          // if the function table is updated, make sure to get
          // the additional functions
          if (diff == 0) {} else {
            run(next.functionCount, next.functions.take(diff))
          }
        }

        run(ppf.functionCount, ppf.functions)

        lb.toList

      }
    }
  }

  /**
   * Get the post-page JavaScript functions for a sequence of page IDs.
   * This is used by the CometActor to get the post-page JavaScript functions
   * for the comet actor and for the page the the comet actor is associated with
   */
  def postPageJavaScript(pageIds: Seq[String]): List[JsCmd] = {
    for {
      rv <- pageIds.toList.distinct
      js <- postPageJavaScript(rv)
    } yield js
  }

  /**
   * Get the JavaScript to execute as part of the current page
   */
  def postPageJavaScript(): List[JsCmd] = postPageJavaScript(RenderVersion.get)

  /**
   * Updates the timestamp of the functions owned by this owner and return the
   * number of updated functions
   */
  private[http] def updateFuncByOwner(ownerName: String, time: Long): Int = {
    accessPostPageFuncs {
      for {
        funcInfo <- postPageFunctions.get(ownerName)
      } postPageFunctions += (ownerName -> funcInfo.updateLastSeen)
    }

    withAjaxRequests { currentAjaxRequests =>
      currentAjaxRequests.get(ownerName).foreach { requestInfos =>
        val updated = requestInfos.map(_.copy(lastSeen = time))

        currentAjaxRequests += (ownerName -> updated)
      }
    }

    synchronized {
      (0 /: messageCallback)((l, v) => l + (v._2.owner match {
        case Full(owner) if (owner == ownerName) =>
          v._2.lastSeen = time
          1
        case Empty => v._2.lastSeen = time
        1
        case _ => 0
      }))
    }
  }

  /**
   * Returns true if there are functions bound for this owner
   */
  private[http] def hasFuncsForOwner(owner: String): Boolean = synchronized {
    !messageCallback.find(_._2.owner == owner).isEmpty
  }

  private def shutDown() = {
    var done: List[() => Unit] = Nil

    S.initIfUninitted(this) {
      onSessionEnd.foreach(_(this))
      synchronized {
        LiftSession.onAboutToShutdownSession.foreach(_(this))

        _running_? = false

        SessionMaster.sendMsg(RemoveSession(this.uniqueId))

        asyncComponents.foreach {
          case (_, comp) => done ::= (() => tryo(comp ! ShutDown))
        }
        cleanUpSession()
        LiftSession.onShutdownSession.foreach(f => done ::= (() => f(this)))
      }
    }

    done.foreach(_.apply())
  }

  /**
   * Find the template assocaited with the Loc
   */
  private[http] def locTemplate: Box[NodeSeq] =
    for (loc <- S.location;
         template <- loc.template) yield template

  /**
   * Define the context path for this session.  This allows different
   * sessions to have different context paths.
   */
  def contextPath = LiftRules.calculateContextPath() openOr _contextPath

  /**
   * Convert a template into a Lift Response.
   *
   * @param template -- the NodeSeq that makes up the page... or the template
   * will be located via findVisibleTemplate
   * @param request -- the Req the led to this rendering
   * @param path -- the ParsePath that led to this page
   * @param code -- the HTTP response code (usually 200)
   *
   * @return a Box of LiftResponse with all the proper page rewriting
   */
  def processTemplate(template: Box[NodeSeq], request: Req, path: ParsePath, code: Int): Box[LiftResponse] = {
    overrideResponseCode.doWith(Empty) {
      (template or findVisibleTemplate(path, request)).map {
        xhtml =>
          fullPageLoad.doWith(true) {
            // allow parallel snippets
            // Phase 1: snippets & templates processing
            val rawXml: NodeSeq = processSurroundAndInclude(PageName get, xhtml)

            // Make sure that functions have the right owner. It is important for this to
            // happen before the merge phase so that in merge to have a correct view of
            // mapped functions and their owners.
            updateFunctionMap(S.functionMap, RenderVersion get, millis)

            // Clear the function map after copying it... but it
            // might get some nifty new functions during the merge phase
            S.clearFunctionMap

            // Phase 2: Head & Tail merge, add additional elements to body & head
            val xml = merge(rawXml, request)

            // snapshot for ajax calls
            messageCallback(S.renderVersion) = S.PageStateHolder(Full(S.renderVersion), this)

            // But we need to update the function map because there
            // may be addition functions created during the JsToAppend processing
            // See issue #983
            updateFunctionMap(S.functionMap, RenderVersion get, millis)

            notices = Nil
            // Phase 3: Response conversion including fixHtml
            LiftRules.convertResponse((xml, overrideResponseCode.is openOr code),
              S.getHeaders(LiftRules.defaultHeaders((xml, request))),
              S.responseCookies,
              request)
          }
      }
    }
  }

  private object overrideResponseCode extends TransientRequestVar[Box[Int]](Empty)

  /**
   * If the sitemap entry for this Req is marked stateless,
   * run the rest of the request as stateless
   */
  private def checkStatelessInSiteMap[T](req: Req)(f: => T): T = {
    req.location match {
      case Full(loc) if loc.stateless_? => this.doAsStateless(f)
      case _ => f
    }
  }

  /**
   * Destroy the current session, then create a new session and
   * continue the execution of the code.  The continuation function
   * must return Nothing (it must throw an exception... this is typically
   * done by calling S.redirectTo(...)).  This method is
   * useful for changing sessions on login.  Issue #727.
   */
  def destroySessionAndContinueInNewSession(continuation: () => Nothing): Nothing = {
    throw new ContinueResponseException(continuation)
  }

  private[http] def processRequest(request: Req,
                                   continuation: Box[() => Nothing]): Box[LiftResponse] = {
    ieMode.is // make sure this is primed
    S.oldNotices(notices)
    LiftSession.onBeginServicing.foreach(f => tryo(f(this, request)))
    val ret = try {
      // run the continuation in the new session
      // if there is a continuation
      continuation match {
        case Full(func) => {
          func()
          S.redirectTo("/")
        }
        case _ => // do nothing
      }

      val sessionDispatch = S.highLevelSessionDispatcher

      val toMatch = request
      NamedPF.applyBox(toMatch, sessionDispatch) match {
        case Full(f) =>
          runParams(request)
          try {
            f() match {
              case Full(r) => Full(checkRedirect(r))
              case _ => LiftRules.notFoundOrIgnore(request, Full(this))
            }
          } finally {
            notices = S.getAllNotices
          }

        case _ =>
          RenderVersion.get // touch this early

          runParams(request)

          val early = LiftRules.preAccessControlResponse_!!.firstFull(request)

          // Process but make sure we're okay, sitemap wise
          val response: Box[LiftResponse] = early or (request.testLocation match {
            case Left(true) =>
              checkStatelessInSiteMap(request) {
                cleanUpBeforeRender

                PageName(request.uri + " -> " + request.path)
                LiftRules.allowParallelSnippets.doWith(() => !Props.inGAE) {
                  (request.location.flatMap(_.earlyResponse) or LiftRules.earlyResponse.firstFull(request)) or
                    (processTemplate(locTemplate, request, request.path, 200) or
                      request.createNotFound {
                        processTemplate(Empty, request, _, 404)
                      })
                }
              }

            case Right(Full(resp)) => Full(resp)
            case _ if (LiftRules.passNotFoundToChain) => Empty
            case _ if Props.mode == Props.RunModes.Development =>
              request.createNotFound {
                processTemplate(Empty, request, _, 404)
              } or
                Full(ForbiddenResponse("The requested page was not defined in your SiteMap, so access was blocked.  (This message is displayed in development mode only)"))
            case _ => request.createNotFound {
              processTemplate(Empty, request, _, 404)
            }
          })

          // Before returning the response check for redirect and set the appropriate state.
          response.map(checkRedirect)
      }
    } catch {
      case ContinueResponseException(cre) => throw cre

      case ite: java.lang.reflect.InvocationTargetException if (ite.getCause.isInstanceOf[ResponseShortcutException]) =>
        Full(handleRedirect(ite.getCause.asInstanceOf[ResponseShortcutException], request))

      case rd: net.liftweb.http.ResponseShortcutException => Full(handleRedirect(rd, request))

      case e: LiftFlowOfControlException => throw e

      case e => S.runExceptionHandlers(request, e)

    }

    LiftSession.onEndServicing.foreach(f => tryo(f(this, request, ret)))
    ret
  }

  /**
   * Merge all the head elements into the main head element and move tail stuff to the end of the
   * page.
   */
  def performHeadMerge(in: NodeSeq, req: Req): Node = merge(in, req)

  private def cleanUpBeforeRender {
    // Reset the mapping between ID and Style for Ajax notices.
    MsgErrorMeta(new HashMap)
    MsgWarningMeta(new HashMap)
    MsgNoticeMeta(new HashMap)
  }

  private[http] def handleRedirect(re: ResponseShortcutException, request: Req): LiftResponse = {
    if (re.doNotices) notices = S.getAllNotices

    re.response
  }

  /**
   * Set a session-local variable to a value
   *
   * @param name -- the name of the variable
   * @param value -- the value of the variable
   */
  private[liftweb] def set[T](name: String, value: T): Unit = sessionVarSync.synchronized {
    myVariables = myVariables + (name -> value)
  }

  /**
   * Gets the named variable if it exists
   *
   * @param name -- the name of the session-local variable to get
   *
   * @return Full ( value ) if found, Empty otherwise
   */
  private[liftweb] def get[T](name: String): Box[T] = sessionVarSync.synchronized {
    Box(myVariables.get(name)).asInstanceOf[Box[T]]
  }

  /**
   * Unset the named variable
   *
   * @param name the variable to unset
   */
  private[liftweb] def unset(name: String): Unit = sessionVarSync.synchronized {
    myVariables -= name
  }


  private[http] def attachRedirectFunc(uri: String, f: Box[() => Unit]) = {
    f map {
      fnc =>
        val func: String = LiftSession.this.synchronized {
          val funcName = Helpers.nextFuncName
          messageCallback(funcName) = S.NFuncHolder(() => {
            fnc()
          })
          funcName
        }
        Helpers.appendFuncToURL(uri, func + "=_")
    } openOr uri

  }

  private[http] def checkRedirect(resp: LiftResponse): LiftResponse = resp match {
    case RedirectWithState(uri, state, cookies) =>
      state.msgs.foreach(m => S.message(m._1, m._2))
      notices = S.getAllNotices
      RedirectResponse(attachRedirectFunc(uri, state.func), cookies: _*)
    case _ => resp
  }


  private def allElems(in: NodeSeq, f: Elem => Boolean): List[Elem] = {
    val lb = new ListBuffer[Elem]

    def appendAll(in: NodeSeq, lb: ListBuffer[Elem]) {
      in.foreach {
        case Group(ns) => appendAll(ns, lb)
        case e: Elem if f(e) => lb += e; appendAll(e.child, lb)
        case e: Elem => appendAll(e.child, lb)
        case _ =>
      }
    }
    appendAll(in, lb)

    lb.toList
  }

  private def findVisibleTemplate(path: ParsePath, session: Req): Box[NodeSeq] = {
    val tpath = path.partPath
    val splits = tpath.toList.filter {
      a => !a.startsWith("_") && !a.startsWith(".") && a.toLowerCase.indexOf("-hidden") == -1
    } match {
      case s@_ if (!s.isEmpty) => s
      case _ => List("index")
    }
    Templates(splits, S.locale)
  }

  private[liftweb] def findTemplate(name: String): Box[NodeSeq] = {
    val splits = (if (name.startsWith("/")) name else "/" + name).split("/").toList.drop(1) match {
      case Nil => List("index")
      case s => s
    }

    Templates("templates-hidden" :: splits, S.locale) match {
      case Full(x) => Full(x)
      case f: Failure if Props.devMode => f
      case _ => Templates(splits, S.locale)
    }
  }

  /*
   * Given a Snippet name, try to determine the fully-qualified Class
   * so that we can instantiate it via reflection.
   */
  private def findSnippetClass(name: String): Box[Class[AnyRef]] = {
    if (name == null) Empty
    else {
      // Name might contain some relative packages, so split them out and put them in the proper argument of findClass
      val (packageSuffix, terminal) = name.lastIndexOf('.') match {
        case -1 => ("", name)
        case i => ("." + name.substring(0, i), name.substring(i + 1))
      }
      findClass(terminal, LiftRules.buildPackage("snippet").map(_ + packageSuffix) :::
        (("lift.app.snippet" + packageSuffix) :: ("net.liftweb.builtin.snippet" + packageSuffix) :: Nil))
    }
  }

  private def instantiateOrRedirect[T](c: Class[T]): Box[T] = {
    try {
      LiftSession.constructFrom(this,
        S.location.flatMap(_.
          currentValue.map(v =>
          ParamPair(v, v.asInstanceOf[Object].getClass))),
        c)

    } catch {
      case e: IllegalAccessException => Empty
    }
  }

  private def findAttributeSnippet(attrValue: String, rest: MetaData, params: AnyRef*): MetaData = {
    S.doSnippet(attrValue) {
      val (cls, method) = splitColonPair(attrValue)

      first(LiftRules.snippetNamesToSearch.vend(cls)) {
        nameToTry =>
          findSnippetClass(nameToTry) flatMap {
            clz =>
              instantiateOrRedirect(clz) flatMap {
                inst =>
                  invokeMethod(clz, inst, method) or invokeMethod(clz, inst, method, params.toList.toArray) match {
                    case Full(md: MetaData) => Full(md.copy(rest))
                    case _ => Empty
                  }
              }
          }
      } openOr rest
    }
  }

  private object DotSplit {
    def unapply(in: String): Option[List[String]] = {
      val i = in.lastIndexOf('.')
      if (i >= 0) Some(List(in.substring(0, i), in.substring(i + 1)))
      else None
    }
  }

  private def colonToDot(in: String): String = {
    if (in.indexOf('/') >= 0) {
      val len = in.length()
      val ret = new java.lang.StringBuilder(len)
      var x = 0
      while (x < len) {
        val c = in.charAt(x)
        if (c == '/') {
          ret.append('.')
        }
        else ret.append(c)
        x += 1
      }
      ret.toString
    } else in
  }

  /**
   * Split a string separated by a point or by a column in 2 parts. Uses default values if only one is found or if no parts are found
   * @param in string to split
   * @return a pair containing the first and second parts
   */
  private def splitColonPair(in: String): (String, String) = {
    (in match {
      case null => List("")
      case DotSplit(lst) => lst
      case s => s.roboSplit(":")
    }) match {
      case f :: s :: _ => (colonToDot(f), s)
      case f :: Nil => (colonToDot(f), "render")
      case _ => ("yikes dude, there's no method name defined", "render")
    }
  }


  /**
   * Finds a template named name and then runs it throught the Lift processing engine
   */
  def findAndProcessTemplate(name: List[String]): Box[Elem] = {
    def findElem(in: NodeSeq): Box[Elem] =
      in.toList.flatMap {
        case e: Elem => Some(e)
        case _ => None
      } headOption

    for {
      template <- Templates(name, S.locale) ?~ ("Template " + name + " not found")
      res <- findElem(processSurroundAndInclude(name.mkString("/", "/", ""), template))
    } yield res
  }

  private def processAttributes(in: MetaData, allow: Boolean): MetaData = {
    if (!allow) in else {
    in match {
      case Null => Null
      case mine: PrefixedAttribute if (mine.pre == "lift") => {
        mine.key match {
          case s if s.indexOf('.') > -1 => findAttributeSnippet(s, processAttributes(in.next, allow), mine)
          case "snippet" => findAttributeSnippet(mine.value.text, processAttributes(in.next, allow))
          case _ => mine.copy(processAttributes(in.next, allow))
        }
      }
      case notMine => notMine.copy(processAttributes(in.next, allow))
    }
  }
  }

  /**
   * See if there's a object singleton with the right name
   */
  private def findSnippetObject(cls: String): Box[AnyRef] =
    findSnippetClass(cls + "$").flatMap {
      c =>
        tryo {
          val field = c.getField("MODULE$")
          field.get(null)
        }
    }

  /*
   * We need to locate a snippet instance for the given tag name. We look in
   * this order:
   *
   * 1. Check to see if a StatefulSnippet has already registered itself
   * 2. See if we have a custom snippet dispatch defined in LiftRules
   * 3. Locate a Class or Object based on the snippet name
   *
   * For the cases #2 and #3, we need to set the snippet name if the returned snippet
   * class is a StatefulSnippet so that the registration function works on return calls.
   */
  private def findSnippetInstance(cls: String): Box[AnyRef] =
    S.snippetForClass(cls) or
      (LiftRules.snippet(cls) or
        findSnippetClass(cls).flatMap(c => instantiateOrRedirect(c) or findSnippetObject(cls))) match {
      case Full(inst: StatefulSnippet) =>
        inst.addName(cls); S.overrideSnippetForClass(cls, inst); Full(inst)
      case Full(ret) => Full(ret)
      case fail: Failure => fail
      case _ => Empty
    }

  /**
   * Report a snippet error depending on what the run mode is
   */
  private def reportSnippetError(page: String,
                                 snippetName: Box[String],
                                 why: LiftRules.SnippetFailures.Value,
                                 addlMsg: NodeSeq,
                                 whole: NodeSeq): NodeSeq = {
    (for {
      nodeSeq <- S.currentSnippetNodeSeq if S.ignoreFailedSnippets
    } yield {
      // don't keep nailing the same snippet name if we just failed it
      (snippetName or S.currentSnippet).foreach(s => _lastFoundSnippet.set(s))
      nodeSeq
    }) openOr {

    for {
      f <- LiftRules.snippetFailedFunc.toList
    } {
      f(LiftRules.SnippetFailure(page, snippetName, why))
    }

    if (Props.devMode || Props.testMode) {
      overrideResponseCode.set(LiftRules.devModeFailureResponseCodeOverride)
    }

    Helpers.errorDiv(
      <div>Error processing snippet:
        <b>
          {snippetName openOr "N/A"}
        </b> <br/>
        Reason:
        <b>
          {why}{addlMsg}
        </b> <br/>
        XML causing this error:
          <br/>
        <pre style="background: lightgrey; padding: 6px; border: 1px solid">
          {whole.toString}
        </pre>
      </div>) openOr NodeSeq.Empty
    }
  }

  private final def findNSAttr(attrs: MetaData, prefix: String, key: String): Option[Seq[Node]] =
    attrs match {
      case Null => Empty
      case p: PrefixedAttribute if p.pre == prefix && p.key == key => Some(p.value)
      case x => findNSAttr(x.next, prefix, key)
    }

  /**
   * Wrap an AFuncHolder with the current snippet and Loc context so that for Ajax calls, the original snippets,
   * RequestVars and Loc (location) are populated
   *
   * @param f the AFuncHolder that you want to wrap with execution context
   */
  private[http] def contextFuncBuilder(f: S.AFuncHolder): S.AFuncHolder = {
    val currentMap = snippetMap.is
    val curLoc = S.location

    val requestVarFunc: Function1[Function0[Any], Any] = RequestVarHandler.generateSnapshotRestorer()
    new S.ProxyFuncHolder(f) {
      override def apply(in: List[String]): Any =
        requestVarFunc(() =>
          S.CurrentLocation.doWith(curLoc) {
            snippetMap.doWith(snippetMap.is ++ currentMap) {
              super.apply(in)
            }
          }
        )

      override def apply(in: FileParamHolder): Any =
        requestVarFunc(() =>
          S.CurrentLocation.doWith(curLoc) {
            snippetMap.doWith(snippetMap.is ++ currentMap) {
              super.apply(in)
            }
          }
        )
    }
  }

  /**
   * During the HTTP request/response cycle or in a CometActor,
   * Lift populates "S" with information about the current session,
   * the current request, etc.  This method allows you to wrap a
   * function in another function that will snapshot current state
   * (request vars, Req, Loc, etc.) such that when the returned
   * function is executed, it will be executed as if it had been
   * executed in the scope of the thread where it was create.
   * This allows you to farm work out to separate threads, but make
   * it look to those threads as if the scope was the same as if it
   * had been executed on the thread that created the function.
   */
  def buildDeferredFunction[T](f: () => T): () => T = {
    val currentReq: Box[Req] = S.request.map(_.snapshot)

    val renderVersion = RenderVersion.get

    val currentMap = snippetMap.is
    val curLoc = S.location

    val requestVarFunc = RequestVarHandler.generateSnapshotRestorer[T]()

    () => {
      requestVarFunc(() =>
        executeInScope(currentReq, renderVersion)(f()))
    }
  }

  def executeInScope[T](req: Box[Req], renderVersion: String)(f: => T): T = {
    def doExec(): T = {
      RenderVersion.doWith(renderVersion) {
      try {
        f
      } finally {
        if (S.functionMap.size > 0) {
          this.updateFunctionMap(S.functionMap,
            renderVersion, millis)
          S.clearFunctionMap
        }
      }
      }
    }

    req match {
      case Full(r) => S.init(r, this)(doExec())
      case _ => S.initIfUninitted(this)(doExec())
    }
  }

  private def processSnippet(page: String, snippetName: Box[String],
                             attrs: MetaData,
                             wholeTag: NodeSeq,
                             passedKids: NodeSeq): NodeSeq = {
    val isForm = !attrs.get("form").toList.isEmpty

    val eagerEval: Boolean =
      (attrs.get("eager_eval").map(toBoolean) or
        findNSAttr(attrs, "lift", "eager_eval").map(toBoolean) or
        findNSAttr(attrs, "l", "eager_eval").map(toBoolean)
        ) getOrElse false

    val kids = if (eagerEval) processSurroundAndInclude(page, passedKids) else passedKids

    // Locate a snippet as defined by our SiteMap Loc
    def locSnippet(snippet: String): Box[NodeSeq] =
      for (loc <- S.location;
           func <- loc.snippet(snippet)) yield func(kids)

    def locateAndCacheSnippet(tagName: String): Box[AnyRef] =
      snippetMap.is.get(tagName) or {
        first(LiftRules.snippetNamesToSearch.vend(tagName)) {
          nameToTry =>
            val ret = findSnippetInstance(nameToTry)
            // Update the snippetMap so that we reuse the same instance in this request (unless the snippet is transient)
            ret.filter(TransientSnippet.notTransient(_)).foreach(s => snippetMap.set(snippetMap.is.updated(tagName, s)))

            ret
        }
      }

    def runWhitelist(snippet: String, cls: String, method: String, kids: NodeSeq)(f: => NodeSeq): NodeSeq = {
      val pf = LiftRules.snippetWhiteList.vend()
      val pair = (cls, method)
      if (pf.isDefinedAt(pair)) {
        val func = pf(pair)
        func.map(_.apply(kids)) openOr reportSnippetError(page, snippetName,
                    LiftRules.SnippetFailures.MethodNotFound,
                    NodeSeq.Empty,
                    wholeTag)
        } else f
  }

    val ret: NodeSeq =
      try {

        snippetName.map{snippet =>
          val (cls, method) = splitColonPair(snippet)
          S.doSnippet(snippet)(
            runWhitelist(snippet, cls, method, kids){(S.locateMappedSnippet(snippet).map(_(kids)) or
              locSnippet(snippet)).openOr(
              S.locateSnippet(snippet).map(_(kids)) openOr {

                (locateAndCacheSnippet(cls)) match {
                  // deal with a stateless request when a snippet has
                  // different behavior in stateless mode
                  case Full(inst: StatelessBehavior) if !stateful_? =>
                    if (inst.statelessDispatch.isDefinedAt(method))
                    inst.statelessDispatch(method)(kids) else NodeSeq.Empty

                  case Full(inst: StatefulSnippet) if !stateful_? =>
                    reportSnippetError(page, snippetName,
                      LiftRules.SnippetFailures.StateInStateless,
                      NodeSeq.Empty,
                      wholeTag)

                  case Full(inst: StatefulSnippet) =>
                    if (inst.dispatch.isDefinedAt(method)) {
                      val res = inst.dispatch(method)(kids)

                      inst.mergeIntoForm(isForm, res, SHtml.hidden(() => inst.registerThisSnippet))
                      /* (if (isForm && !res.isEmpty) SHtml.hidden(() => inst.registerThisSnippet) else NodeSeq.Empty) ++
                      res*/
                    } else reportSnippetError(page, snippetName,
                      LiftRules.SnippetFailures.StatefulDispatchNotMatched,
                      NodeSeq.Empty,
                      wholeTag)

                  case Full(inst: DispatchSnippet) =>
                    if (inst.dispatch.isDefinedAt(method)) inst.dispatch(method)(kids)
                    else reportSnippetError(page, snippetName,
                      LiftRules.SnippetFailures.StatefulDispatchNotMatched,
                      NodeSeq.Empty,
                      wholeTag)

                  case Full(inst) => {
                    def gotIt: Box[NodeSeq] =
                      for {
                        meth <- tryo(inst.getClass.getMethod(method))
                        if classOf[CssBindFunc].isAssignableFrom(meth.getReturnType)
                      } yield meth.invoke(inst).asInstanceOf[CssBindFunc].apply(kids)

                    import java.lang.reflect.{Type, ParameterizedType}

                    def isFunc1(tpe: Type): Boolean = tpe match {
                      case null => false
                      case c: Class[_] => classOf[Function1[_, _]] isAssignableFrom c
                      case _ => false
                    }

                    def isNodeSeq(tpe: Type): Boolean = tpe match {
                      case null => false
                      case c: Class[_] => classOf[NodeSeq] isAssignableFrom c
                      case _ => false
                    }

                    def testGeneric(tpe: Type): Boolean = tpe match {
                      case null => false
                      case pt: ParameterizedType =>
                        if (isFunc1(pt.getRawType) &&
                          pt.getActualTypeArguments.length == 2 &&
                          isNodeSeq(pt.getActualTypeArguments()(0)) &&
                          isNodeSeq(pt.getActualTypeArguments()(1)))
                          true
                        else testGeneric(pt.getRawType)

                      case clz: Class[_] =>
                        if (clz == classOf[Object]) false
                        else clz.getGenericInterfaces.find(testGeneric) match {
                          case Some(_) => true
                          case _ => testGeneric(clz.getSuperclass)
                        }

                      case _ => false
                    }

                    def isFuncNodeSeq(meth: Method): Boolean = {
                      (classOf[Function1[_, _]] isAssignableFrom meth.getReturnType) &&
                        testGeneric(meth.getGenericReturnType)
                    }


                    def nodeSeqFunc: Box[NodeSeq] =
                      for {
                        meth <- tryo(inst.getClass.getMethod(method))
                        if isFuncNodeSeq(meth)
                      } yield meth.invoke(inst).asInstanceOf[Function1[NodeSeq,
                        NodeSeq]].apply(kids)


                    (gotIt or nodeSeqFunc) openOr {

                      val ar: Array[AnyRef] = List(Group(kids)).toArray
                      ((Helpers.invokeMethod(inst.getClass, inst, method, ar)) or
                        Helpers.invokeMethod(inst.getClass, inst, method)) match {
                        case CheckNodeSeq(md) => md
                        case it =>
                          val intersection = if (Props.devMode) {
                            val methodNames = inst.getClass.getMethods().map(_.getName).toList.distinct
                            val methodAlts = List(method, Helpers.camelify(method),
                              Helpers.camelifyMethod(method))
                            methodNames intersect methodAlts
                          } else Nil

                          reportSnippetError(page, snippetName,
                            LiftRules.SnippetFailures.MethodNotFound,
                            if (intersection.isEmpty) NodeSeq.Empty
                            else
                              <div>There are possible matching methods (
                                {intersection}
                                ),
                                but none has the required signature:
                                <pre>def
                                  {method}
                                  (in: NodeSeq): NodeSeq</pre>
                              </div>,
                            wholeTag)
                      }
                    }
                  }
                  case Failure(_, Full(exception), _) => logger.warn("Snippet instantiation error", exception)
                  reportSnippetError(page, snippetName,
                    LiftRules.SnippetFailures.InstantiationException,
                    NodeSeq.Empty,
                    wholeTag)

                  case _ => reportSnippetError(page, snippetName,
                    LiftRules.SnippetFailures.ClassNotFound,
                    NodeSeq.Empty,
                    wholeTag)

                }
              })})}.openOr {
          reportSnippetError(page, snippetName,
            LiftRules.SnippetFailures.NoNameSpecified,
            NodeSeq.Empty,
            wholeTag)
        }
      } catch {
        case ExclosedSnippetFailure(e) =>
          reportSnippetError(page, snippetName,
            e.snippetFailure,
            e.buildStackTrace,
            wholeTag)

        case e: SnippetFailureException =>
          reportSnippetError(page, snippetName,
            e.snippetFailure,
            e.buildStackTrace,
            wholeTag)
      }

    def checkMultiPart(in: MetaData): MetaData = in.filter(_.key == "multipart").toList match {
      case Nil => Null
      case x => new UnprefixedAttribute("enctype", Text("multipart/form-data"), Null)
    }

    def checkAttr(attr_name: String, in: MetaData, base: MetaData): MetaData =
      in.filter(_.key == attr_name).toList match {
        case Nil => base
        case x => new UnprefixedAttribute(attr_name, Text(x.head.value.text),
          base)
      }

    if (ret.isEmpty) ret
    else
      attrs.get("form").map(_.text.trim.toLowerCase) match {
        case Some("post") =>
          S.withAttrs(attrs.filter(_.key == "multipart")) {
            net.liftweb.builtin.snippet.Form.post(ret)
          } match {
            case e: Elem => e % LiftRules.formAttrs.vend.foldLeft[MetaData](Null)((base, name) => checkAttr(name, attrs, base))
            case x => x
          }

        case Some(ft) =>
          <form action={S.uri} method={ft}>
            {ret}
          </form> %
            checkMultiPart(attrs) % LiftRules.formAttrs.vend.foldLeft[MetaData](Null)((base, name) => checkAttr(name, attrs, base))
        case _ => ret
      }

  }

  private object ExclosedSnippetFailure {
    def unapply(e: Throwable): Option[SnippetFailureException] = e.getCause match {
      case null => None
      case e: SnippetFailureException => Some(e)
      case _ => None
    }
  }

  /**
   * Apply HTML specific corrections such as adding the context path etc.
   */
  def fixHtml(in: NodeSeq): NodeSeq = Req.fixHtml(contextPath, in)


  /**
   * The partial function that defines how lift tags are processed for this session.  Initially composed
   * of LiftRules.liftTagProcessing orElse the default lift tag processing.  If you need to change the
   * way a particular session handles lift tags, alter this partial function.
   */
  @volatile
  var liftTagProcessing: List[LiftRules.LiftTagPF] = _

  /**
   * The basic partial function that does lift tag processing
   */
  private def _defaultLiftTagProcessing: LiftRules.LiftTagPF =
    NamedPF("Default Lift Tags") {
      case ("snippet", elm, metaData, kids, page) =>
        metaData.get("type") match {
          case Some(tn) =>
            S.doSnippet(tn.text) {
              NamedPF((tn.text, elm, metaData, kids, page),
                liftTagProcessing)
            }

          case _ => processSnippet(page, Empty, elm.attributes, elm, elm.child)
        }
      case (snippetInfo, elm, metaData, kids, page) =>
        processSnippet(page, Full(snippetInfo), metaData, elm, kids)
    }

  liftTagProcessing = LiftRules.liftTagProcessing.toList ::: List(_defaultLiftTagProcessing)

  private def asNodeSeq(in: Seq[Node]): NodeSeq = in


  private class DeferredProcessor extends SpecializedLiftActor[ProcessSnippet] {
    protected def messageHandler = {
      case ProcessSnippet(f) => f()
    }
  }

  private case class ProcessSnippet(f: () => Unit)

  // if the "lift:parallel" attribute is part of the snippet, create an
  // actor and send the message off to that actor
  private def processOrDefer(isLazy: Boolean)(f: => NodeSeq): NodeSeq = {
    /*
    val isLazy = LiftRules.allowParallelSnippets() &&
            node.attributes.find {
              case p: PrefixedAttribute => p.pre == "lift" && (p.key == "parallel")
              case _ => false
            }.isDefined
            */

    if (fullPageLoad.? && isLazy && LiftRules.allowParallelSnippets()) {
      // name the node
      val nodeId = randomString(20)

      val renderVersion = RenderVersion.get

      val theNode = <lift_deferred:node id={nodeId}/>

      // take a snapshot of the hashmap used to communicate between threads
      val hash = deferredSnippets.is

      // insert an empty node
      hash.synchronized {
        hash(nodeId) = Empty
      }

      // create a function that will restore our RequestVars
      val reqVarCallback = deferredSnippets.generateSnapshotRestorer[NodeSeq]()

      // create a new actor
      val actor = new DeferredProcessor

      // snapshot the current Req
      val req = S.request.map(_.snapshot)

      // send the ProcessSnippet message to the Actor
      actor ! ProcessSnippet(() => {
        executeInScope(req, renderVersion) {
          // process the message
          val bns = tryo {
            reqVarCallback(() => f)
          }

          // set the node
          hash.synchronized {
            hash(nodeId) = bns match {
              case Empty => Failure("Weird Empty Node", Empty, Empty)
              case x => x
            }

            // and notify listeners
            hash.notify()
          }
        }
      })

      theNode
    }
    else f
  }

  private object _lastFoundSnippet extends ThreadGlobal[String]

  /**
   * Processes the surround tag and other lift tags
   *
   * @param page the name of the page currently being processed
   * @param in the DOM to process
   */
  def processSurroundAndInclude(page: String, in: NodeSeq): NodeSeq = {
    try {
      in.flatMap {
        case Group(nodes) =>
          Group(processSurroundAndInclude(page, nodes))

        case elem @ SnippetNode(element, kids, isLazy, attrs, snippetName) if snippetName != _lastFoundSnippet.value =>
          processOrDefer(isLazy) {
            S.withCurrentSnippetNodeSeq(elem) {
              S.doSnippet(snippetName) {
                S.withAttrs(attrs) {
                  processSurroundAndInclude(page,
                    NamedPF((snippetName,
                      element, attrs,
                      kids,
                      page),
                      liftTagProcessing))
                }
              }
            }
          }

        case v: Elem =>
          Elem(v.prefix, v.label, processAttributes(v.attributes, this.allowAttributeProcessing.is),
            v.scope, processSurroundAndInclude(page, v.child): _*)

        case pcd: scala.xml.PCData => pcd
        case text: Text => text
        case unparsed: Unparsed => unparsed

        case a: Atom[Any] if (a.getClass == classOf[Atom[Any]]) => new Text(a.data.toString)

        case v => v
      }
    } finally {
      _lastFoundSnippet.set(null)
    }
  }

  /**
   * A nicely named proxy for processSurroundAndInclude.  This method processes
   * a Lift template
   *
   * @param pageName -- the name of the page being processed (for error reporting)
   * @param template -- the template to process using Lift's templating engine
   */
  def runTemplate(pageName: String, template: NodeSeq): NodeSeq =
    processSurroundAndInclude(pageName, template)

  /**
   * Run the code, but if the session is not stateful, then
   * throw a StateInStatelessException
   */
  def testStatefulFeature[T](f: => T): T = {
    if (this.stateful_?) f
    else throw new StateInStatelessException(
      "Accessing stateful feature outside of a stateful session")
  }

  /**
   * Finds all Comet actors by type
   */
  def findComet(theType: String): List[LiftCometActor] = synchronized {
    testStatefulFeature {
      asyncComponents.flatMap {
        case ((Full(name), _), value) if name == theType => Full(value)
        case _ => Empty
      }.toList
    }
  }

  /**
   * Find the comet actor by type and name
   */
  def findComet(theType: String, name: Box[String]): Box[LiftCometActor] = synchronized {
    testStatefulFeature {
      asyncComponents.get(Full(theType) -> name)
    }
  }


  /**
   * This method will send a message to a CometActor, whether or not
   * the CometActor is instantiated.  If the CometActor already exists
   * in the session, the message will be sent immediately.  If the CometActor
   * is not yet instantiated, the message will be sent to the CometActor
   * as part of setup (@see setupComet) if it is created as part
   * of the current HTTP request/response cycle.
   *
   * @param theType the type of the CometActor
   * @param name the optional name of the CometActor
   * @param msg the message to send to the CometActor
   */
  def sendCometActorMessage(theType: String, name: Box[String], msg: Any) {
    testStatefulFeature {
      findComet(theType, name) match {
        case Full(a) => a ! msg
        case _ => setupComet(theType, name, msg)
      }
    }
  }

  /**
   * Allows you to send messages to a CometActor that may or may not be set up yet
   */
  def setupComet(theType: String, name: Box[String], msg: Any) {
    testStatefulFeature {
      cometSetup.atomicUpdate(v => (Full(theType) -> name, msg) :: v)
    }
  }

  private[liftweb] def findComet(theType: Box[String], name: Box[String],
                                 defaultXml: NodeSeq,
                                 attributes: Map[String, String]): Box[LiftCometActor] = {
    testStatefulFeature {
      val what = (theType -> name)
      val ret = synchronized {

        val ret = Box(asyncComponents.get(what)).or({
          theType.flatMap {
            tpe =>
              val ret = findCometByType(tpe, name, defaultXml, attributes)
              ret.foreach(r =>
                synchronized {
                  asyncComponents(what) = r
                  asyncById(r.uniqueId) = r
                })
              ret
          }
        })

        ret
      }

      for {
        actor <- ret
        (cst, csv) <- cometSetup.is if cst == what
      } actor ! csv

      cometSetup.atomicUpdate(v => v.filter(_._1 != what))

      ret
    }
  }


  /**
   * Finds a Comet actor by ID
   */
  def getAsyncComponent(id: String): Box[LiftCometActor] = synchronized(
    testStatefulFeature(asyncById.get(id)))

  /**
   * Adds a new Comet actor to this session
   */
  private[http] def addCometActor(act: LiftCometActor): Unit = synchronized {
    testStatefulFeature {
      asyncById(act.uniqueId) = act
    }
  }

  private[liftweb] def addAndInitCometActor(act: LiftCometActor,
                                            theType: Box[String],
                                            name: Box[String],
                                            defaultXml: NodeSeq,
                                            attributes: Map[String, String]) = {
    testStatefulFeature {
      val what = (theType -> name)
      synchronized {
        asyncById(act.uniqueId) = act
        asyncComponents(what) = act
      }
      act.callInitCometActor(this, theType, name, defaultXml, attributes)
      act ! PerformSetupComet2(if (act.sendInitialReq_?)
        S.request.map(_.snapshot)
      else Empty)
    }
  }

  /**
   * Remove a Comet actor
   */
  private[http] def removeCometActor(act: LiftCometActor): Unit = synchronized {
    testStatefulFeature {
      asyncById -= act.uniqueId
      messageCallback -= act.jsonCall.funcId
      asyncComponents -= (act.theType -> act.name)

      val toCmp = Full(act.uniqueId)

      messageCallback.foreach {
        case (k, f) =>
          if (f.owner == toCmp) messageCallback -= k
      }

      accessPostPageFuncs {
        postPageFunctions -= act.uniqueId
      }

      val id = Full(act.uniqueId)
      messageCallback.keysIterator.foreach {
        k =>
          val f = messageCallback(k)
          if (f.owner == id) {
            messageCallback -= k
          }
      }
    }
  }

  private def findCometByType(contType: String,
                              name: Box[String],
                              defaultXml: NodeSeq,
                              attributes: Map[String, String]): Box[LiftCometActor] = {
    testStatefulFeature {
      val createInfo = CometCreationInfo(contType, name, defaultXml, attributes, this)

      val boxCA: Box[LiftCometActor] = LiftRules.cometCreationFactory.vend.apply(createInfo).map {
        a => a ! PerformSetupComet2(if (a.sendInitialReq_?)
          S.request.map(_.snapshot)
        else Empty);
        a
      } or
        LiftRules.cometCreation.toList.find(_.isDefinedAt(createInfo)).map(_.apply(createInfo)).map {
          a => a ! PerformSetupComet2(if (a.sendInitialReq_?)
            S.request.map(_.snapshot)
          else Empty);
          a
        } or
        (findType[LiftCometActor](contType, LiftRules.buildPackage("comet") ::: ("lift.app.comet" :: Nil)).flatMap {
          cls =>
            tryo((e: Throwable) => e match {
              case e: java.lang.NoSuchMethodException => ()
              case e => logger.info("Comet find by type Failed to instantiate " + cls.getName, e)
            }) {
              val constr = cls.getConstructor()
              val ret = constr.newInstance().asInstanceOf[LiftCometActor]
              ret.callInitCometActor(this, Full(contType), name, defaultXml, attributes)

              ret ! PerformSetupComet2(if (ret.sendInitialReq_?)
                S.request.map(_.snapshot)
              else Empty)
              ret.asInstanceOf[LiftCometActor]
            } or tryo((e: Throwable) => logger.info("Comet find by type Failed to instantiate " + cls.getName, e)) {
              val constr = cls.getConstructor(this.getClass, classOf[Box[String]], classOf[NodeSeq], classOf[Map[String, String]])
              val ret = constr.newInstance(this, name, defaultXml, attributes).asInstanceOf[LiftCometActor];

              ret ! PerformSetupComet2(if (ret.sendInitialReq_?)
                S.request.map(_.snapshot)
              else Empty)
              ret.asInstanceOf[LiftCometActor]
            }
        })
      boxCA.foreach {
        _.setCometActorLocale(S.locale)
      }

      boxCA
    }
  }

  private def failedFind(in: Failure): NodeSeq =
    <html xmlns:lift="http://liftweb.net" xmlns="http://www.w3.org/1999/xhtml">
        <head/>
      <body>
        {Helpers.errorDiv(
        <div>Error locating template.
            <br/>
          Message:
          <b>
            {in.msg}
          </b> <br/>{in.exception.map(e => <pre>
          {e.toString}{e.getStackTrace.map(_.toString).mkString("\n")}
        </pre>).openOr(NodeSeq.Empty)}
        </div>) openOr NodeSeq.Empty}
      </body>
    </html>

  private[liftweb] def findAndMerge(templateName: Box[String], atWhat: Map[String, NodeSeq]): NodeSeq = {
    val name: String = templateName.map(s => if (s.startsWith("/")) s else "/" + s).openOr("/templates-hidden/default")

    def hasLiftBind(s: NodeSeq): Boolean =
      (Helpers.findOption(s) {
        case e if "lift" == e.prefix && "bind" == e.label => Some(true)
        case _ => None
      }).isDefined

    findTemplate(name) match {
      case f@Failure(msg, be, _) if Props.devMode =>
        failedFind(f)
      case Full(s) =>
        if (hasLiftBind(s)) Helpers.bind(atWhat, s)
        else atWhat.toList match {
          case Nil => s
          case xs => xs.map {
            case (id, replacement) => (("#" + id) #> replacement)
          }.reduceLeft(_ & _)(s)
        }
      case _ => atWhat.valuesIterator.toSeq.flatMap(_.toSeq).toList
    }
  }

}


/**
 * The response from a page saying that it's been rendered
 */
case object ShutDown

/**
 * If a class is to be used as a lift view (rendering from code rather than a static template)
 * and the method names are to be used as "actions", the view must be marked as "InsecureLiftView"
 * because there exists the ability to execute arbitrary methods based on wire content
 */
trait InsecureLiftView

/**
 *  The preferred way to do lift views... implement a partial function that dispatches
 * the incoming request to an appropriate method
 */
trait LiftView {
  implicit def nsToCns(in: NodeSeq): Box[NodeSeq] = Box.legacyNullTest(in)

  def dispatch: PartialFunction[String, () => Box[NodeSeq]]
}

// an object that extracts an elem that defines a snippet
private object SnippetNode {
  private def removeLift(str: String): String =
    str.indexOf(":") match {
      case x if x >= 0 => str.substring(x + 1)
      case _ => str
    }

  private def makeMetaData(key: String, value: String, rest: MetaData): MetaData = key.indexOf(":") match {
    case x if x > 0 => new PrefixedAttribute(key.substring(0, x),
      key.substring(x + 1),
      value, rest)

    case _ => new UnprefixedAttribute(key, value, rest)
  }

  private def pairsToMetaData(in: List[String]): MetaData = in match {
    case Nil => Null
    case x :: xs => {
      val rest = pairsToMetaData(xs)
      x.charSplit('=').map(Helpers.urlDecode) match {
        case Nil => rest
        case x :: Nil => makeMetaData(x, "", rest)
        case x :: y :: _ => makeMetaData(x, y, rest)
      }
    }
  }

  private def isLiftClass(s: String): Boolean =
    s.startsWith("lift:") || s.startsWith("l:")

  private def snippy(in: Elem): Option[(String, MetaData)] =
    ((for {
      cls <- in.attribute("class")
      snip <- cls.text.charSplit(' ').find(isLiftClass)
    } yield snip) orElse in.attribute("lift").map(_.text)
      orElse in.attribute("data-lift").map(_.text)).map {
      snip =>
        snip.charSplit('?') match {
          case Nil => "this should never happen" -> Null
          case x :: Nil => urlDecode(removeLift(x)) -> Null
          case x :: xs => urlDecode(removeLift(x)) -> pairsToMetaData(xs.flatMap(_.roboSplit("[;&]")))
        }
    }

  private def liftAttrsAndParallel(in: MetaData): (Boolean, MetaData) = {
    var next = in
    var par = false
    var nonLift: MetaData = Null

    while (next != Null) {
      next match {
        // remove the lift class css classes from the class attribute
        case up: UnprefixedAttribute if up.key == "class" =>
          up.value.text.charSplit(' ').filter(s => !isLiftClass(s)) match {
            case Nil =>
            case xs => nonLift = new UnprefixedAttribute("class",
              xs.mkString(" "),
              nonLift)
          }

        case p: PrefixedAttribute
          if (p.pre == "l" || p.pre == "lift") && p.key == "parallel"
        => par = true

        case up: UnprefixedAttribute if up.key == "lift" || up.key == "data-lift" => // ignore

        case p: PrefixedAttribute
          if p.pre == "lift" && p.key == "snippet"
        => nonLift = p.copy(nonLift)

        case a => nonLift = a.copy(nonLift)
      }
      next = next.next
    }


    (par, nonLift)
  }


  def unapply(baseNode: Node): Option[(Elem, NodeSeq, Boolean, MetaData, String)] =
    baseNode match {
      case elm: Elem if elm.prefix == "lift" || elm.prefix == "l" => {
        Some((elm, elm.child,
          elm.attributes.find {
            case p: PrefixedAttribute => p.pre == "lift" && (p.key == "parallel")
            case _ => false
          }.isDefined,
          elm.attributes, elm.label))
      }

      case elm: Elem => {
        for {
          (snippetName, lift) <- snippy(elm)
        } yield {
          val (par, nonLift) = liftAttrsAndParallel(elm.attributes)
          val newElm = new Elem(elm.prefix, elm.label,
            nonLift, elm.scope, elm.child: _*)
          (newElm, newElm, par ||
            (lift.find {
              case up: UnprefixedAttribute if up.key == "parallel" => true
              case _ => false
            }.
              flatMap(up => AsBoolean.unapply(up.value.text)) getOrElse
              false), lift, snippetName)

        }
      }

      case _ => {
        None
      }
    }
}
