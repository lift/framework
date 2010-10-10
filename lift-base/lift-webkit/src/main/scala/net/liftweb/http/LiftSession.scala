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

import _root_.scala.collection.mutable.{HashMap, ArrayBuffer, ListBuffer}
import _root_.scala.xml._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.actor._
import _root_.net.liftweb.http.js.{JsCmd, AjaxInfo}
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.builtin.snippet._
import _root_.java.lang.reflect.{Method, Modifier, InvocationTargetException}
import _root_.scala.xml._
import _root_.java.io.InputStream
import _root_.java.util.concurrent.TimeUnit
import _root_.java.util.Locale
import js._
import scala.reflect.Manifest
import provider._
import _root_.net.liftweb.actor._
import Box._


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
   * Holds user's functions that will be called when a stateful request has been processed
   */
  var onEndServicing: List[(LiftSession, Req, Box[LiftResponse]) => Unit] = Nil
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

      for ((id, info @ SessionInfo(session, _, _, _, _)) <- ses.elements) {
        if (now - session.lastServiceTime > session.inactivityLength || session.markedForTermination) {
          logger.info(" Session " + id + " expired")
          destroyer(info)
        } else {
          session.doCometActorCleanup()
          session.cleanupUnseenFuncs()
        }
      }}) :: Nil

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

  private def lockRead[T](f: => T): T = this.synchronized{f}

  private def lockWrite[T](f: => T): T = this.synchronized{f}

  /**
   * Adds a new session to SessionMaster
   */
  def addSession(liftSession: LiftSession, userAgent: Box[String], ipAddress: Box[String]) {
    lockAndBump {
      Full(SessionInfo(liftSession, userAgent, ipAddress, -1, 0L)) // bumped twice during session creation.  Ticket #529 DPP
    }
    liftSession.startSession()
    liftSession.httpSession.foreach(_.link(liftSession))
  }

  protected def messageHandler = reaction

  /**
   * Shut down all sessions
   */
  private[http] def shutDownAllSessions() {
    val ses = lockRead(sessions)
    ses.keySet.foreach(k => this ! RemoveSession(k))
    while(true) {
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
                try {
                  s.doShutDown
                  try {
                    s.httpSession.foreach(_.unlink(s))
                  } catch {
                    case e => // ignore... sometimes you can't do this and it's okay
                  }
                } catch {
                  case e => logger.warn("Failure in remove session", e)

                } finally {
                  lockWrite {sessions = sessions - sessionId}
                }
      }

    case CheckAndPurge =>
      val ses = lockRead {sessions}

      for {
        f <- sessionCheckFuncs
      } {
        if (Props.inGAE) {
          f(ses, shutDown => this.sendMsg(RemoveSession(shutDown.session.uniqueId)))
        } else {
          ActorPing.schedule(() => f(ses, shutDown => this ! RemoveSession(shutDown.session.uniqueId)), 0 seconds)
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
        ActorPing schedule (this, CheckAndPurge, 10 seconds)
      } catch {
        case e => logger.error("Couldn't start SessionMaster ping", e)
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

  def set(value: String) {
    ver(value)
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
}


/**
 * The LiftSession class containg the session state information
 */
@serializable
class LiftSession(private[http] val _contextPath: String, val uniqueId: String,
                  val httpSession: Box[HTTPSession]) extends LiftMerge with Loggable with HowStateful {
  import TemplateFinder._

  type AnyActor = {def !(in: Any): Unit}

  @volatile
  private[http] var markedForTermination = false

  @volatile
  private var _running_? = false

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

  @volatile
  private[http] var lastServiceTime = millis

  @volatile
  private[http] var inactivityLength: Long = 30 minutes

  private[http] var highLevelSessionDispatcher = new HashMap[String, LiftRules.DispatchPF]()
  private[http] var sessionRewriter = new HashMap[String, LiftRules.RewritePF]()


  private object snippetMap extends RequestVar[Map[String, AnyRef]](Map())
  private[http] object deferredSnippets extends RequestVar[HashMap[String, Box[NodeSeq]]](new HashMap)
  private object cometSetup extends RequestVar[List[((Box[String], Box[String]), Any)]](Nil)


  private[http] def startSession(): Unit = {
    _running_? = true
    for (sess <- httpSession) {
      inactivityLength = sess.maxInactiveInterval * 1000L
    }

    lastServiceTime = millis
    LiftSession.onSetupSession.foreach(_(this))
  }

  def running_? = _running_?

  private var cometList: List[(AnyActor, Req)] = Nil

  private[http] def breakOutComet(): Unit = {
    val cl = synchronized {cometList}
    cl.foreach(_._1 ! BreakOut())
  }

  private[http] def cometForHost(hostAndPath: String): List[(AnyActor, Req)] =
  synchronized {cometList}.filter{
    case (_, r) => r.hostAndPath == hostAndPath
  }

  private[http] def enterComet(what: (AnyActor, Req)): Unit = synchronized {
    cometList = what :: cometList
  }

  private[http] def exitComet(what: AnyActor): Unit = synchronized {
    cometList = cometList.remove(_._1 eq what)
  }

  private case class RunnerHolder(name: String, func: S.AFuncHolder, owner: Box[String])

  object ieMode extends SessionVar[Boolean](LiftRules.calcIEMode())  {
    override private[liftweb] def magicSessionVar_? = true
  }

  def terminateHint {
    if (_running_?) {
      markedForTermination = true;
    }
  }

  /**
   * Executes the user's functions based on the query parameters
   */
  def runParams(state: Req): List[Any] = {

    val toRun = {
      // get all the commands, sorted by owner,
      (state.uploadedFiles.map(_.name) ::: state.paramNames).
              flatMap {n => synchronized {messageCallback.get(n)}.map(mcb => RunnerHolder(n, mcb, mcb.owner))}.
              sort {
        case (RunnerHolder(_, _, Full(a)), RunnerHolder(_, _, Full(b))) if a < b => true
        case (RunnerHolder(_, _, Full(a)), RunnerHolder(_, _, Full(b))) if a > b => false
        case (RunnerHolder(an, _, Full(a)), RunnerHolder(bn, _, Full(b))) if a == b => an < bn
        case (RunnerHolder(_, _, Full(_)), _) => false
        case (_, RunnerHolder(_, _, Full(_))) => true
        case (RunnerHolder(a, _, _), RunnerHolder(b, _, _)) => a < b
      }
    }

    def buildFunc(i: RunnerHolder): () => Any = i.func match {
      case bfh if bfh.supportsFileParams_? =>
        () => state.uploadedFiles.filter(_.name == i.name).map(v => bfh(v))
      case normal =>
        () => normal(state.params.getOrElse(i.name,
          state.uploadedFiles.filter(_.name == i.name).map(_.fileName)))
    }

    val ret = toRun.map(_.owner).removeDuplicates.flatMap {
      w =>
              val f = toRun.filter(_.owner == w)
              w match {
              // if it's going to a CometActor, batch up the commands
                case Full(id) if asyncById.contains(id) =>
                  asyncById.get(id).toList.
                          flatMap(a => a.!?(5000L, ActionMessageSet(f.map(i => buildFunc(i)), state)) match {
                    case Full(li: List[_]) => li
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
    funcs.foreach {case (name, func) => messageCallback(name) = func.duplicate(uniqueId)}
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
    val acl = synchronized {this.asyncComponents.values.toList}
    acl.foreach(_ ! ShutdownIfPastLifespan)
  }

  /**
   * Adds a cleanup function that will be executed when session is terminated
   */
  def addSessionCleanup(f: LiftSession => Unit): Unit = synchronized {
    onSessionEnd = f :: onSessionEnd
  }

  private[http] def doShutDown() {
    if (running_?) {
      // only deal with comet on stateful sessions
      // stateless temporary sessions bar comet use
      if (stateful_?) {
        this.breakOutComet()
        Thread.sleep(100)
      }
      this.shutDown()
    }
  }

  private[http] def cleanupUnseenFuncs(): Unit = synchronized {
    if (LiftRules.enableLiftGC && stateful_?) {
      val now = millis
      messageCallback.keys.toList.foreach {
        k =>
                val f = messageCallback(k)
                if (!f.sessionLife && f.owner.isDefined && (now - f.lastSeen) > LiftRules.unusedFunctionsLifeTime) {
                  messageCallback -= k
                }
      }
    }
  }

  /**
   * Updates the timestamp of the functions owned by this owner and return the
   * number of updated functions
   */
  private[http] def updateFuncByOwner(ownerName: String, time: Long): Int = synchronized {
    (0 /: messageCallback)((l, v) => l + (v._2.owner match {
      case Full(owner) if (owner == ownerName) =>
        v._2.lastSeen = time
        1
      case Empty => v._2.lastSeen = time
      1
      case _ => 0
    }))
  }

  /**
   * Returns true if there are functions bound for this owner
   */
  private[http] def hasFuncsForOwner(owner: String): Boolean = synchronized{!messageCallback.find(_._2.owner == owner).isEmpty}

  private def shutDown() = {
    var done: List[() => Unit] = Nil

    S.initIfUninitted(this) {
      onSessionEnd.foreach(_(this))
      synchronized {
        LiftSession.onAboutToShutdownSession.foreach(_(this))

        _running_? = false

        SessionMaster.sendMsg(RemoveSession(this.uniqueId))

        asyncComponents.foreach {case (_, comp) => done ::= (() => tryo(comp ! ShutDown))}
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

  def contextPath = LiftRules.calculateContextPath() openOr _contextPath

  private def checkDesignerFriendly(in: NodeSeq): NodeSeq = {
    def df(in: MetaData): Boolean = in match {
      case Null => false
      case p: PrefixedAttribute 
      if (p.pre == "l" || p.pre == "lift") && p.key == "df" => true
      case n => df(n.next)
    }


    val check = in.find {
      case e: Elem => e.label == "html" && df(e.attributes)
      case _ => false
    }.isDefined

    def findSurroundSnippet(in: NodeSeq): Option[NodeSeq] = {
      def isSurAttr(m: MetaData): Boolean = m match {
        case Null => false
        case p: PrefixedAttribute if 
          ((((p.pre == "l" || p.pre == "lift") && p.key == "s") ||
            (p.pre == "l" && p.key == "snippet")) &&
           p.value.text == "surround") => true
        case n => isSurAttr(n.next)
      }


      def isSurround(e: Elem) =
        (("l" == e.prefix || "lift" == e.prefix) &&
         e.label == "surround") && isSurAttr(e.attributes)

      in.flatMap {
        case Group(nodes) => findSurroundSnippet(nodes)
        case e: Elem if isSurround(e) => Some(e)
        case e: Elem => findSurroundSnippet(e.child)
        case _ => None
      }.headOption
    }

    if (check) {
      findSurroundSnippet(in) match {
        case Some(found) => found
        case _ => in
      }
    } else in
  }

  /**
   * Convert a template into a Lift Response.
   *
   * @param template -- the NodeSeq that makes up the page... or the template
   * will be located via findVisibleTemplate
   * @param request -- the Req the led to this rendering
   * @param path -- the ParsePath that led to this page
   * @param code -- the HTTP response code (usually 200)
   *
   * @returns a Box of LiftResponse with all the proper page rewriting
   */
  def processTemplate(template: Box[NodeSeq], request: Req, path: ParsePath, code: Int): Box[LiftResponse] = {
    (template or findVisibleTemplate(path, request)).map { 
      xhtmlBase =>
        val xhtml = checkDesignerFriendly(xhtmlBase)
        // Phase 1: snippets & templates processing
        val rawXml: NodeSeq = processSurroundAndInclude(PageName get, xhtml)
      
      // Make sure that functions have the right owner. It is important for this to
      // happen before the merge phase so that in merge to have a correct view of
      // mapped functions and their owners.
      updateFunctionMap(S.functionMap, RenderVersion get, millis)
      
      // Phase 2: Head & Tail merge, add additional elements to body & head
      val xml = merge(rawXml, request)
      
      notices = Nil
      // Phase 3: Response conversion including fixHtml
      LiftRules.convertResponse((xml, code),
                                S.getHeaders(LiftRules.defaultHeaders((xml, request))),
                                S.responseCookies,
                                request)
    }
  }


  private[http] def processRequest(request: Req): Box[LiftResponse] = {
    ieMode.is // make sure this is primed
    S.oldNotices(notices)
    LiftSession.onBeginServicing.foreach(f => tryo(f(this, request)))
    val ret = try {
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
            notices = S.getNotices
          }

        case _ =>
          RenderVersion.get // touch this early

          runParams(request)

          val early = LiftRules.preAccessControlResponse_!!.firstFull(request)

          // Process but make sure we're okay, sitemap wise
          val response: Box[LiftResponse] = early or (request.testLocation match {
            case Left(true) =>
              cleanUpBeforeRender

              PageName(request.uri + " -> " + request.path)
              LiftRules.allowParallelSnippets.doWith(() => !Props.inGAE) {
               (request.location.flatMap(_.earlyResponse) or LiftRules.earlyResponse.firstFull(request)) or
                 (processTemplate(locTemplate, request, request.path, 200) or
                    request.createNotFound{processTemplate(Empty, request, _, 404)})
              }

            case Right(Full(resp)) => Full(resp)
            case _ if (LiftRules.passNotFoundToChain) => Empty
            case _ if Props.mode == Props.RunModes.Development =>
              request.createNotFound{processTemplate(Empty, request, _, 404)} or
              Full(ForbiddenResponse("The requested page was not defined in your SiteMap, so access was blocked.  (This message is displayed in development mode only)"))
            case _ => request.createNotFound{processTemplate(Empty, request, _, 404)}
          })

          // Before returning the response check for redirect and set the appropriate state.
          response.map(checkRedirect)
      }
    } catch {
      case ite: _root_.java.lang.reflect.InvocationTargetException if (ite.getCause.isInstanceOf[ResponseShortcutException]) =>
        Full(handleRedirect(ite.getCause.asInstanceOf[ResponseShortcutException], request))

      case rd: _root_.net.liftweb.http.ResponseShortcutException => Full(handleRedirect(rd, request))

      case e => NamedPF.applyBox((Props.mode, request, e), LiftRules.exceptionHandler.toList);

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
    if (re.doNotices) notices = S.getNotices

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
              SHtml.appendFuncToURL(uri, func + "=_")
    } openOr uri

  }

  private[http] def checkRedirect(resp: LiftResponse): LiftResponse = resp match {
    case RedirectWithState(uri, state, cookies@_*) =>
      state.msgs.foreach(m => S.message(m._1, m._2))
      notices = S.getNotices
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
    val splits = tpath.toList.filter {a => !a.startsWith("_") && !a.startsWith(".") && a.toLowerCase.indexOf("-hidden") == -1} match {
      case s@_ if (!s.isEmpty) => s
      case _ => List("index")
    }
    findAnyTemplate(splits, S.locale)
  }

  private[liftweb] def findTemplate(name: String): Box[NodeSeq] = {
    val splits = (if (name.startsWith("/")) name else "/" + name).split("/").toList.drop(1) match {
      case Nil => List("index")
      case s => s
    }

    findAnyTemplate("templates-hidden" :: splits, S.locale) match {
      case Full(x) => Full(x)
      case f: Failure if Props.devMode => f
      case _ => findAnyTemplate(splits, S.locale)
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
        case i  => ("." + name.substring(0, i), name.substring(i+1))
      }
      findClass(terminal, LiftRules.buildPackage("snippet").map(_ + packageSuffix) :::
                (("lift.app.snippet" + packageSuffix) :: ("net.liftweb.builtin.snippet" + packageSuffix) :: Nil))
    }
  }

  private def instantiateOrRedirect[T](c: Class[T]): Box[T] = tryo({
    case e: ResponseShortcutException => throw e
    case ite: _root_.java.lang.reflect.InvocationTargetException
      if (ite.getCause.isInstanceOf[ResponseShortcutException]) => throw ite.getCause.asInstanceOf[ResponseShortcutException]
  }, c.newInstance)

  private def findAttributeSnippet(attrValue: String, rest: MetaData, params: AnyRef*): MetaData = {
    S.doSnippet(attrValue) {
      val (cls, method) = splitColonPair(attrValue, null, "render")

      first(LiftRules.snippetNamesToSearch.vend(cls)) { nameToTry =>
        findSnippetClass(nameToTry) flatMap { clz =>
          instantiateOrRedirect(clz) flatMap { inst =>
            invokeMethod(clz, inst, method) or invokeMethod(clz, inst, method, params.toList.toArray) match {
              case Full(md: MetaData) => Full(md.copy(rest))
              case _ => Empty
            }
          }
        }
      } openOr rest
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

    for{
      template <- findAnyTemplate(name, S.locale) ?~ ("Template " + name + " not found")
      res <- findElem(processSurroundAndInclude(name.mkString("/", "/", ""), template))
    } yield res
  }

  private def processAttributes(in: MetaData): MetaData = {
    in match {
      case Null => Null
      case mine: PrefixedAttribute if (mine.pre == "lift") => {
        mine.key match {
          case s if s.indexOf('.') > -1 => findAttributeSnippet(s, processAttributes(in.next), mine)
          case "snippet" => findAttributeSnippet(mine.value.text, processAttributes(in.next))
          case _ => mine.copy(processAttributes(in.next))
        }
      }
      case notMine => notMine.copy(processAttributes(in.next))
    }
  }

  /**
  * See if there's a object singleton with the right name
  */
  private def findSnippetObject(cls: String): Box[AnyRef] =
  findSnippetClass(cls+"$").flatMap {
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
      case fail : Failure => fail
      case _ => Empty
    }

  private def reportSnippetError(page: String,
                                 snippetName: Box[String],
                                 why: LiftRules.SnippetFailures.Value,
                                 addlMsg: NodeSeq,
                                 whole: NodeSeq): NodeSeq =
    {
      for{
        f <- LiftRules.snippetFailedFunc.toList
      }
        f(LiftRules.SnippetFailure(page, snippetName, why))

      Props.mode match {
        case Props.RunModes.Development =>
          <div style="display: block; margin: 8px; border: 2px solid red">Error processing snippet {snippetName openOr "N/A"}. Reason: {why} {addlMsg} XML causing this error:<br/>
          <pre>{whole.toString}</pre>
          <i>note: this error is displayed in the browser because
          your application is running in "development" mode.If you
          set the system property run.mode=production, this error will not
          be displayed, but there will be errors in the output logs.
          </i>
          </div>
        case _ => NodeSeq.Empty
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
      RenderVersion.set(renderVersion)
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

    val eagerEval: Boolean = (attrs.get("eager_eval").map(toBoolean) or
            findNSAttr(attrs, "lift", "eager_eval").map(toBoolean)
            ) getOrElse false

    val kids = if (eagerEval) processSurroundAndInclude(page, passedKids) else passedKids

    // Locate a snippet as defined by our SiteMap Loc
    def locSnippet(snippet: String): Box[NodeSeq] =
      for (loc <- S.location;
           func <- loc.snippet(snippet)) yield func(kids)

    def locateAndCacheSnippet(tagName: String): Box[AnyRef] =
      snippetMap.is.get(tagName) or {
        first(LiftRules.snippetNamesToSearch.vend(tagName)) { nameToTry =>
          val ret = findSnippetInstance(nameToTry)
          // Update the snippetMap so that we reuse the same instance in this request
          ret.foreach(s => snippetMap.set(snippetMap.is.update(tagName, s)))
          ret
        }
      }

    val ret: NodeSeq = 
      try {
      snippetName.map(snippet =>
            S.doSnippet(snippet)(
              (S.locateMappedSnippet(snippet).map(_(kids)) or
                      locSnippet(snippet)).openOr(
                S.locateSnippet(snippet).map(_(kids)) openOr {
                  val (cls, method) = splitColonPair(snippet, null, "render")
                  (locateAndCacheSnippet(cls)) match {
                    case Full(inst: StatefulSnippet) if !stateful_? =>
                      reportSnippetError(page, snippetName,
                        LiftRules.SnippetFailures.StateInStateless,
                        NodeSeq.Empty,
                        wholeTag)

                    case Full(inst: StatefulSnippet) =>
                      if (inst.dispatch.isDefinedAt(method)) {
                        val res = inst.dispatch(method)(kids)

                        (if (isForm && !res.isEmpty) <div>{SHtml.hidden(() => inst.registerThisSnippet)}</div> else NodeSeq.Empty) ++
                                res
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
                      val ar: Array[AnyRef] = List(Group(kids)).toArray
                      ((Helpers.invokeMethod(inst.getClass, inst, method, ar)) or
                              Helpers.invokeMethod(inst.getClass, inst, method)) match {
                        case CheckNodeSeq(md) => md
                        case it =>
                          val intersection = if (Props.devMode) {
                            val methodNames = inst.getClass.getMethods().map(_.getName).toList.removeDuplicates
                            val methodAlts = List(method, Helpers.camelCase(method),
                              Helpers.camelCaseMethod(method))
                            methodNames intersect methodAlts
                          } else Nil

                          reportSnippetError(page, snippetName,
                            LiftRules.SnippetFailures.MethodNotFound,
                            if (intersection.isEmpty) NodeSeq.Empty else
                              <div>There are possible matching methods ({intersection}),
                              but none has the required signature:<pre>def{method}(in: NodeSeq): NodeSeq</pre> </div>,
                            wholeTag)
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
                }))).openOr {
      reportSnippetError(page, snippetName,
        LiftRules.SnippetFailures.NoNameSpecified,
        NodeSeq.Empty,
        wholeTag)
                      }
      } catch {
        case e: StateInStatelessException =>
          reportSnippetError(page, snippetName,
                             LiftRules.SnippetFailures.StateInStateless,
                             NodeSeq.Empty,
                             wholeTag)
      }

    def checkMultiPart(in: MetaData): MetaData = in.filter(_.key == "multipart").toList match {
      case Nil => Null
      case x => new UnprefixedAttribute("enctype", Text("multipart/form-data"), Null)
    }

    def checkAttr(attr_name: String, in: MetaData, base: MetaData): MetaData =
      in.filter(_.key == attr_name).toList match {
        case Nil => base
        case x => new UnprefixedAttribute(attr_name, Text(x.first.value.text),
          base)
      }

    if (ret.isEmpty) ret else
      attrs.get("form").map(ft => (
              (<form action={S.uri} method={ft.text.trim.toLowerCase}>{ret}</form> %
                      checkMultiPart(attrs)) % LiftRules.formAttrs.vend.foldLeft[MetaData](Null)((base, name) => checkAttr(name, attrs, base))
              )) getOrElse ret

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

    if (isLazy && LiftRules.allowParallelSnippets()) {
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
          val bns = tryo {reqVarCallback(() => f)}
          
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

  /**
   * Processes the surround tag and other lift tags
   */
  def processSurroundAndInclude(page: String, in: NodeSeq): NodeSeq =
    in.flatMap {
      v =>
              v match {
                case Group(nodes) =>
                  Group(processSurroundAndInclude(page, nodes))
                
                case SnippetNode(element, kids, isLazy, attrs, snippetName) =>

                  // case elm: Elem if elm.prefix == "lift" || elm.prefix == "l" =>
                  processOrDefer(isLazy) {
                    S.doSnippet(snippetName) {
                      S.withAttrs(attrs) {
                        S.setVars(attrs) {
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

                case elm: Elem =>
                  Elem(v.prefix, v.label, processAttributes(v.attributes),
                    v.scope, processSurroundAndInclude(page, v.child): _*)
                case _ => v
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
   * Allows you to send messages to a CometActor that may or may not be set up yet
   */
  def setupComet(theType: String, name: Box[String], msg: Any) {
    testStatefulFeature {
      cometSetup((Full(theType) -> name, msg) :: cometSetup.is)
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

    for{
      actor <- ret
      (cst, csv) <- cometSetup.is if cst == what
    } actor ! csv

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
     act ! PerformSetupComet
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
    val id = Full(act.uniqueId)
    messageCallback.keys.toList.foreach {
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

    LiftRules.cometCreationFactory.vend.apply(createInfo).map{
    a => a ! PerformSetupComet; a} or
    LiftRules.cometCreation.toList.find(_.isDefinedAt(createInfo)).map(_.apply(createInfo)).map{
    a => a ! PerformSetupComet; a} or
    (findType[LiftCometActor](contType, LiftRules.buildPackage("comet") ::: ("lift.app.comet" :: Nil)).flatMap {
      cls =>
              tryo((e: Throwable) => e match {
                case e: _root_.java.lang.NoSuchMethodException => ()
                case e => logger.info("Comet find by type Failed to instantiate " + cls.getName, e)
              }) {
                val constr = cls.getConstructor()
                val ret = constr.newInstance().asInstanceOf[LiftCometActor]
                ret.callInitCometActor(this, Full(contType), name, defaultXml, attributes)

                ret ! PerformSetupComet
                ret.asInstanceOf[LiftCometActor]
              } or tryo((e: Throwable) => logger.info("Comet find by type Failed to instantiate " + cls.getName, e)) {
                val constr = cls.getConstructor(this.getClass, classOf[Box[String]], classOf[NodeSeq], classOf[Map[String, String]])
                val ret = constr.newInstance(this, name, defaultXml, attributes).asInstanceOf[LiftCometActor];

                ret ! PerformSetupComet
                ret.asInstanceOf[LiftCometActor]
              }
    })
    }
  }

  private def failedFind(in: Failure): NodeSeq =
    <html xmlns:lift="http://liftweb.net" xmlns="http://www.w3.org/1999/xhtml"> <head/>
    <body> 
  <div style="border: 1px red solid">Error locating template.Message: {in.msg} <br/> {in.exception.map(e => <pre>{e.toString}{e.getStackTrace.map(_.toString).mkString("\n")}</pre>).openOr(NodeSeq.Empty)} This message is displayed because you are in Development mode.
    </div> </body> </html>

  private[liftweb] def findAndMerge(templateName: Box[Seq[Node]], atWhat: Map[String, NodeSeq]): NodeSeq = {
    val name = templateName.map(s => if (s.text.startsWith("/")) s.text else "/" + s.text).openOr("/templates-hidden/default")

    findTemplate(name) match {
      case f@Failure(msg, be, _) if Props.devMode =>
        failedFind(f)
      case Full(s) => bind(atWhat, s)
      case _ => atWhat.values.flatMap(_.toSeq).toList
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

/**
 * Contains functions for obtaining templates
 */
object TemplateFinder {
  private val suffixes = List("html", "xhtml", "htm")

  import LiftRules.ViewDispatchPF

  private def checkForLiftView(part: List[String], last: String, what: ViewDispatchPF): Box[NodeSeq] = {
    if (what.isDefinedAt(part)) {
      what(part) match {
        case Right(lv) => if (lv.dispatch.isDefinedAt(last)) lv.dispatch(last)() else Empty
        case _ => Empty
      }
    } else Empty
  }

  private def checkForFunc(whole: List[String], what: ViewDispatchPF): Box[NodeSeq] =
    if (what.isDefinedAt(whole)) what(whole) match {
      case Left(func) => func()
      case _ => Empty
    }
    else Empty

  private def findInViews(whole: List[String], part: List[String],
                          last: String,
                          what: List[ViewDispatchPF]): Box[NodeSeq] =
    what match {
      case Nil => Empty
      case x :: xs =>
        (checkForLiftView(part, last, x) or checkForFunc(whole, x)) match {
          case Full(ret) => Full(ret)
          case _ => findInViews(whole, part, last, xs)
        }
    }

  /**
   * Given a list of paths (e.g. List("foo", "index")),
   * find the template.
   * @param places - the path to look in
   *
   * @return the template if it can be found
   */
  def findAnyTemplate(places: List[String]): Box[NodeSeq] =
    findAnyTemplate(places, S.locale)

  /**
   * Given a list of paths (e.g. List("foo", "index")),
   * find the template.
   * @param places - the path to look in
   * @param locale - the locale of the template to search for
   *
   * @return the template if it can be found
   */
  def findAnyTemplate(places: List[String], locale: Locale): Box[NodeSeq] = {
    /*
     From a Scala coding standpoint, this method is ugly.  It's also a performance
     hotspot that needed some tuning.  I've made the code very imperative and
     tried to make sure there are no anonymous functions created in this method.
     The few extra lines of code and the marginal reduction in readibility should
     yield better performance.  Please don't change this method without chatting with
     me first.  Thanks!  DPP
     */
    val lrCache = LiftRules.templateCache
    val cache = if (lrCache.isDefined) lrCache.open_! else NoCache

    val key = (locale, places)
    val tr = cache.get(key)

    if (tr.isDefined) tr
    else
      {
        val part = places.dropRight(1)
        val last = places.last

        findInViews(places, part, last, LiftRules.viewDispatch.toList) match {
          case Full(lv) =>
            Full(lv)

          case _ =>
            val pls = places.mkString("/", "/", "")

            val se = suffixes.elements
            val sl = List("_" + locale.toString, "_" + locale.getLanguage, "")

            var found = false
            var ret: NodeSeq = null

            while (!found && se.hasNext) {
              val s = se.next
              val le = sl.elements
              while (!found && le.hasNext) {
                val p = le.next
                val name = pls + p + (if (s.length > 0) "." + s else "")
                import scala.xml.dtd.ValidationException
                val xmlb = try {
                  LiftRules.doWithResource(name) { PCDataXmlParser(_) } match {
                    case Full(seq) => seq
                    case _ => Empty
                  }
                } catch {
                  case e: ValidationException if Props.devMode =>
                    return (Full(
                      <div style="border: 1px red solid">Error locating template {name}.<br/>
                      Message:{e.getMessage}<br/>
                      {
                      <pre>{e.toString}{e.getStackTrace.map(_.toString).mkString("\n")}</pre>
                      }
                      This message is displayed because you are in Development mode.
                    </div>))

                  case e: ValidationException => Empty
                }
                if (xmlb.isDefined) {
                  found = true
                  ret = (cache(key) = xmlb.open_!)
                } else if (xmlb.isInstanceOf[Failure] && Props.devMode) {
                  val msg = xmlb.asInstanceOf[Failure].msg
                  val e = xmlb.asInstanceOf[Failure].exception
                  return (Full(<div style="border: 1px red solid">Error locating template{name}.<br/>Message:{msg}<br/>{
                  {
                    e match {
                      case Full(e) =>
                        <pre>{e.toString}{e.getStackTrace.map(_.toString).mkString("\n")}</pre>
                      case _ => NodeSeq.Empty
                    }
                  }}This message is displayed because you are in Development mode.
                  </div>))
                }
              }
            }

            if (found) Full(ret)
            else lookForClasses(places)
        }
      }
  }

  private def lookForClasses(places: List[String]): Box[NodeSeq] = {
    val (controller, action) = places match {
      case ctl :: act :: _ => (ctl, act)
      case ctl :: _ => (ctl, "index")
      case Nil => ("default_template", "index")
    }
    val trans = List[String => String](n => n, n => camelCase(n))
    val toTry = trans.flatMap(f => (LiftRules.buildPackage("view") ::: ("lift.app.view" :: Nil)).map(_ + "." + f(controller)))

    first(toTry) {
      clsName =>
              try {
                tryo(List(classOf[ClassNotFoundException]), Empty)(Class.forName(clsName).asInstanceOf[Class[AnyRef]]).flatMap {
                  c =>
                          (c.newInstance match {
                            case inst: InsecureLiftView => c.getMethod(action).invoke(inst)
                            case inst: LiftView if inst.dispatch.isDefinedAt(action) => inst.dispatch(action)()
                            case _ => Empty
                          }) match {
                            case null | Empty | None => Empty
                            case n: Group => Full(n)
                            case n: Elem => Full(n)
                            case s: NodeSeq => Full(s)
                            case Some(n: Group) => Full(n)
                            case Some(n: Elem) => Full(n)
                            case Some(n: NodeSeq) => Full(n)
                            case Some(SafeNodeSeq(n)) => Full(n)
                            case Full(n: Group) => Full(n)
                            case Full(n: Elem) => Full(n)
                            case Full(n: NodeSeq) => Full(n)
                            case Full(SafeNodeSeq(n)) => Full(n)
                            case _ => Empty
                          }
                }
              } catch {
                case ite: _root_.java.lang.reflect.InvocationTargetException /* if (ite.getCause.isInstanceOf[ResponseShortcutException]) */ => throw ite.getCause
                case re: ResponseShortcutException => throw re
                case _ => Empty
              }
    }
  }
}

/**
* A case class that contains the information necessary to set up a CometActor
*/
final case class CometCreationInfo(contType: String,
                                   name: Box[String],
                                   defaultXml: NodeSeq,
                                   attributes: Map[String, String],
                                   session: LiftSession)

class StateInStatelessException(msg: String) extends Exception(msg)



  // an object that extracts an elem that defines a snippet
  private object SnippetNode {
    
    private def snippy(in: MetaData): Option[String] =
      in match {
        case Null => None
        case p: PrefixedAttribute if 
          ((p.pre == "l" || p.pre == "lift") && p.key == "s") ||
        (p.pre == "l" && p.key == "snippet") => Some(p.value.text)
        case x => snippy(x.next)
      }

    private def liftAttrsAndParallel(in: MetaData): (Boolean, MetaData, MetaData) = {
      var next = in
      var par = false
      var nonLift: MetaData = Null
      var lift: MetaData = Null
   

      while (next != Null) {
        next match {
          case p: PrefixedAttribute 
          if (p.pre == "l" || p.pre == "lift") && p.key == "parallel"
          => par = true


          case p: PrefixedAttribute 
          if p.pre == "lift" && p.key == "snippet"
          => nonLift = p.copy(nonLift)

          case p: PrefixedAttribute 
          if (p.pre == "l" || p.pre == "lift") 
            => lift = new UnprefixedAttribute(p.key, p.value, lift)

          case a => nonLift = a.copy(nonLift)
        }
        next = next.next
      }
      
      
      (par, nonLift, lift)
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
            snippetName <- snippy(elm.attributes)
          } yield {
            val (par, nonLift, lift) = liftAttrsAndParallel(elm.attributes)
            val newElm = new Elem(elm.prefix, elm.label, 
                                  nonLift, elm.scope, elm.child :_*)
            (newElm, newElm, par, lift, snippetName)
             
          }
        }

        case _ => {
          None
        }
      }
  }


}
}
