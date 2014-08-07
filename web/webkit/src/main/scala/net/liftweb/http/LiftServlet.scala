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

import xml.{Node, NodeSeq, Group}

import common._
import actor._
import util._
import util.Helpers._
import js._
import auth._
import provider._
import json.JsonAST.JValue

/**
 * Wrap a LiftResponse and cache the result to avoid computing the actual response
 * more than once
 */
private [http] case class CachedResponse(wrapped: LiftResponse) extends LiftResponse {
  private val _cachedResponse = wrapped.toResponse

  def toResponse = _cachedResponse

  // Should we retry request processing
  def failed_? = _cachedResponse.code >= 500 && _cachedResponse.code < 600
}

class LiftServlet extends Loggable {
  private var servletContext: HTTPContext = null

  def this(ctx: HTTPContext) = {
    this ()
    this.servletContext = ctx
  }

  def getContext: HTTPContext = servletContext

  def destroy = {
    try {
      LiftRules.ending = true

      tryo {
        SessionMaster.shutDownAllSessions()
      }

      val cur = millis

      // wait 10 seconds or until the request count is zero
      while (LiftRules.reqCnt.get > 0 && (millis - cur) < 10000L) {
        Thread.sleep(20)
      }

      tryo {
        Schedule.shutdown
      }
      tryo {
        LAScheduler.shutdown()
      }

      tryo {
        LAPinger.shutdown
      }

      LiftRules.runUnloadHooks()
      logger.debug("Destroyed Lift handler.")
      // super.destroy
    } catch {
      case e: Exception => logger.error("Destruction failure", e)
    }
  }

  def init = {
    LiftRules.ending = false
  }

  def getLiftSession(request: Req): LiftSession = LiftRules.getLiftSession(request)

  private def wrapState[T](req: Req, session: Box[LiftSession])(f: => T): T = {
    session match {
      case Full(ses) => S.init(req, ses)(f)
      case _ => CurrentReq.doWith(req)(f)
    }
  }

  private def handleGenericContinuation(reqOrg: Req, resp: HTTPResponse, session: Box[LiftSession], func: ((=> LiftResponse) => Unit) => Unit): Boolean = {

    val req = if (null eq reqOrg) reqOrg else reqOrg.snapshot

    def runFunction(doAnswer: LiftResponse => Unit) {
      Schedule.schedule(() => {
        val answerFunc: (=> LiftResponse) => Unit = response =>
          doAnswer(wrapState(req, session)(response))

        func(answerFunc)

      }, 5.millis)
    }

    if (reqOrg.request.suspendResumeSupport_?) {
      runFunction(liftResponse => {
        // do the actual write on a separate thread
        Schedule.schedule(() => {
          reqOrg.request.resume(reqOrg, liftResponse)
        }, 0.seconds)
      })

      reqOrg.request.suspend(cometTimeout)
      false
    } else {
      val future = new LAFuture[LiftResponse]

      runFunction(answer => future.satisfy(answer))

      future.get(cometTimeout) match {
        case Full(answer) => sendResponse(answer, resp, req); true
        case _ => false
      }
    }
  }

  /**
   * Processes the HTTP requests
   */
  def service(req: Req, resp: HTTPResponse): Boolean = {
    try {
      def doIt: Boolean = {
        if (LiftRules.logServiceRequestTiming) {
          logTime {
            val ret = doService(req, resp)
            val msg = "Service request (" + req.request.method + ") " + req.request.uri + " returned " + resp.getStatus + ","
            (msg, ret)
          }
        } else {
          doService(req, resp)
        }
      }

      req.request.resumeInfo match {
        case None => doIt
        case r if r eq null => doIt
        case Some((or: Req, r: LiftResponse)) if (req.path == or.path) => sendResponse(r, resp, req); true
        case _ => doIt
      }
    } catch {
      case rest.ContinuationException(theReq, sesBox, func) =>
        handleGenericContinuation(theReq, resp, sesBox, func); true // we have to return true to hold onto the request

      case e if e.getClass.getName.endsWith("RetryRequest") => throw e
      case e: Throwable => logger.info("Request for " + req.request.uri + " failed " + e.getMessage, e); throw e
    }
  }

  private def flatten(in: List[Any]): List[Any] = in match {
    case Nil => Nil
    case Some(x: AnyRef) :: xs => x :: flatten(xs)
    case Full(x: AnyRef) :: xs => x :: flatten(xs)
    case (lst: Iterable[_]) :: xs => lst.toList ::: flatten(xs)
    case (x: AnyRef) :: xs => x :: flatten(xs)
    case x :: xs => flatten(xs)
  }

  private def authPassed_?(req: Req): Boolean = {

    val checkRoles: (Role, List[Role]) => Boolean = {
      case (resRole, roles) => (false /: roles)((l, r) => l || resRole.isChildOf(r.name))
    }

    val role = NamedPF.applyBox(req, LiftRules.httpAuthProtectedResource.toList)
    role.map(_ match {
      case Full(r) =>
        LiftRules.authentication.verified_?(req) match {
          case true => checkRoles(r, userRoles.get)
          case _ => false
        }
      case _ => LiftRules.authentication.verified_?(req)
    }) openOr true
  }

  private val recent: LRUMap[String, Int] = new LRUMap(2000)

  private def registerRecentlyChecked(id: String): Unit =
    synchronized {
      val next = recent.get(id) match {
        case Full(x) => x + 1
        case _ => 1
      }

      recent(id) = next
    }

  private def recentlyChecked(id: Box[String]): Int = synchronized {
    id.flatMap(recent.get).openOr(0)
  }

  trait ProcessingStep {
    def process(req: Req): Box[LiftResponse]

    def processFunc: (Req) => Box[LiftResponse] = process _
  }

  /** To save memory these are only created once and should just be holders for functions **/

  object ShuttingDown extends ProcessingStep {

    def notFoundOrIgnore(req: Req, session: Box[LiftSession]): Box[LiftResponse] = {
      if (LiftRules.passNotFoundToChain) {
        net.liftweb.common.Failure("Not found")
      } else {
        Full(
          session.map(_.checkRedirect(req.createNotFound))
            .getOrElse(req.createNotFound)
        )
      }
    }

    def process(req: Req) = {
      if(LiftRules.ending)
        notFoundOrIgnore(req,Empty)
      else
        Empty
    }

  }

  object CheckAuth extends ProcessingStep {

    def authPassed_?(req: Req): Boolean = {

      val checkRoles: (Role, List[Role]) => Boolean = {
        case (resRole, roles) => (false /: roles)((l, r) => l || resRole.isChildOf(r.name))
      }

      val role = NamedPF.applyBox(req, LiftRules.httpAuthProtectedResource.toList)
      role.map(_ match {
        case Full(r) =>
          LiftRules.authentication.verified_?(req) match {
            case true => checkRoles(r, userRoles.get)
            case _ => false
          }
        case _ => LiftRules.authentication.verified_?(req)
      }) openOr true
    }

    def process(req: Req) =
      if(!authPassed_?(req))
        Full(LiftRules.authentication.unauthorizedResponse)
      else
        Empty

  }

  object SessionLossCheck extends ProcessingStep {

    def process(req: Req): Box[LiftResponse] = {
      val (isComet, isAjax) = cometOrAjax_?(req)
      val sessionIdCalc = new SessionIdCalc(req)

      if (LiftRules.redirectAsyncOnSessionLoss && !sessionExists_?(sessionIdCalc.id) && (isComet || isAjax)) {
        val theId = sessionIdCalc.id

        // okay after 2 attempts to redirect, just ignore calls to the
        // async URL
        if (recentlyChecked(theId) > 1) {
          net.liftweb.common.Failure("Too many attempts")
        } else {
          val cmd =
            if (isComet)
              js.JE.JsRaw(LiftRules.noCometSessionCmd.vend.toJsCmd + ";lift.setToWatch({});").cmd
            else
              js.JE.JsRaw(LiftRules.noAjaxSessionCmd.vend.toJsCmd).cmd

          Full(new JsCommands(cmd :: Nil).toResponse)
        }
      } else {
        Empty
      }
    }

    def reqHasSession(req: Req): Boolean = {
      val sessionIdCalc = new SessionIdCalc(req)
      !sessionExists_?(sessionIdCalc.id)
    }

    def sessionExists_?(idb: Box[String]): Boolean = {
      idb.flatMap {
        id =>
          registerRecentlyChecked(id)
          SessionMaster.getSession(id, Empty)
      }.isDefined
    }

    def cometOrAjax_?(req: Req): (Boolean, Boolean) = {
      lazy val ajaxPath = LiftRules.liftContextRelativePath :: "ajax" :: Nil
      lazy val cometPath = LiftRules.liftContextRelativePath :: "comet" :: Nil

      val wp = req.path.wholePath
      val pathLen = wp.length

      def isComet: Boolean = {
        if (pathLen < 3) {
          false
        } else {
          val kindaComet = wp.take(2) == cometPath

          kindaComet && req.acceptsJavaScript_?
        }
      }
      def isAjax: Boolean = {
        if (pathLen < 3) {
          false
        } else {
          val kindaAjax = wp.take(2) == ajaxPath

          kindaAjax && req.acceptsJavaScript_?
        }
      }
      (isComet, isAjax)
    }

  }

  object StatelessResponse extends ProcessingStep {

    def process(req: Req): Box[LiftResponse] = {
      var tmpStatelessHolder: Box[Box[LiftResponse]] = Empty

      if(S.statelessInit(req) {
        // if the request is matched is defined in the stateless table, dispatch
        tmpStatelessHolder = NamedPF.applyBox(req,
          LiftRules.statelessDispatch.toList).map(_.apply() match {
          case Full(a) => Full(LiftRules.convertResponse((a, Nil, S.responseCookies, req)))
          case r => r
        })
        tmpStatelessHolder.isDefined
      }) {
        val f = tmpStatelessHolder.openOrThrowException("This is a full box here, checked on previous line")
        f match {
          case Full(v) => Full(v)
          case Empty => LiftRules.notFoundOrIgnore(req, Empty)
          case f: net.liftweb.common.Failure => Full(req.createNotFound(f))
        }
      } else {
        Empty
      }
    }
  }

  object StatefulResponse extends ProcessingStep {

    def process(req: Req) = {
      // otherwise do a stateful response
      val liftSession = getLiftSession(req)

      def doSession(r2: Req, s2: LiftSession, continue: Box[() => Nothing]): () => Box[LiftResponse] = {
        try {
          S.init(r2, s2) {
            dispatchStatefulRequest(S.request.openOrThrowException("I'm pretty sure this is a full box here"), liftSession, r2, continue)
          }
        } catch {
          case cre: ContinueResponseException =>
            r2.destroyServletSession()
            doSession(r2, getLiftSession(r2), Full(cre.continue))
        }
      }

      val lzy: () => Box[LiftResponse] = doSession(req, liftSession, Empty)

      lzy()
    }
  }

  /**
   * This is the processing pipeline for all lift requests.
    * Basically each of these takes a Req and returns either a
    * Full(Response) - in which case  return
    * Empty - Go to the next handler
    * Failure - short circuit and return
    *
    */
  val processingPipeline: Seq[ProcessingStep] =
    Seq(
      ShuttingDown,
      CheckAuth,
      SessionLossCheck,
      StatelessResponse,
      StatefulResponse
    )

  /**
   * Service the HTTP request
   */
  def doService(req: Req, response: HTTPResponse): Boolean = {

    tryo {
      LiftRules.onBeginServicing.toList.foreach(_(req))
    }

    def stepThroughPipeline(steps: Seq[ProcessingStep]): Box[LiftResponse] = {
      //Seems broken but last step always hits
      steps.head.process(req) match {
        case Empty => stepThroughPipeline(steps.tail)
        case a@_   => a
      }
    }

    /* Go through the pipeline and send response if full **/
    val resp: Box[LiftResponse] = try {
      stepThroughPipeline(processingPipeline)
    } catch {
      case foc: LiftFlowOfControlException => throw foc
      case e: Exception if !e.getClass.getName.endsWith("RetryRequest") => S.runExceptionHandlers(req, e)
    }

    tryo {
      LiftRules.onEndServicing.toList.foreach(_(req, resp))
    }

    resp match {
      case Full(EmptyResponse) =>
        true

      case Full(cresp) =>
        sendResponse(cresp, response, req)
        true

      case _ => {
        false
      }
    }
  }

  private def dispatchStatefulRequest(req: Req,
                                      liftSession: LiftSession,
                                      originalRequest: Req,
                                      continuation: Box[() => Nothing]): () => Box[LiftResponse] = {
    val toMatch = req

    val dispatch: (Boolean, Box[LiftResponse]) =
      NamedPF.find(toMatch, LiftRules.dispatchTable(req.request)) match {
        case Full(pf) =>
          LiftSession.onBeginServicing.foreach(_(liftSession, req))
          val ret: (Boolean, Box[LiftResponse]) =
            try {
              try {
                // run the continuation in the new session
                // if there is a continuation
                continuation match {
                  case Full(func) => {
                    func()
                    S.redirectTo("/")
                  }
                  case _ => // do nothing
                }

                liftSession.runParams(req)
                S.functionLifespan(true) {
                  pf(toMatch)() match {
                    case Full(v) =>
                      (true, Full(LiftRules.convertResponse((liftSession.checkRedirect(v), Nil,
                        S.responseCookies, req))))

                    case Empty =>
                      (true, LiftRules.notFoundOrIgnore(req, Full(liftSession)))

                    case f: net.liftweb.common.Failure =>
                      (true, net.liftweb.common.Full(liftSession.checkRedirect(req.createNotFound(f))))
                  }
                }
              } catch {
                case ite: java.lang.reflect.InvocationTargetException if (ite.getCause.isInstanceOf[ResponseShortcutException]) =>
                  (true, Full(liftSession.handleRedirect(ite.getCause.asInstanceOf[ResponseShortcutException], req)))

                case rd: net.liftweb.http.ResponseShortcutException => (true, Full(liftSession.handleRedirect(rd, req)))
              }
            } finally {
              if (S.functionMap.size > 0) {
                liftSession.updateFunctionMap(S.functionMap, S.renderVersion, millis)
                S.clearFunctionMap
              }
              liftSession.notices = S.getNotices
            }

          LiftSession.onEndServicing.foreach(_(liftSession, req,
            ret._2))
          ret

        case _ => (false, Empty)
      }

    val wp = req.path.wholePath

    if (LiftRules.enableContainerSessions && !req.stateless_?) {
      req.request.session
    }

    def respToFunc(in: Box[LiftResponse]): () => Box[LiftResponse] = {
      val ret = in.map(LiftRules.performTransform)
      () => ret
    }

    // FIXME Make comet and ajax into pipelining steps.
    val (comet_?, ajax_?) = SessionLossCheck.cometOrAjax_?(req)

    val toReturn: () => Box[LiftResponse] =
      if (dispatch._1) {
        respToFunc(dispatch._2)
      } else if (comet_?) {
        handleComet(req, liftSession, originalRequest) match {
          case Left(x) => respToFunc(x)
          case Right(x) => x
        }
      } else if (ajax_?) {
        respToFunc(handleAjax(liftSession, req))
      } else {
        respToFunc(liftSession.processRequest(req, continuation))
      }

    toReturn
  }

  /**
   * Tracks the two aspects of an AJAX version: the sequence number,
   * whose sole purpose is to identify requests that are retries for the
   * same resource, and pending requests, which indicates how many
   * requests are still queued for this particular page version on the
   * client. The latter is used to expire result data for sequence
   * numbers that are no longer needed.
   */
  private case class AjaxVersionInfo(renderVersion:String, sequenceNumber:Long, pendingRequests:Int)
  private object AjaxVersions {
    def unapply(ajaxPathPart: String) : Option[AjaxVersionInfo] = {
      val separator = ajaxPathPart.indexOf("-")
      if (separator > -1 && ajaxPathPart.length > separator + 2)
        Some(
          AjaxVersionInfo(ajaxPathPart.substring(0, separator),
            java.lang.Long.parseLong(ajaxPathPart.substring(separator + 1, ajaxPathPart.length - 1), 36),
            Integer.parseInt(ajaxPathPart.substring(ajaxPathPart.length - 1), 36))
        )
      else
        None
    }
  }
  /**
   * Extracts two versions from a given AJAX path:
   *  - The RenderVersion, which is used for GC purposes.
   *  - The requestVersions, which let us determine if this is
   *    a request we've already dealt with or are currently dealing
   *    with (so we don't rerun the associated handler). See
   *    handleVersionedAjax for more.
   *
   * The requestVersion is passed to the function that is passed in.
   */
  private def extractVersions[T](path: List[String])(f: (Box[AjaxVersionInfo]) => T): T = {
    val LiftPath = LiftRules.liftContextRelativePath
    path match {
      case LiftPath :: "ajax" :: AjaxVersions(versionInfo @ AjaxVersionInfo(renderVersion, _, _)) :: _ =>
        RenderVersion.doWith(renderVersion)(f(Full(versionInfo)))
      case LiftPath :: "ajax" :: renderVersion :: _ =>
        RenderVersion.doWith(renderVersion)(f(Empty))
      case _ => f(Empty)
    }
  }

  /**
   * Generates the JsCmd needed to initialize comets in
   * `S.requestCometVersions` on the client.
   */
  private def commandForComets: JsCmd = {
    val cometVersions = S.requestCometVersions.is

    if (cometVersions.nonEmpty) {
      js.JE.Call(
        "lift.registerComets",
        js.JE.JsObj(
          S.requestCometVersions.is.toList.map {
            case CometVersionPair(guid, version) =>
              (guid, js.JE.Num(version))
          }: _*
        ),
        true
      ).cmd
    } else {
      js.JsCmds.Noop
    }
  }

  /**
   * Runs the actual AJAX processing. This includes handling __lift__GC,
   * or running the parameters in the session. It returns once the AJAX
   * request has completed with a response meant for the user. In cases
   * where the request is taking to respond, an LAFuture may be wrapped
   * around the execution; see `handleAjax` for more.
   */
  private def runAjax(liftSession: LiftSession,
                      requestState: Req): Box[LiftResponse] = {
    try {
      requestState.param("__lift__GC") match {
        case Full(_) =>
          liftSession.updateFuncByOwner(RenderVersion.get, millis)
          Full(JavaScriptResponse(js.JsCmds.Noop))

        case _ =>
          try {
            val what = flatten(try {
              liftSession.runParams(requestState)
            } catch {
              case ResponseShortcutException(_, Full(to), _) =>
                import net.liftweb.http.js.JsCmds._
                List(RedirectTo(to))
            })

            val what2 = what.flatMap {
              case js: JsCmd => List(js)
              case jv: JValue => List(jv)
              case n: NodeSeq => List(n)
              case js: JsCommands => List(js)
              case r: LiftResponse => List(r)
              case s => Nil
            }

            val ret: LiftResponse = what2 match {
              case (json: JsObj) :: Nil => JsonResponse(json)
              case (jv: JValue) :: Nil => JsonResponse(jv)
              case (js: JsCmd) :: xs => {
                (JsCommands(S.noticesToJsCmd :: Nil) &
                  (js :: (xs.collect {
                    case js: JsCmd => js
                  }).reverse) &
                  S.jsToAppend &
                  commandForComets
                ).toResponse
              }

              case (n: Node) :: _ => XmlResponse(n)
              case (ns: NodeSeq) :: _ => XmlResponse(Group(ns))
              case (r: LiftResponse) :: _ => r
              case _ => JsCommands(S.noticesToJsCmd :: JsCmds.Noop :: S.jsToAppend).toResponse
            }

            LiftRules.cometLogger.debug("AJAX Response: " + liftSession.uniqueId + " " + ret)

            Full(ret)
          } finally {
            if (S.functionMap.size > 0) {
              liftSession.updateFunctionMap(S.functionMap, RenderVersion.get, millis)
              S.clearFunctionMap
            }
          }
      }
    } catch {
      case foc: LiftFlowOfControlException => throw foc
      case e: Exception => S.runExceptionHandlers(requestState, e)
    }
  }

  // Retry requests will stop trying to wait for the original request to
  // complete 500ms after the client's timeout. This is because, while
  // we want the original thread to complete so that it can provide an
  // answer for future retries, we don't want retries tying up resources
  // when the client won't receive the response anyway.
  private lazy val ajaxPostTimeout: Long = LiftRules.ajaxPostTimeout * 1000L + 500L
  /**
   * Kick off AJAX handling. Extracts relevant versions and handles the
   * begin/end servicing requests. Then checks whether to wait on an
   * existing request for this same version to complete or whether to
   * do the actual processing.
   */
  private def handleAjax(liftSession: LiftSession,
                         requestState: Req): Box[LiftResponse] = {
    extractVersions(requestState.path.partPath) { versionInfo =>
      LiftRules.cometLogger.debug("AJAX Request: " + liftSession.uniqueId + " " + requestState.params)
      tryo {
        LiftSession.onBeginServicing.foreach(_(liftSession, requestState))
      }

      // Here, a Left[LAFuture] indicates a future that needs to be
      // *satisfied*, meaning we will run the request processing.
      // A Right[LAFuture] indicates a future we need to *wait* on,
      // meaning we will return the result of whatever satisfies the
      // future.
      val nextAction:Either[LAFuture[Box[LiftResponse]], LAFuture[Box[LiftResponse]]] =
        versionInfo match {
          case Full(AjaxVersionInfo(_, handlerVersion, pendingRequests)) =>
            val renderVersion = RenderVersion.get

            liftSession.withAjaxRequests { currentAjaxRequests =>
              // Create a new future, put it in the request list, and return
              // the associated info with the future that needs to be
              // satisfied by the current request handler.
              def newRequestInfo = {
                val info = AjaxRequestInfo(handlerVersion, new LAFuture[Box[LiftResponse]], millis)

                val existing = currentAjaxRequests.getOrElseUpdate(renderVersion, Nil)
                currentAjaxRequests += (renderVersion -> (info :: existing))

                info
              }

              val infoList = currentAjaxRequests.get(renderVersion)
              val (requestInfo, result) =
                infoList
                  .flatMap { entries =>
                    entries
                      .find(_.requestVersion == handlerVersion)
                      .map { entry =>
                        (entry, Right(entry.responseFuture))
                      }
                  }
                  .getOrElse {
                    val entry = newRequestInfo

                    (entry, Left(entry.responseFuture))
                  }

              // If there are no other pending requests, we can
              // invalidate all the render version's AJAX entries except
              // for the current one, as the client is no longer looking
              // to retry any of them.
              if (pendingRequests == 0) {
                // Satisfy anyone waiting on futures for invalid
                // requests with a failure.
                for {
                  list <- infoList
                  entry <- list if entry.requestVersion != handlerVersion
                } {
                  entry.responseFuture.satisfy(net.liftweb.common.Failure("Request no longer pending."))
                }

                currentAjaxRequests += (renderVersion -> List(requestInfo))
              }

              result
            }

          case _ =>
            // Create a future that processes the ajax response
            // immediately. This runs if we don't have a handler
            // version, which happens in cases like AJAX requests for
            // Lift GC that don't go through the de-duping pipeline.
            // Because we always return a Left here, the ajax processing
            // always runs for this type of request.
            Left(new LAFuture[Box[LiftResponse]])
        }

      val ret:Box[LiftResponse] =
        nextAction match {
          case Left(future) =>
            val result = runAjax(liftSession, requestState) map CachedResponse

            if (result.exists(_.failed_?)) {
              // The request failed. The client will retry it, so
              // remove it from the list of current Ajax requests that
              // needs to be satisfied so we re-process the next request
              // from scratch
              liftSession.withAjaxRequests { currentAjaxRequests =>
                currentAjaxRequests.remove(RenderVersion.get)
              }
            }

            future.satisfy(result)
            result

          case Right(future) =>
            val ret = future.get(ajaxPostTimeout) openOr net.liftweb.common.Failure("AJAX retry timeout.")

            ret
        }

      tryo {
        LiftSession.onEndServicing.foreach(_(liftSession, requestState, ret))
      }

      ret
    }
  }

/**
   * An actor that manages continuations from container (Jetty style)
   */
  class ContinuationActor(request: Req, session: LiftSession,
                          actors: List[(LiftCometActor, Long)],
                          onBreakout: List[AnswerRender] => Unit) extends LiftActor {
    private var answers: List[AnswerRender] = Nil
    private var done = false
    val seqId = Helpers.nextNum

    def messageHandler = {
      case BeginContinuation =>
        val sendItToMe: AnswerRender => Unit = ah => this ! ah

        actors.foreach {
          case (act, when) => act ! Listen(when, ListenerId(seqId), sendItToMe)
        }

      case ar: AnswerRender =>
        answers = ar :: answers
        LAPinger.schedule(this, BreakOut(), 5.millis)

      case BreakOut() if !done =>
        done = true
        session.exitComet(this)
        actors.foreach {
          case (act, _) => tryo(act ! Unlisten(ListenerId(seqId)))
        }
        onBreakout(answers)

      case _ =>
    }

    override def toString = "Actor dude " + seqId
  }

  private object BeginContinuation

  private lazy val cometTimeout: Long = (LiftRules.cometRequestTimeout openOr 120) * 1000L

  private def setupContinuation(request: Req, session: LiftSession, actors: List[(LiftCometActor, Long)]): Any = {
    val cont = new ContinuationActor(request, session, actors,
      answers => request.request.resume(
        (request, S.init(request, session)
          (LiftRules.performTransform(
            convertAnswersToCometResponse(session,
              answers.toList, actors))))))


    try {
      session.enterComet(cont -> request)

      LAPinger.schedule(cont, BreakOut(), TimeSpan(cometTimeout))

      request.request.suspend(cometTimeout + 2000L)
    } finally {
      cont ! BeginContinuation
    }
  }

  private def handleComet(requestState: Req, sessionActor: LiftSession, originalRequest: Req): Either[Box[LiftResponse], () => Box[LiftResponse]] = {
    val actors: List[(LiftCometActor, Long)] =
      requestState.params.toList.flatMap {
        case (name, when) =>
          sessionActor.getAsyncComponent(name).toList.map(c => (c, toLong(when)))
      }

    if (actors.isEmpty) Left(Full(new JsCommands(LiftRules.noCometSessionCmd.vend :: js.JE.JsRaw("lift.setToWatch({});").cmd :: Nil).toResponse))
    else requestState.request.suspendResumeSupport_? match {
      case true => {
        setupContinuation(requestState, sessionActor, actors)
        Left(Full(EmptyResponse))
      }

      case _ => {
        Right(handleNonContinuationComet(requestState, sessionActor, actors, originalRequest))
      }
    }
  }

  private def convertAnswersToCometResponse(session: LiftSession, ret: Seq[AnswerRender], actors: List[(LiftCometActor, Long)]): LiftResponse = {
    val ret2: List[AnswerRender] = ret.toList
    val jsUpdateTime = ret2.map(ar => "lift.updWatch('" + ar.who.uniqueId + "', '" + ar.when + "');").mkString("\n")
    val jsUpdateStuff = ret2.map {
      ar => {
        val ret = ar.response.toJavaScript(session, ar.displayAll)

        if (!S.functionMap.isEmpty) {
          session.updateFunctionMap(S.functionMap,
            ar.who.uniqueId, ar.when)
          S.clearFunctionMap
        }

        ret
      }
    }

    actors foreach (_._1 ! ClearNotices)

    val addl: List[JsCmd] =
      (for {
        req <- S.request
        rendVer <- extractRenderVersion(req.path.partPath)
      } yield RenderVersion.doWith(rendVer) {
          S.jsToAppend
        }) openOr Nil

    (new JsCommands(JsCmds.Run(jsUpdateTime) :: jsUpdateStuff ::: addl)).toResponse
  }

  private def extractRenderVersion(in: List[String]): Box[String] = in match {
    case _ :: _ :: _ :: rv :: _ => Full(rv)
    case _ => Empty
  }

  private def handleNonContinuationComet(request: Req, session: LiftSession, actors: List[(LiftCometActor, Long)],
                                         originalRequest: Req): () => Box[LiftResponse] = () => {
    val f = new LAFuture[List[AnswerRender]]
    val cont = new ContinuationActor(request, session, actors,
      answers => f.satisfy(answers))

    try {
      cont ! BeginContinuation

      session.enterComet(cont -> request)

      LAPinger.schedule(cont, BreakOut(), TimeSpan(cometTimeout))

      val ret2 = f.get(cometTimeout) openOr Nil

      Full(S.init(originalRequest, session) {
        convertAnswersToCometResponse(session, ret2, actors)
      })
    } finally {
      session.exitComet(cont)
    }
  }

  val dumpRequestResponse = Props.getBool("dump.request.response", false)

  private def logIfDump(request: Req, response: BasicResponse) {
    if (dumpRequestResponse) {
      val toDump = request.uri + "\n" +
        request.params + "\n" +
        response.headers + "\n" +
        (
          response match {
            case InMemoryResponse(data, _, _, _) => new String(data, "UTF-8")
            case _ => "data"
          }
          )

      logger.trace(toDump)
    }
  }

  /**
   * Sends the  { @code HTTPResponse } to the browser using data from the
   * { @link Response } and  { @link Req }.
   */
  private[http] def sendResponse(liftResp: LiftResponse, response: HTTPResponse, request: Req) {
    def fixHeaders(headers: List[(String, String)]) = headers map ((v) => v match {
      case ("Location", uri) =>
        val u = request
        (v._1, (
          (for (
            updated <- Full((if (!LiftRules.excludePathFromContextPathRewriting.vend(uri)) u.contextPath else "") + uri).filter(ignore => uri.startsWith("/"));
            rwf <- URLRewriter.rewriteFunc) yield rwf(updated)) openOr uri
          ))
      case _ => v
    })

    def pairFromRequest(req: Req): (Box[Req], Box[String]) = {
      val acceptHeader = for (innerReq <- Box.legacyNullTest(req.request);
                              accept <- innerReq.header("Accept")) yield accept

      (Full(req), acceptHeader)
    }

    val resp = liftResp.toResponse

    logIfDump(request, resp)

    def insureField(headers: List[(String, String)], toInsure: List[(String, String)]): List[(String, String)] = {
      val org = Map(headers: _*)

      toInsure.foldLeft(org) {
        case (map, (key, value)) =>
          if (map.contains(key)) map
          else map + (key -> value)
      }.toList

    }


    val len = resp.size
    // insure that certain header fields are set
    val header = if (resp.code == 304 || resp.code == 303)
      fixHeaders(resp.headers)
    else
      insureField(fixHeaders(resp.headers),
        LiftRules.defaultHeaders(NodeSeq.Empty -> request) :::
          /* List(("Content-Type",
        LiftRules.determineContentType(pairFromRequest(request)))) ::: */
          (if (len >= 0) List(("Content-Length", len.toString)) else Nil))

    LiftRules.beforeSend.toList.foreach(f => tryo(f(resp, response, header, Full(request))))
    // set the cookies
    response.addCookies(resp.cookies)

    // send the response
    response.addHeaders(header.map {
      case (name, value) => HTTPParam(name, value)
    })
    response.addHeaders(
      LiftRules.supplementalHeaders.vend.map {
        case (name, value) => HTTPParam(name, value)
      }
    )

    liftResp match {
      case ResponseWithReason(_, reason) => response setStatusWithReason (resp.code, reason)
      case _ => response setStatus resp.code
    }

    try {
      resp match {
        case EmptyResponse =>
        case InMemoryResponse(bytes, _, _, _) =>
          response.outputStream.write(bytes)
          response.outputStream.flush()

        case StreamingResponse(stream, endFunc, _, _, _, _) =>
          import scala.language.reflectiveCalls

          try {
            var len = 0
            val ba = new Array[Byte](8192)
            val os = response.outputStream
            stream match {
              case jio: java.io.InputStream => len = jio.read(ba)
              case stream => len = stream.read(ba)
            }
            while (len >= 0) {
              if (len > 0) os.write(ba, 0, len)
              stream match {
                case jio: java.io.InputStream => len = jio.read(ba)
                case stream => len = stream.read(ba)
              }
            }
            response.outputStream.flush()
          } finally {
            endFunc()
          }

        case OutputStreamResponse(out, _, _, _, _) =>
          out(response.outputStream)
          response.outputStream.flush()
      }
    } catch {
      case e: java.io.IOException => // ignore IO exceptions... they happen
    }

    LiftRules.afterSend.toList.foreach(f => tryo(f(resp, response, header, Full(request))))
  }
}

import net.liftweb.http.provider.servlet._

private class SessionIdCalc(req: Req) {
  private val LiftPath = LiftRules.liftContextRelativePath
  lazy val id: Box[String] = req.request.sessionId match {
    case Full(id) => Full(id)
    case _ => req.path.wholePath match {
      case LiftPath :: "comet" :: _ :: id :: _ :: _ => Full(id)
      case _ => Empty
    }
  }
}

class LiftFilter extends ServletFilterProvider
