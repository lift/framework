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
      case e => logger.error("Destruction failure", e)
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

      }, 5 millis)
    }

    if (reqOrg.request.suspendResumeSupport_?) {
      runFunction(liftResponse => {
        // do the actual write on a separate thread
        Schedule.schedule(() => {
          reqOrg.request.resume(reqOrg, liftResponse)
        }, 0 seconds)
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
      case e => logger.info("Request for " + req.request.uri + " failed " + e.getMessage, e); throw e
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

  /**
   * Service the HTTP request
   */
  def doService(req: Req, response: HTTPResponse): Boolean = {
    var tmpStatelessHolder: Box[Box[LiftResponse]] = Empty

    tryo {
      LiftRules.onBeginServicing.toList.foreach(_(req))
    }

    def hasSession(idb: Box[String]): Boolean = {
      idb.flatMap {
        id =>
          registerRecentlyChecked(id)
          SessionMaster.getSession(id, Empty)
      }.isDefined
    }

    def isCometOrAjax(req: Req): Boolean = {
      val wp = req.path.wholePath
      val len = wp.length

      if (len < 2) false
      else {
        val kindaComet = wp.head == LiftRules.cometPath
        val cometScript = (len >= 3 && kindaComet &&
          wp(2) == LiftRules.cometScriptName())
        val kindaAjax = wp.head == LiftRules.ajaxPath
        val ajaxScript = len >= 2 && kindaAjax &&
          wp(1) == LiftRules.ajaxScriptName()


        ((kindaComet && !cometScript) || (kindaAjax && !ajaxScript)) &&
          req.acceptsJavaScript_?
      }
    }

    val sessionIdCalc = new SessionIdCalc(req)

    val resp: Box[LiftResponse] = try {
      // if the application is shutting down, return a 404
      if (LiftRules.ending) {
        LiftRules.notFoundOrIgnore(req, Empty)
      } else if (!authPassed_?(req)) {
        Full(LiftRules.authentication.unauthorizedResponse)
      } else if (LiftRules.redirectAjaxOnSessionLoss && !hasSession(sessionIdCalc.id) && isCometOrAjax(req)) {

        val theId = sessionIdCalc.id
        // okay after 2 attempts to redirect, just
        // ignore calls to the comet URL
        if (recentlyChecked(theId) > 1) {
          Empty
        } else {
          Full(JavaScriptResponse(js.JE.JsRaw("window.location = " +
            (req.request.
              header("Referer") openOr
              "/").encJs).cmd, Nil, Nil, 200))
        }
      } else
      // if the request is matched is defined in the stateless table, dispatch
      if (S.statelessInit(req) {
        tmpStatelessHolder = NamedPF.applyBox(req,
          LiftRules.statelessDispatch.toList).map(_.apply() match {
          case Full(a) => Full(LiftRules.convertResponse((a, Nil, S.responseCookies, req)))
          case r => r
        });
        tmpStatelessHolder.isDefined
      }) {
        val f = tmpStatelessHolder.open_!
        f match {
          case Full(v) => Full(v)
          case Empty => LiftRules.notFoundOrIgnore(req, Empty)
          case f: Failure => Full(req.createNotFound(f))
        }
      } else {
        // otherwise do a stateful response
        val liftSession = getLiftSession(req)

        def doSession(r2: Req, s2: LiftSession, continue: Box[() => Nothing]): () => Box[LiftResponse] = {
          try {
            S.init(r2, s2) {
              dispatchStatefulRequest(S.request.open_!, liftSession, r2, continue)
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
    } catch {
      case foc: LiftFlowOfControlException => throw foc
      case e: Exception if !e.getClass.getName.endsWith("RetryRequest") => {
        val bundle = (Props.mode, req, e)
        S.assertExceptionThrown()
        NamedPF.applyBox(bundle, LiftRules.exceptionHandler.toList)
      }
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

                    case f: Failure =>
                      (true, Full(liftSession.checkRedirect(req.createNotFound(f))))
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

    val toReturn: () => Box[LiftResponse] =
      if (dispatch._1) {
        respToFunc(dispatch._2)
      } else if (wp.length == 3 && wp.head == LiftRules.cometPath &&
        wp(2) == LiftRules.cometScriptName()) {
        respToFunc(LiftRules.serveCometScript(liftSession, req))
      } else if ((wp.length >= 1) && wp.head == LiftRules.cometPath) {
        handleComet(req, liftSession, originalRequest) match {
          case Left(x) => respToFunc(x)
          case Right(x) => x
        }
      } else if (wp.length == 2 && wp.head == LiftRules.ajaxPath &&
        wp(1) == LiftRules.ajaxScriptName()) {
        respToFunc(LiftRules.serveAjaxScript(liftSession, req))
      } else if (wp.length >= 1 && wp.head == LiftRules.ajaxPath) {
        respToFunc(handleAjax(liftSession, req))
      } else {
        respToFunc(liftSession.processRequest(req, continuation))
      }

    toReturn
  }

  private def extractVersion[T](path: List[String])(f: => T): T = {
    path match {
      case first :: second :: _ => RenderVersion.doWith(second)(f)
      case _ => f
    }
  }

  private def handleAjax(liftSession: LiftSession,
                         requestState: Req): Box[LiftResponse] = {
    extractVersion(requestState.path.partPath) {

      LiftRules.cometLogger.debug("AJAX Request: " + liftSession.uniqueId + " " + requestState.params)
      tryo {
        LiftSession.onBeginServicing.foreach(_(liftSession, requestState))
      }

      val ret = try {
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
                  import js.JsCmds._
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
                    ((js :: xs).flatMap {
                      case js: JsCmd => List(js)
                      case _ => Nil
                    }.reverse) &
                    S.jsToAppend).toResponse
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
        case e => S.assertExceptionThrown() ; NamedPF.applyBox((Props.mode, requestState, e), LiftRules.exceptionHandler.toList);
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
        LAPinger.schedule(this, BreakOut(), 5 millis)

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

    if (actors.isEmpty) Left(Full(new JsCommands(new JE.JsRaw("lift_toWatch = {}") with JsCmd :: JsCmds.RedirectTo(LiftRules.noCometSessionPage) :: Nil).toResponse))
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
    val jsUpdateTime = ret2.map(ar => "if (lift_toWatch['" + ar.who.uniqueId + "'] !== undefined) lift_toWatch['" + ar.who.uniqueId + "'] = '" + ar.when + "';").mkString("\n")
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
    LiftRules.supplimentalHeaders(response)

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

import provider.servlet._

class LiftFilter extends ServletFilterProvider

private class SessionIdCalc(req: Req) {
  private val CometPath = LiftRules.cometPath
  lazy val id: Box[String] = req.request.sessionId match {
    case Full(id) => Full(id)
    case _ => req.path.wholePath match {
      case CometPath :: _ :: id :: _ if id != LiftRules.cometScriptName() => Full(id)
      case _ => Empty
    }
  }
}
