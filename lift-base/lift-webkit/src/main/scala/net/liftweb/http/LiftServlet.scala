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

import _root_.java.net.URLDecoder
import _root_.scala.xml.{Node, NodeSeq, Group, Elem, MetaData, Null, XML, Comment, Text}
import _root_.scala.collection.immutable.HashMap
import _root_.scala.xml.transform._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers
import _root_.net.liftweb.util.ActorPing
import _root_.java.util.{Locale, ResourceBundle}
import _root_.java.net.URL
import js._
import auth._
import provider._

import _root_.net.liftweb.actor._


class LiftServlet {
  private var servletContext: HTTPContext = null

  def this(ctx: HTTPContext) = {
    this ()
    this.servletContext = ctx
  }

  def getContext: HTTPContext = servletContext

  def destroy = {
    try {
      LiftRules.ending = true
      LiftRules.runUnloadHooks()
      ActorPing.shutdown
      LAScheduler.shutdown
      Log.debug("Destroyed Lift handler.")
      // super.destroy
    } catch {
      case e => Log.error("Destruction failure", e)
    }
  }

  def init = {
    LiftRules.ending = false
  }

  def getLiftSession(request: Req): LiftSession = LiftRules.getLiftSession(request)

  /**
   * Processes the HTTP requests
   */
  def service(req: Req, resp: HTTPResponse): Boolean = {
    try {
      def doIt: Boolean = {
        if (LiftRules.logServiceRequestTiming) {
          logTime("Service request (" + req.request.method + ") " + req.request.uri) {
            doService(req, resp)
          }
        } else {
          doService(req, resp)
        }
      }
      LiftRules.checkContinuations(req request) match {
        case None => doIt
        case r if r eq null => doIt
        case Some((or: Req, r: LiftResponse)) if (req.path == or.path) => sendResponse(r.toResponse, resp, Empty); true
        case _ => doIt
      }
    } catch {
      case e if e.getClass.getName.endsWith("RetryRequest") => throw e
      case e => Log.warn("Request for " + req.request.uri + " failed " + e.getMessage, e); throw e
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
      case _ => true
    }) openOr true
  }

  /**
   * Service the HTTP request
   */
  def doService(req: Req, response: HTTPResponse): Boolean = {
    var tmpStatelessHolder: Box[() => Box[LiftResponse]] = null

    tryo {LiftRules.onBeginServicing.toList.foreach(_(req))}

    def hasSession(req: Req): Boolean = {
      req.request.sessionId.flatMap(id => SessionMaster.getSession(id, Empty)).isDefined
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

    val resp: Box[LiftResponse] =
    // if the application is shutting down, return a 404
    if (LiftRules.ending) {
      LiftRules.notFoundOrIgnore(req, Empty)
    } else if (!authPassed_?(req)) {
      Full(LiftRules.authentication.unauthorizedResponse)
    } else if (LiftRules.redirectAjaxOnSessionLoss && !hasSession(req) && isCometOrAjax(req)) {
      Full(JavaScriptResponse(js.JE.JsRaw("window.location = " +
              (req.request.header("Referer") openOr "/").encJs).cmd, Nil, Nil, 200))
    } else
    // if the request is matched is defined in the stateless table, dispatch
    if ({
      tmpStatelessHolder = NamedPF.applyBox(req,
        LiftRules.statelessDispatchTable.toList);
      tmpStatelessHolder.isDefined
    })
      {
        val f = tmpStatelessHolder.open_!
        f() match {
          case Full(v) => Full(LiftRules.convertResponse((v, Nil, S.responseCookies, req)))
          case Empty => LiftRules.notFoundOrIgnore(req, Empty)
          case f: Failure => Full(req.createNotFound(f))
        }
      } else {
      // otherwise do a stateful response
      val liftSession = getLiftSession(req)
      S.init(req, liftSession) {
        dispatchStatefulRequest(S.request.open_!, liftSession)
      }
    }

    tryo {LiftRules.onEndServicing.toList.foreach(_(req, resp))}

    resp match {
      case Full(cresp) =>
        val resp = cresp.toResponse

        logIfDump(req, resp)

        sendResponse(resp, response, Full(req))
        true

      case _ => false
    }
  }

  private def dispatchStatefulRequest(req: Req,
                                      liftSession: LiftSession):
  Box[LiftResponse] =
    {
      val toMatch = req

      val dispatch: (Boolean, Box[LiftResponse]) =
      NamedPF.find(toMatch, LiftRules.dispatchTable(req.request)) match {
        case Full(pf) =>
          LiftSession.onBeginServicing.foreach(_(liftSession, req))
          val ret: (Boolean, Box[LiftResponse]) =
          try {
            try {
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
              case ite: _root_.java.lang.reflect.InvocationTargetException if (ite.getCause.isInstanceOf[ResponseShortcutException]) =>
                (true, Full(liftSession.handleRedirect(ite.getCause.asInstanceOf[ResponseShortcutException], req)))

              case rd: _root_.net.liftweb.http.ResponseShortcutException => (true, Full(liftSession.handleRedirect(rd, req)))

              case e => (true, NamedPF.applyBox((Props.mode, req, e), LiftRules.exceptionHandler.toList))

            }

          } finally {
             if (S.functionMap.size > 0) {
                liftSession.updateFunctionMap(S.functionMap, S.uri, millis)
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

      if (LiftRules.enableContainerSessions) req.request.session

      val toTransform: Box[LiftResponse] =
      if (dispatch._1)
        {
          dispatch._2
        } else if (wp.length == 3 && wp.head == LiftRules.cometPath &&
              wp(2) == LiftRules.cometScriptName()) {
        LiftRules.serveCometScript(liftSession, req)
      } else if ((wp.length >= 1) && wp.head == LiftRules.cometPath) {
        handleComet(req, liftSession)
      } else if (wp.length == 2 && wp.head == LiftRules.ajaxPath &&
              wp(1) == LiftRules.ajaxScriptName()) {
        LiftRules.serveAjaxScript(liftSession, req)
      } else if (wp.length >= 1 && wp.head == LiftRules.ajaxPath) {
        handleAjax(liftSession, req)
      } else {
        liftSession.processRequest(req)
      }

      toTransform.map(LiftRules.performTransform)
    }

  private def extractVersion(path: List[String]) {
    path match {
      case first :: second :: _ => RenderVersion.set(second)
      case _ =>
    }
  }

  private def handleAjax(liftSession: LiftSession,
                         requestState: Req): Box[LiftResponse] =
    {
      extractVersion(requestState.path.partPath)

      LiftRules.cometLogger.debug("AJAX Request: " + liftSession.uniqueId + " " + requestState.params)
      tryo {LiftSession.onBeginServicing.foreach(_(liftSession, requestState))}

      val ret = try {
        requestState.param("__lift__GC") match {
        case Full(_) =>
          liftSession.updateFuncByOwner(RenderVersion.get, millis)
          Full(JavaScriptResponse(js.JsCmds.Noop))

        case _ =>
          try {
            val what = flatten(liftSession.runParams(requestState))

            val what2 = what.flatMap {
              case js: JsCmd => List(js)
              case n: NodeSeq => List(n)
              case js: JsCommands => List(js)
              case r: LiftResponse => List(r)
              case s => Nil
            }

            val ret: LiftResponse = what2 match {
              case (json: JsObj) :: Nil => JsonResponse(json)
              case (js: JsCmd) :: xs => (JsCommands(S.noticesToJsCmd :: Nil) & ((js :: xs).flatMap {case js: JsCmd => List(js) case _ => Nil}.reverse)).toResponse
              case (n: Node) :: _ => XmlResponse(n)
              case (ns: NodeSeq) :: _ => XmlResponse(Group(ns))
              case (r: LiftResponse) :: _ => r
              case _ => JsCommands(S.noticesToJsCmd :: JsCmds.Noop :: Nil).toResponse
            }

            LiftRules.cometLogger.debug("AJAX Response: " + liftSession.uniqueId + " " + ret)

            Full(ret)
          } finally {
            liftSession.updateFunctionMap(S.functionMap, RenderVersion.get, millis)
          }
        }
      } catch {
        case e => NamedPF.applyBox((Props.mode, requestState, e), LiftRules.exceptionHandler.toList);
      }
      tryo {LiftSession.onEndServicing.foreach(_(liftSession, requestState, ret))}
      ret
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

        actors.foreach {case (act, when) => act ! Listen(when, ListenerId(seqId), sendItToMe)}

      case ar: AnswerRender =>
        answers = ar :: answers
        LAPinger.schedule(this, BreakOut, 5 millis)

      case BreakOut if !done =>
        done = true
        session.exitComet(this)
        actors.foreach {case (act, _) => tryo(act ! Unlisten(ListenerId(seqId)))}
        onBreakout(answers)

      case _ =>
    }

    override def toString = "Actor dude " + seqId
  }

  private object BeginContinuation

  private lazy val cometTimeout: Long = (LiftRules.cometRequestTimeout openOr 120) * 1000L

  private def setupContinuation(request: Req, session: LiftSession, actors: List[(LiftCometActor, Long)]): Nothing = {
    val cont = new ContinuationActor(request, session, actors,
      answers => LiftRules.resumeRequest(
        (request, S.init(request, session)
                  (LiftRules.performTransform(
          convertAnswersToCometResponse(session,
            answers.toArray, actors)))),
        request.request))

    cont ! BeginContinuation

    session.enterComet(cont)

    LAPinger.schedule(cont, BreakOut, TimeSpan(cometTimeout))

    LiftRules.doContinuation(request.request, cometTimeout + 2000L)
  }

  private def handleComet(requestState: Req, sessionActor: LiftSession): Box[LiftResponse] = {
    val actors: List[(LiftCometActor, Long)] =
    requestState.params.toList.flatMap {
      case (name, when) =>
        sessionActor.getAsyncComponent(name).toList.map(c => (c, toLong(when)))
    }

    if (actors.isEmpty) Full(new JsCommands(JsCmds.RedirectTo(LiftRules.noCometSessionPage) :: Nil).toResponse)
    else LiftRules.checkContinuations(requestState.request) match {
      case Some(null) =>
        setupContinuation(requestState, sessionActor, actors)

      case _ =>
        handleNonContinuationComet(requestState, sessionActor, actors)
    }
  }

  private def convertAnswersToCometResponse(session: LiftSession, ret: Seq[AnswerRender], actors: List[(LiftCometActor, Long)]): LiftResponse = {
    val ret2: List[AnswerRender] = ret.toList
    val jsUpdateTime = ret2.map(ar => "if (lift_toWatch['" + ar.who.uniqueId + "'] !== undefined) lift_toWatch['" + ar.who.uniqueId + "'] = '" + ar.when + "';").mkString("\n")
    val jsUpdateStuff = ret2.map {
      ar =>
              val ret = ar.response.toJavaScript(session, ar.displayAll)

              if (!S.functionMap.isEmpty) {
                session.updateFunctionMap(S.functionMap,
                  ar.who.uniqueId, ar.when)
                S.clearFunctionMap
              }

              ret
    }

    actors foreach (_._1 ! ClearNotices)

    (new JsCommands(JsCmds.Run(jsUpdateTime) :: jsUpdateStuff)).toResponse
  }

  private def handleNonContinuationComet(request: Req, session: LiftSession, actors: List[(LiftCometActor, Long)]): Box[LiftResponse] = {
    val f = new LAFuture[List[AnswerRender]]
    val cont = new ContinuationActor(request, session, actors,
      answers => f.satisfy(answers))

    try {
      cont ! BeginContinuation

      session.enterComet(cont)

      LAPinger.schedule(cont, BreakOut, TimeSpan(cometTimeout))

      val ret2 = f.get(cometTimeout) openOr Nil
      Full(convertAnswersToCometResponse(session, ret2, actors))
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

      Log.info(toDump)
    }
  }

  /**
   * Sends the  { @code HTTPResponse } to the browser using data from the
   * { @link Response } and  { @link Req }.
   */
  def sendResponse(resp: BasicResponse, response: HTTPResponse, request: Box[Req]) {
    def fixHeaders(headers: List[(String, String)]) = headers map ((v) => v match {
      case ("Location", uri) => (v._1, (
              (for (u <- request;
                    updated <- Full((if (!LiftRules.excludePathFromContextPathRewriting.vend(uri)) u.contextPath else "") + uri) if (uri.startsWith("/"));
                    f <- URLRewriter.rewriteFunc map (_(updated))) yield f) openOr uri
              ))
      case _ => v
    })

    def pairFromRequest(in: Box[Req]): (Box[Req], Box[String]) = {
      val acceptHeader = for (req <- in;
                              innerReq <- Box.legacyNullTest(req.request);
                              accept <- innerReq.header("Accept")) yield accept

      (in, acceptHeader)
    }


    val len = resp.size
    // insure that certain header fields are set
    val header = insureField(fixHeaders(resp.headers), List(("Content-Type",
            LiftRules.determineContentType(pairFromRequest(request))),
      ("Content-Length", len.toString)))

    LiftRules.beforeSend.toList.foreach(f => tryo(f(resp, response, header, request)))
    // set the cookies
    response.addCookies(resp.cookies)

    // send the response
    response.addHeaders(header.map {case (name, value) => HTTPParam(name, value)})
    LiftRules.supplimentalHeaders(response)

    response setStatus resp.code

    try {
      resp match {
        case InMemoryResponse(bytes, _, _, _) =>
          response.outputStream.write(bytes)
          response.outputStream.flush()

        case StreamingResponse(stream, endFunc, _, _, _, _) =>
          try {
            var len = 0
            val ba = new Array[Byte](8192)
            val os = response.outputStream
            stream match {
              case jio: _root_.java.io.InputStream => len = jio.read(ba)
              case stream => len = stream.read(ba)
            }
            while (len >= 0) {
              if (len > 0) os.write(ba, 0, len)
              stream match {
                case jio: _root_.java.io.InputStream => len = jio.read(ba)
                case stream => len = stream.read(ba)
              }
            }
            response.outputStream.flush()
          } finally {
            endFunc()
          }
      }
    } catch {
      case e: _root_.java.io.IOException => // ignore IO exceptions... they happen
    }

    LiftRules.afterSend.toList.foreach(f => tryo(f(resp, response, header, request)))
  }
}

import provider.servlet._

class LiftFilter extends ServletFilterProvider

}
}
