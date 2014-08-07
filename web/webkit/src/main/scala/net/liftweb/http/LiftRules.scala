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

import common._
import util._
import util.Helpers._
import sitemap._
import http.js.JSArtifacts
import http.js.jquery._
import http.provider._
import js._
import JE._
import JsCmds._
import auth._

import scala.xml._
import java.util.{Locale, TimeZone, ResourceBundle, Date}
import java.io.{InputStream, ByteArrayOutputStream, BufferedReader, StringReader}
import java.util.concurrent.{ConcurrentHashMap => CHash}
import scala.reflect.Manifest

import java.util.concurrent.atomic.AtomicInteger
import net.liftweb.actor.{LiftActor, LAFuture}

class LiftRulesJBridge {
  def liftRules: LiftRules = LiftRules
}

sealed trait LiftRulesMocker {
  def realInstance: LiftRules
}

object LiftRulesMocker {
  implicit def toLiftRules(in: LiftRulesMocker): LiftRules = in.realInstance

  /**
   * In Dev and Test mode, there's an option to stuff another LiftRules
   * instance in here and use that one for mocking
   */
  object devTestLiftRulesInstance extends ThreadGlobal[LiftRules]

  /**
   * This function, in Test and Dev mode will vend the instance of LiftRules.
   * If there is an instance set in devTestLiftRulesInstance, that instance
   * will be used, otherwise the global instance in LiftRules.prodInstance
   * will be used.
   */
  @volatile var calcLiftRulesInstance: () => LiftRules =
    () => devTestLiftRulesInstance.box.openOr( LiftRules.prodInstance)
}

/**
 * The data structure that contains information to determine if the
 * request should be treated as a stateful or stateless request
 */
final case class StatelessReqTest(path: List[String], httpReq: HTTPRequest)

/**
 * Sometimes we're going to have to surface more data from one of these requests
 * than we might like (for example, extra info about continuing the computation on
 * a different thread), so we'll start off right by having an Answer trait
 * that will have some subclasses and implicit conversions
 */
sealed trait DataAttributeProcessorAnswer

/**
 * The companion object that has the implicit conversions
 */
object DataAttributeProcessorAnswer {
  implicit def nodesToAnswer(in: NodeSeq): DataAttributeProcessorAnswer = DataAttributeProcessorAnswerNodes(in)
  implicit def nodeFuncToAnswer(in: () => NodeSeq): DataAttributeProcessorAnswer = DataAttributeProcessorAnswerFork(in)
  implicit def nodeFutureToAnswer(in: LAFuture[NodeSeq]): DataAttributeProcessorAnswer = DataAttributeProcessorAnswerFuture(in)
  implicit def setNodeToAnswer(in: Seq[Node]): DataAttributeProcessorAnswer = DataAttributeProcessorAnswerNodes(in)
}

/**
 * Yep... just a bunch of nodes.
 * @param nodes
 */
final case class DataAttributeProcessorAnswerNodes(nodes: NodeSeq) extends DataAttributeProcessorAnswer

/**
 * A function that returns a bunch of nodes... run it on a different thread
 * @param nodeFunc
 */
final case class DataAttributeProcessorAnswerFork(nodeFunc: () => NodeSeq) extends DataAttributeProcessorAnswer

/**
 * A future that returns nodes... run them on a different thread
 * @param nodeFuture the future of the NodeSeq
 */
final case class DataAttributeProcessorAnswerFuture(nodeFuture: LAFuture[NodeSeq]) extends DataAttributeProcessorAnswer

/**
 * The Lift configuration singleton
 */
object LiftRules extends LiftRulesMocker {
  lazy val prodInstance: LiftRules = new LiftRules()

  private[this] val devOrTest = Props.devMode || Props.testMode

  /**
   * Get the real instance of LiftRules
   */
  def realInstance: LiftRules = if (devOrTest) {
    LiftRulesMocker.calcLiftRulesInstance()
  } else prodInstance

  type DispatchPF = PartialFunction[Req, () => Box[LiftResponse]];

  /**
   * A partial function that allows processing of any attribute on an Elem
   * if the attribute begins with "data-"
   */
  type DataAttributeProcessor = PartialFunction[(String, String, Elem, LiftSession), DataAttributeProcessorAnswer]

  /**
   * The pattern/PartialFunction for matching tags in Lift
   */
  type TagProcessor = PartialFunction[(String, Elem, LiftSession), DataAttributeProcessorAnswer]

  /**
   * The test between the path of a request and whether that path
   * should result in stateless servicing of that path
   */
  type StatelessTestPF = PartialFunction[List[String], Boolean]


  /**
   * The test between the path of a request, the HTTP request, and whether that path
   * should result in stateless servicing of that path
   */
  type StatelessReqTestPF = PartialFunction[StatelessReqTest, Boolean]

  type RewritePF = PartialFunction[RewriteRequest, RewriteResponse]
  type SnippetPF = PartialFunction[List[String], NodeSeq => NodeSeq]
  type LiftTagPF = PartialFunction[(String, Elem, MetaData, NodeSeq, String), NodeSeq]
  type URINotFoundPF = PartialFunction[(Req, Box[Failure]), NotFound]
  type URLDecoratorPF = PartialFunction[String, String]
  type SnippetDispatchPF = PartialFunction[String, DispatchSnippet]
  type ViewDispatchPF = PartialFunction[List[String], Either[() => Box[NodeSeq], LiftView]]
  type HttpAuthProtectedResourcePF = PartialFunction[Req, Box[Role]]
  type ExceptionHandlerPF = PartialFunction[(Props.RunModes.Value, Req, Throwable), LiftResponse]
  type ResourceBundleFactoryPF = PartialFunction[(String, Locale), ResourceBundle]
  type SplitSuffixPF = PartialFunction[List[String], (List[String], String)]
  type CometCreationPF = PartialFunction[CometCreationInfo, LiftCometActor]
  /**
   * A partial function that allows the application to define requests that should be
   * handled by lift rather than the default handler
   */
  type LiftRequestPF = PartialFunction[Req, Boolean]

  /*
  private[this] var _doneBoot = false
  private[http] def doneBoot = _doneBoot

  private[http] def doneBoot_=(in: Boolean) {_doneBoot = in}
*/


  /**
   * Holds the failure information when a snippet can not be executed.
   */
  case class SnippetFailure(page: String, typeName: Box[String], failure: SnippetFailures.Value)

  object SnippetFailures extends Enumeration {
    val NoTypeDefined = Value(1, "No Type Defined")
    val ClassNotFound = Value(2, "Class Not Found")
    val StatefulDispatchNotMatched = Value(3, "Stateful Snippet: Dispatch Not Matched")
    val MethodNotFound = Value(4, "Method Not Found")
    val NoNameSpecified = Value(5, "No Snippet Name Specified")
    val InstantiationException = Value(6, "Exception During Snippet Instantiation")
    val DispatchSnippetNotMatched = Value(7, "Dispatch Snippet: Dispatch Not Matched")

    val StateInStateless = Value(8, "Access to Lift's statefull features from Stateless mode")
    val CometTimeout = Value(9, "Comet Component did not response to requests")
    val CometNotFound = Value(10, "Comet Component not found")
    val ExecutionFailure = Value(11, "Execution Failure")
    val NoCometType = Value(12, "Comet Type not specified")
  }

  def defaultFuncNameGenerator(runMode: Props.RunModes.Value): () => String =
    runMode match {
      case Props.RunModes.Test => S.generateTestFuncName _
      case _                   => S.generateFuncName _
    }
}

/**
 * LiftRules is the global object that holds all of Lift's configuration.
 */
class LiftRules() extends Factory with FormVendor with LazyLoggable {
  import LiftRules._

  private var _doneBoot = false

  /**
   * Does the LiftRules instance think it's done booting?
   */
  def doneBoot = _doneBoot

  def noticesContainerId = "lift__noticesContainer__"

  /**
   * If you want to make the Lift inactivity timeout shorter than
   * the container inactivity timeout, set the inactivity timeout here
   */
  val sessionInactivityTimeout = new FactoryMaker[Box[Long]](Empty){}

  /**
   * The function that converts a scala.text.Document to
   * a String (used for JsonAST.JValue to text convertion.
   * By default, use Printer.pretty for dev mode and
   * Printer.compact for other modes
   */
  val jsonOutputConverter = new FactoryMaker[scala.text.Document => String]({
    import json.Printer
    if (Props.devMode) Printer.pretty _ else Printer.compact _}){}


  /**
   * Set the default fadeout mechanism for Lift notices. Thus you provide a function that take a NoticeType.Value
   * and decide the duration after which the fade out will start and the actual fadeout time. This is applicable
   * for general notices (not associated with id-s) regardless if they are set for the page rendering, ajax
   * response or Comet response.
   */
  val noticesAutoFadeOut = new FactoryMaker[(NoticeType.Value) => Box[(TimeSpan, TimeSpan)]]((notice : NoticeType.Value) => Empty){}

  /**
   * Use this to apply various effects to the notices. The user function receives the NoticeType
   * and the id of the element containing the specific notice. Thus it is the function's responsibility to form
   * the javascript code for the visual effects. This is applicable for both ajax and non ajax contexts.
   * For notices associated with ID's the user type will receive an Empty notice type. That's because the effect
   * is applied on the real estate holding the notices for this ID. Typically this contains a single message.
   */
  val noticesEffects = new FactoryMaker[(Box[NoticeType.Value], String) => Box[JsCmd]]((notice: Box[NoticeType.Value], id: String) => Empty){}


  /**
   * Holds user functions that will be executed very early in the request processing. The functions'
   * result will be ignored.
   */
  val early = RulesSeq[(HTTPRequest) => Any]

  /**
   * Holds user functions that are executed before sending the response to client. The functions'
   * result will be ignored.
   */
  val beforeSend = RulesSeq[(BasicResponse, HTTPResponse, List[(String, String)], Box[Req]) => Any]

  /**
   * Defines the resources that are protected by authentication and authorization. If this function
   * is not defined for the input data, the resource is considered unprotected ergo no authentication
   * is performed. If this function is defined and returns a Full box, it means that this resource
   * is protected by authentication, and authenticated subjed must be assigned to the role returned by
   * this function or to a role that is child-of this role. If this function returns Empty it means that
   * this resource is protected by authentication but no authorization is performed meaning that roles are
   * not verified.
   */
  val httpAuthProtectedResource = RulesSeq[HttpAuthProtectedResourcePF]

  /**
   * The HTTP authentication mechanism that Lift will perform. See <i>LiftRules.protectedResource</i>
   */
  @volatile var authentication: HttpAuthentication = NoAuthentication

  /**
   * A function that takes the HTTPSession and the contextPath as parameters
   * and returns a LiftSession reference. This can be used in cases subclassing
   * LiftSession is necessary.
   */
  @volatile var sessionCreator: (HTTPSession, String) => LiftSession = {
    case (httpSession, contextPath) => new LiftSession(contextPath, httpSession.sessionId, Full(httpSession))
  }

  /**
   * A method that returns a function to create migratory sessions.  If you want migratory sessions for your
   * application, <code>LiftRules.sessionCreator = LiftRules.sessionCreatorForMigratorySessions</code>
   */
  def sessionCreatorForMigratorySessions: (HTTPSession, String) => LiftSession = {
    case (httpSession, contextPath) => new LiftSession(contextPath, httpSession.sessionId, Full(httpSession)) with MigratorySession
  }

  @volatile var enableContainerSessions = true

  @volatile var getLiftSession: (Req) => LiftSession = (req) => _getLiftSession(req)

  // Unique identifier for this particular instance of Lift, used for
  // tagging resources below in attachResourceId.
  private val instanceResourceId = "instance-" + Helpers.nextFuncName

  /**
   * Attaches an ID entity for resource URI specified in
   * link or script tags. This allows controlling browser
   * resource caching. By default this just adds a query string
   * parameter unique per application lifetime. More complex
   * implementation could user per resource MD5 sequences thus
   * "forcing" browsers to refresh the resource only when the resource
   * file changes. Users can define other rules as well. Inside user's
   * function it is safe to use S context as attachResourceId is called
   * from inside the &lt;lift:with-resource-id&gt; snippet
   *
   */
  @volatile var attachResourceId: (String) => String = (name) => {
    name + (if (name contains ("?")) "&" else "?") + instanceResourceId + "=_"
  }

  /**
   * Returns a LiftSession instance.
   */
  private def _getLiftSession(req: Req): LiftSession = {
    val wp = req.path.wholePath
    val LiftPath = LiftRules.liftContextRelativePath
    val cometSessionId = wp match {
      case LiftPath :: "comet" :: _ :: session :: _ => Full(session)
      case _ => Empty
    }

    val ret = SessionMaster.getSession(req, cometSessionId) match {
      case Full(ret) =>
        ret.fixSessionTime()
        ret

      case Failure(_, _, _) =>
        LiftRules.statelessSession.vend.apply(req)

      case _ =>
        val ret = LiftSession(req)
        ret.fixSessionTime()
        SessionMaster.addSession(ret, req,
                                 req.request.userAgent,
                                 SessionMaster.getIpFromReq(req))
        ret
    }

    makeCometBreakoutDecision(ret, req)
    ret
  }

  /**
   * A function that takes appropriate action in breaking out of any
   * existing comet requests based on the request, browser type, etc.
   */
  @volatile var makeCometBreakoutDecision: (LiftSession, Req) => Unit =
  (session, req) => {
    // get the open sessions to the host (this means that any DNS wildcarded
    // Comet requests will not be counted), as well as all invalid/expired
    // sessions
    val (which, invalid) = session.cometForHost(req.hostAndPath)

    // get the maximum requests given the browser type
    val max = maxConcurrentRequests.vend(req) - 2 // this request and any open comet requests

    // dump the oldest requests
    which.drop(max).foreach {
      case (actor, req) => actor ! BreakOut()
    }
    invalid.foreach {
      case (actor, req) => actor ! BreakOut()
    }
  }

  /**
   * The path to handle served resources
   */
  @volatile var resourceServerPath = "classpath"

  /**
   * Holds the JS library specific UI artifacts. By default it uses JQuery's artifacts
   */
  @volatile var jsArtifacts: JSArtifacts = JQueryArtifacts

  /**
   * Use this PartialFunction to to automatically add static URL parameters
   * to any URL reference from the markup of Ajax request.
   */
  val urlDecorate = RulesSeq[URLDecoratorPF]

  /**
   * Should the JSESSIONID be encoded in the URL if cookies are
   * not supported
   */
  @volatile var encodeJSessionIdInUrl_? = false

  /**
  * Partial function to allow you to build a CometActor from code rather than via reflection
  */
  val cometCreation = RulesSeq[CometCreationPF]

  private def noComet(ignore: CometCreationInfo): Box[LiftCometActor] = Empty

  /**
  * A factory that will vend comet creators
  */
  val cometCreationFactory: FactoryMaker[CometCreationInfo => Box[LiftCometActor]] =
  new FactoryMaker(() => noComet _) {}

  /**
   * Should codes that represent entities be converted to XML
   * entities when rendered?
   */
  val convertToEntity: FactoryMaker[Boolean] = new FactoryMaker(false) {}

  /**
   * Certain paths and requests within your application can be marked as stateless
   * and if there is access to Lift's stateful facilities (setting
   * SessionVars, updating function tables, etc.) the developer will
   * receive a notice and the operation will not complete.
   */
  val statelessReqTest = RulesSeq[StatelessReqTestPF]

  val statelessSession: FactoryMaker[Req => LiftSession with StatelessSession] =
    new FactoryMaker((req: Req) => new LiftSession(req.contextPath,
                                                   Helpers.nextFuncName,
                                                   Empty) with
                     StatelessSession) {}


  /**
   * Holds user functions that are executed after the response is sent to client. The functions' result
   * will be ignored.
   */
  val afterSend = RulesSeq[(BasicResponse, HTTPResponse, List[(String, String)], Box[Req]) => Any]

  /**
   * Calculate the Comet Server (by default, the server that
   * the request was made on, but can do the multi-server thing
   * as well)
   */
  @volatile var cometServer: () => Option[String] = () => None

  /**
   * The maximum concurrent requests.  If this number of
   * requests are being serviced for a given session, messages
   * will be sent to all Comet requests to terminate
   */
  val maxConcurrentRequests: FactoryMaker[Req => Int] = new FactoryMaker((x: Req) => x match {
    case r if r.isIPad || r.isIPhone => 1
    case r if r.isFirefox35_+ || r.isIE8 || r.isIE9 || r.isChrome3_+ || r.isOpera9 || r.isSafari3_+ => 4
    case _ => 2
  }) {}

  /**
   * A partial function that determines content type based on an incoming
   * Req and Accept header
   */
  @volatile var determineContentType: PartialFunction[(Box[Req], Box[String]), String] = {
    case (_, Full(accept)) if this.useXhtmlMimeType && accept.toLowerCase.contains("application/xhtml+xml") =>
      "application/xhtml+xml; charset=utf-8"
    case _ => "text/html; charset=utf-8"
  }

  lazy val liftVersion: String = {
    val cn = """\.""".r.replaceAllIn(LiftRules.getClass.getName, "/")
    val ret: Box[String] =
    for{
      url <- Box !! LiftRules.getClass.getResource("/" + cn + ".class")
      newUrl = new java.net.URL(url.toExternalForm.split("!")(0) + "!" + "/META-INF/MANIFEST.MF")
      str <- tryo(new String(readWholeStream(newUrl.openConnection.getInputStream), "UTF-8"))
      ma <- """Implementation-Version: (.*)""".r.findFirstMatchIn(str)
    } yield ma.group(1)

    ret openOr "Unknown Lift Version"
  }

  lazy val liftBuildDate: Date = {
    val cn = """\.""".r.replaceAllIn(LiftRules.getClass.getName, "/")
    val ret: Box[Date] =
    for{
      url <- Box !! LiftRules.getClass.getResource("/" + cn + ".class")
      newUrl = new java.net.URL(url.toExternalForm.split("!")(0) + "!" + "/META-INF/MANIFEST.MF")
      str <- tryo(new String(readWholeStream(newUrl.openConnection.getInputStream), "UTF-8"))
      ma <- """Built-Time: (.*)""".r.findFirstMatchIn(str)
      asLong <- asLong(ma.group(1))
    } yield new Date(asLong)

    ret openOr new Date(0L)
  }

  /**
   * Hooks to be run when LiftServlet.destroy is called.
   */
  val unloadHooks = RulesSeq[() => Unit]

  /**
   * For each unload hook registered, run them during destroy()
   */
  private[http] def runUnloadHooks() {
    unloadHooks.toList.foreach{f =>
      tryo{f()}
    }
  }

  /**
   * The maximum allowed size of a complete mime multi-part POST.  Default 8MB
   */
  @volatile var maxMimeSize: Long = 8 * 1024 * 1024

  /**
   * Should pages that are not found be passed along the request processing chain to the
   * next handler outside Lift?
   */
  @volatile var passNotFoundToChain = false

  /**
   * The maximum allowed size of a single file in a mime multi-part POST.
   * Default 7MB
   */
  @volatile var maxMimeFileSize: Long = 7 * 1024 * 1024

  /**
   * The function referenced here is called if there's a localization lookup failure
   */
  @volatile var localizationLookupFailureNotice: Box[(String, Locale) => Unit] = Empty

  /**
   * When a parameter is received either via POST or GET and does not have a
   * corresponding mapping on the server, the function provided by this
   * FactoryMaker will be called with the req and parameter name.
   *
   * By default, if the parameter looks Lift-like (i.e., it starts with an F),
   * then we log a warning with the given parameter name and URI.
   */
  val handleUnmappedParameter = new FactoryMaker[(Req,String)=>Unit](
    () => { (req: Req, parameterName: String) =>
      if (parameterName.startsWith("F"))
        logger.warn("Unmapped Lift-like parameter seen in request [%s]: %s".format(req.uri, parameterName))
    }
  ) {}

  /**
   * Set to false if you want to have 404's handled the same way in dev and production mode
   */
  @volatile var displayHelpfulSiteMapMessages_? = true

  /**
   * The default location to send people if SiteMap access control fails. The path is
   * expressed a a List[String]
   */
  @volatile var siteMapFailRedirectLocation: List[String] = List()

  private[http] def notFoundOrIgnore(requestState: Req, session: Box[LiftSession]): Box[LiftResponse] = {
    if (passNotFoundToChain) Empty
    else session match {
      case Full(session) => Full(session.checkRedirect(requestState.createNotFound))
      case _ => Full(requestState.createNotFound)
    }
  }

  /**
   * Allows user adding additional Lift tags (the tags must be prefixed by lift namespace such as <lift:xxxx/>).
   * Each LiftTagPF function will be called with the following parameters:
   * <pre>
   *  - Element label,
   *  - The Element itselft,
   *  - The attributes
   *  - The child nodes
   *  - The page name
   * </pre>
   */
  val liftTagProcessing = RulesSeq[LiftTagPF]

  /**
   * If you don't want lift to send the application/xhtml+xml mime type to those browsers
   * that understand it, then set this to  { @code false }
   */
  @volatile var useXhtmlMimeType: Boolean = true


  private def _stringToXml(s: String): NodeSeq = Text(s)

  /**
   * A function that defines how a String should be converted to XML
   * for the localization stuff.  By default, Text(s) is returned,
   * but you can change this to attempt to parse the XML in the String and
   * return the NodeSeq.
   */
  @volatile var localizeStringToXml: String => NodeSeq = _stringToXml _

  /**
   * The base name of the resource bundle
   */
  @volatile var resourceNames: List[String] = List("lift")

  /**
   * This function is called to convert the current set of Notices into
   * a JsCmd that will be executed on the client to display the Notices.
   *
   * @see net.liftweb.builtin.snippet.Msgs
   */
  @volatile var noticesToJsCmd: () => JsCmd = () => {
    import builtin.snippet.{Msg,Msgs,MsgErrorMeta,MsgNoticeMeta,MsgWarningMeta}

    // A "wrapper" that simply returns the javascript
    val passJs = (in : JsCmd) => in

    // Delegate to Msgs for fadeout and effects
    def noticesFadeOut(noticeType: NoticeType.Value): JsCmd =
      Msgs.noticesFadeOut(noticeType, Noop, passJs)

    def groupEffects(noticeType: NoticeType.Value): JsCmd =
      Msgs.effects(Full(noticeType), noticeType.id, Noop, passJs)

    def idEffects(id : String): JsCmd =
      Msgs.effects(Empty, id, Noop, passJs)

    // Compute the global notices first
    val groupMessages = Msgs.renderNotices() match {
      case NodeSeq.Empty => JsCmds.Noop
      case xml => LiftRules.jsArtifacts.setHtml(LiftRules.noticesContainerId, xml) &
        noticesFadeOut(NoticeType.Notice) &
        noticesFadeOut(NoticeType.Warning) &
        noticesFadeOut(NoticeType.Error) &
        groupEffects(NoticeType.Notice) &
        groupEffects(NoticeType.Warning) &
        groupEffects(NoticeType.Error)
    }

    // We need to determine the full set of IDs that need messages rendered.
    val idSet = (S.idMessages((S.errors)) ++
                 S.idMessages((S.warnings)) ++
                 S.idMessages((S.notices))).map(_._1).distinct

    // Merge each Id's messages and effects into the JsCmd chain
    idSet.foldLeft(groupMessages) {
      (chain,id) => chain &
        LiftRules.jsArtifacts.setHtml(id, Msg.renderIdMsgs(id)) &
        idEffects(id)
    }
  }

  /**
   * The base name of the resource bundle of the lift core code
   */
  @volatile var liftCoreResourceName = "i18n.lift-core"

  /**
   * The JsCmd to execute when the comet session is lost. The comet
   * session is considered lost when either (a) a comet request comes
   * in for a session that does not exist on the server or (b) a comet
   * request comes in for a session that has no associated comet actors
   * (this typically happens when the server restarts).
   *
   * By default, we invoke lift.cometOnSessionLost, which can be
   * overridden client-side for more complex work.
   * lift.cometOnSessionLost reloads the current page by default.
   */
  val noCometSessionCmd = new FactoryMaker[JsCmd](
    () => JsCmds.Run("lift.cometOnSessionLost()")
  ) {}

  /**
   * The JsCmd to execute when the ajax session is lost. The ajax
   * session is considered lost when either an ajax request comes in for
   * a session that does not exist on the server.
   *
   * By default, we invoke lift.ajaxOnSessionLost, which can be
   * overridden client-side for more complex work.
   * lift.ajaxOnSessionLost reloads the page by default.
   */
  val noAjaxSessionCmd = new FactoryMaker[JsCmd](
    () => JsCmds.Run("lift.ajaxOnSessionLost()")
  ) {}

  /**
   * Server-side actors that represent client-side
   * actor endpoints (client actors, Round Trips) need
   * a lifespan. By default, it's 60 seconds, but you might
   * want to make it longer if the client is going to get
   * delayed by long computations that bar it from re-establishing
   * the long polling connection
   */
  val clientActorLifespan = new FactoryMaker[LiftActor => Long](
    () => (actor: LiftActor) => (30.minutes): Long
  ){}

  /**
   * Put a function that will calculate the request timeout based on the
   * incoming request.
   */
  @volatile var calcRequestTimeout: Box[Req => Int] = Empty

  /**
   * If you want the standard (non-AJAX) request timeout to be something other than
   * 10 seconds, put the value here
   */
  @volatile var stdRequestTimeout: Box[Int] = Empty

  /**
   * If you want the AJAX request timeout to be something other than 120 seconds, put the value here
   */
  @volatile var cometRequestTimeout: Box[Int] = Empty

  /**
   * If a Comet request fails timeout for this period of time. Default value is 10 seconds
   */
  @volatile var cometFailureRetryTimeout: Long = 10.seconds

  /**
   * The timeout in milliseconds of a comet ajax-request. Defaults to 5000 ms.
   */
  @volatile var cometProcessingTimeout: Long = 5.seconds

  /**
   * The timeout in milliseconds of a comet render-request. Defaults to 30000 ms.
   */
  @volatile var cometRenderTimeout: Long = 30.seconds

  /**
   * The dispatcher that takes a Snippet and converts it to a
   * DispatchSnippet instance
   */
  val snippetDispatch = RulesSeq[SnippetDispatchPF]


  /**
   * Function that generates variants on snippet names to search for, given the name from the template.
   * The default implementation just returns name :: Nil (e.g. no change).
   * The names are searched in order.
   * See also searchSnippetsWithRequestPath for an implementation.
   */
  @volatile var snippetNamesToSearch: FactoryMaker[String => List[String]] =
      new FactoryMaker(() => (name: String) => name :: Nil) {}

  /**
   * Implementation for snippetNamesToSearch that looks first in a package named by taking the current template path.
   * For example, suppose the following is configured in Boot:
   *   LiftRules.snippetNamesToSearch.default.set(() => LiftRules.searchSnippetsWithRequestPath)
   *   LiftRules.addToPackages("com.mycompany.myapp")
   *   LiftRules.addToPackages("com.mycompany.mylib")
   * The tag <lift:MySnippet> in template foo/bar/baz.html would search for the snippet in the following locations:
   *   - com.mycompany.myapp.snippet.foo.bar.MySnippet
   *   - com.mycompany.myapp.snippet.MySnippet
   *   - com.mycompany.mylib.snippet.foo.bar.MySnippet
   *   - com.mycompany.mylib.snippet.MySnippet
   *   - and then the Lift builtin snippet packages
   */
  def searchSnippetsWithRequestPath(name: String): List[String] =
    S.request.map(_.path.partPath.dropRight(1)) match {
      case Full(xs) if !xs.isEmpty => (xs.mkString(".") + "." + name) :: name :: Nil
      case _ => name :: Nil
    }

  /**
   * Change this variable to set view dispatching
   */
  val viewDispatch = RulesSeq[ViewDispatchPF]

  private[http] def snippet(name: String): Box[DispatchSnippet] = NamedPF.applyBox(name, snippetDispatch.toList)

  /**
   * If the request times out (or returns a non-Response) you can
   * intercept the response here and create your own response
   */
 @volatile  var requestTimedOut: Box[(Req, Any) => Box[LiftResponse]] = Empty

  /**
   * A function that takes the current HTTP request and returns the current
   */
  @volatile var timeZoneCalculator: Box[HTTPRequest] => TimeZone = defaultTimeZoneCalculator _

  def defaultTimeZoneCalculator(request: Box[HTTPRequest]): TimeZone = TimeZone.getDefault

  /**
   * How many times do we retry an Ajax command before calling it a failure?
   */
  @volatile var ajaxRetryCount: Box[Int] = Empty

  /**
   * The JavaScript to execute at the beginning of an
   * Ajax request (for example, showing the spinning working thingy)
   */
  @volatile var ajaxStart: Box[() => JsCmd] = Empty

  import FuncJBridge._

  /**
   * Set the Ajax end JavaScript function.  The
   * Java-callable alternative to assigning the var ajaxStart
   */
  def setAjaxStart(f: Func0[JsCmd]): Unit = {
    ajaxStart = Full(f: () => JsCmd)
  }


  /**
   * The function that calculates if the response should be rendered in
   * IE6/7/8 compatibility mode
   */
  @volatile var calcIEMode: () => Boolean =
  () => (for (r <- S.request) yield r.isIE6 || r.isIE7 ||
          r.isIE8) openOr true

  /**
   * The JavaScript to execute to log a message on the client side when
   * lift.logError is called.
   *
   * If Empty no logging is performed
   * The default when running in DevMode is to call lift.logError which
   * will use JavaScript console if available or alert otherwise.
   *
   * To always use alert set:
   *
   *   LiftRules.jsLogFunc = Full(v => JE.Call("alert",v).cmd)
   */
  @volatile var jsLogFunc: Box[JsVar => JsCmd] =
    if (Props.devMode) Full(v => JE.Call("lift.logError", v))
    else Empty

  /**
   * The JavaScript to execute at the end of an
   * Ajax request (for example, removing the spinning working thingy)
   */
  @volatile var ajaxEnd: Box[() => JsCmd] = Empty

  /**
   * Set the Ajax end JavaScript function.  The
   * Java-callable alternative to assigning the var ajaxEnd
   */
  def setAjaxEnd(f: Func0[JsCmd]): Unit = {
    ajaxEnd = Full(f: () => JsCmd)
  }

  /**
   * An XML header is inserted at the very beginning of returned XHTML pages.
   * This function defines the cases in which such a header is inserted.  The
   * function takes a NodeResponse (the LiftResponse that is converting the
   * XML to a stream of bytes), the Node (root node of the XML), and
   * a Box containing the content type.
   */
  @volatile var calculateXmlHeader: (NodeResponse, Node, Box[String]) => String = {
    case _ if S.skipXmlHeader => ""
    case (_, up: Unparsed, _) => ""

    case (_, _, Empty) | (_, _, Failure(_, _, _)) =>
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

    case (_, _, Full(s)) if (s.toLowerCase.startsWith("text/html")) =>
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

    case (_, _, Full(s)) if (s.toLowerCase.startsWith("text/xml") ||
        s.toLowerCase.startsWith("text/xhtml") ||
        s.toLowerCase.startsWith("application/xml") ||
        s.toLowerCase.startsWith("application/xhtml+xml")) =>
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

    case _ => ""
  }

  /**
   * The default action to take when the JavaScript action fails
   */
  @volatile var ajaxDefaultFailure: Box[() => JsCmd] =
  Full(() => JsCmds.Alert(S.?("ajax.error")))

  /**
   * A function that takes the current HTTP request and returns the current
   */
  @volatile var localeCalculator: Box[HTTPRequest] => Locale = defaultLocaleCalculator _

  def defaultLocaleCalculator(request: Box[HTTPRequest]) =
    request.flatMap(_.locale).openOr(Locale.getDefault())

  val resourceBundleFactories = RulesSeq[ResourceBundleFactoryPF]

  /**
   * Given the current location (based on the Req.path.partPath),
   * what are the resource bundles in the templates for the current
   * page.
   *
   * @see DefaultRoutines.resourceForCurrentLoc()
   */
  val resourceForCurrentLoc: FactoryMaker[() => List[ResourceBundle]] =
    new FactoryMaker(() => () => DefaultRoutines.resourceForCurrentReq()) {}


  /**
   * Ever wanted to add custom attribute processing to Lift? Here's your chance.
   * Every attribute with the data- prefix will be tested against this
   * RulesSeq and if there's a match, then use the rule process. Simple, easy, cool.
   */
  val dataAttributeProcessor: RulesSeq[DataAttributeProcessor] = new RulesSeq()

  /**
   * Ever wanted to match on *any* arbitrary tag in your HTML and process it
   * any way you wanted? Well, here's your chance, dude. You can capture any
   * tag and do anything you want with it.
   *
   * Note that this set of PartialFunctions is run for **EVERY** node
   * in the DOM so make sure it runs *FAST*.
   *
   * Also, no subsequent processing of the returned NodeSeq is done (no
   * LiftSession.processSurroundAndInclude()) so evaluate everything
   * you want to.
   *
   * But do avoid infinite loops, so make sure the PartialFunction actually
   * returns true *only* when you're going to return a modified node.
   *
   * An example might be:
   *
   *
   *    case ("script", e, session) if e.getAttribute("data-serverscript").isDefined => ...
   */
  val tagProcessor: RulesSeq[TagProcessor] = new RulesSeq()


  /**
  * There may be times when you want to entirely control the templating process.  You can insert
  * a function to this factory that will do your custom template resolution.  If the PartialFunction
  * isDefinedAt the given locale/path, then that's the template returned.  In this way, you can
  * return Empty for a template that's not found and the template will not be found.  Otherwise,
  * if the function is not defined for the locale/path pair, the normal templating system will
  * be used.  Also, keep in mind how FactoryMaker can be used... it can be global, per request, etc.
  */
  val externalTemplateResolver: FactoryMaker[() => PartialFunction[(Locale, List[String]), Box[NodeSeq]]] =
  new FactoryMaker(() => (() => Map.empty: PartialFunction[(Locale, List[String]), Box[NodeSeq]])) {}

  /**
  * There may be times when you want to entirely control the templating process.  You can insert a function
  * that creates a white list of snippets.  The white list is the exhaustive list of snippets.  The
  * snippets are class/method pairs.  If the partial function is defined and the result is a Full Box,
  * the function is run.  If the Box is an EmptyBox, then the result is a snippet lookup failure.  If the
  * partial function is not defined, then the normal snippet resolution mechanism is used.  Please note that
  * in Scala a Map is PartialFunction and you can create Maps that have a default value using the withDefaultValue
  * method.
  */
  val snippetWhiteList: FactoryMaker[() => PartialFunction[(String, String), Box[NodeSeq => NodeSeq]]] =
  new FactoryMaker(() => (() => Map.empty: PartialFunction[(String, String), Box[NodeSeq => NodeSeq]])) {}

  /**
  * This FactoryMaker can be used to disable the little used attributeSnippets
  */
  val allowAttributeSnippets: FactoryMaker[() => Boolean] =
  new FactoryMaker(() => () => true) {}

  private var _sitemap: Box[SiteMap] = Empty

  private var sitemapFunc: Box[() => SiteMap] = Empty

  private object sitemapRequestVar extends TransientRequestVar(resolveSitemap())

  /**
  * Set the sitemap to a function that will be run to generate the sitemap.
  *
  * This allows for changing the SiteMap when in development mode and having
  * the function re-run for each request.<br/>
  *
  * This is **NOT** a mechanism for dynamic SiteMap.  This is a mechanism
  * **ONLY** for allowing you to change the SiteMap during development.
  * There will be significant performance penalties (serializing the
  * service of requests... only one at a time) for changing the SiteMap.
  */
  def setSiteMapFunc(smf: () => SiteMap) {
    sitemapFunc = Full(smf)
    if (!Props.devMode) {
      resolveSitemap()
    }
  }

  /**
   * Define the sitemap.
   */
  def setSiteMap(sm: SiteMap) {
    this.setSiteMapFunc(() => sm)
  }

  private def runAsSafe[T](f: => T): T = synchronized {
     val old = _doneBoot
     try {
        _doneBoot = false
        f
     } finally {
        _doneBoot = old
     }
  }

  private case class PerRequestPF[A, B](f: PartialFunction[A, B]) extends PartialFunction[A, B] {
    def isDefinedAt(a: A) = f.isDefinedAt(a)
    def apply(a: A) = f(a)
  }

  private def resolveSitemap(): Box[SiteMap] = {
    this.synchronized {
      runAsSafe {
        sitemapFunc.flatMap {
          smf =>

          LiftRules.statefulRewrite.remove {
            case PerRequestPF(_) => true
            case _ => false
          }

          val sm = smf()
          _sitemap = Full(sm)
          for (menu <- sm.menus;
               loc = menu.loc;
               rewrite <- loc.rewritePF) LiftRules.statefulRewrite.append(PerRequestPF(rewrite))

          _sitemap
        }
      }
    }
  }

  /**
   * Return the sitemap if set in Boot.  If the current runMode is development
   * mode, the sitemap may be recomputed on each page load.
   */
  def siteMap: Box[SiteMap] = if (Props.devMode) {
    this.synchronized {
      sitemapRequestVar.is
    }
  } else _sitemap

  /**
   * A unified set of properties for managing how to treat
   * HTML, XHTML, HTML5.  The default behavior is to return an
   * OldHtmlPropteries instance, but you can change this
   * to return an Html5Properties instance any you'll get
   * HTML5 support.
   * LiftRules.htmlProperties.default.set((r: Req) => new Html5Properties(r.userAgent))
   */
  val htmlProperties: FactoryMaker[Req => HtmlProperties] =
    new FactoryMaker(() => (r: Req) => new Html5Properties(r.userAgent): HtmlProperties) {}

  /**
   * How long should we wait for all the lazy snippets to render
   */
  val lazySnippetTimeout: FactoryMaker[TimeSpan] = new FactoryMaker(() => 30.seconds) {}

  /**
   * Does the current context support parallel snippet execution
   */
  val allowParallelSnippets: FactoryMaker[Boolean] = new FactoryMaker(() => false) {}

  /**
   * Update the function here that calculates particular paths to
   * excluded from context path rewriting
   */
  val excludePathFromContextPathRewriting: FactoryMaker[String => Boolean] =
  new FactoryMaker(() => ((s: String) => false)) {}

  /**
   * If a deferred snippet has a failure during render,
   * what should we display?
   */
  val deferredSnippetFailure: FactoryMaker[Failure => NodeSeq] =
  new FactoryMaker(() => {
    failure: Failure => {
      if (Props.devMode)
        <div style="border: red solid 2px">A lift:parallel snippet failed to render.Message:{failure.msg}{failure.exception match {
          case Full(e) =>
            <pre>{e.getStackTrace.map(_.toString).mkString("\n")}</pre>
          case _ => NodeSeq.Empty
        }}<i>note: this error is displayed in the browser because
        your application is running in "development" mode.If you
        set the system property run.mode=production, this error will not
        be displayed, but there will be errors in the output logs.
        </i>
        </div>
      else NodeSeq.Empty
    }
  }) {}

  /**
   * If a deferred snippet has a failure during render,
   * what should we display?
   */

  val deferredSnippetTimeout: FactoryMaker[NodeSeq] =
  new FactoryMaker(() => {
        if (Props.devMode)
        <div style="border: red solid 2px">
          A deferred snippet timed out during render.

          <i>note: this error is displayed in the browser because
            your application is running in "development" mode.  If you
            set the system property run.mode=production, this error will not
            be displayed, but there will be errors in the output logs.
          </i>
        </div>
        else NodeSeq.Empty
      }) {}


  /**
   * Should comments be stripped from the served XHTML
   */
  val stripComments: FactoryMaker[Boolean] =
  new FactoryMaker(() => {
        if (Props.devMode)
        false
        else true
      }) {}



  private[http] val reqCnt = new AtomicInteger(0)

  @volatile private[http] var ending = false

  private[http] def bootFinished() {
    _doneBoot = true
  }

  /**
   * Holds user's DispatchPF functions that will be executed in a stateless context. This means that
   * no session will be created and no JSESSIONID cookie will be presented to the user (unless
   * the user has presented a JSESSIONID cookie).
   *
   * This is the way to do stateless REST in Lift
   */
  val statelessDispatch = RulesSeq[DispatchPF]

  /**
   * Add functionality around all of the HTTP request/response cycle.
   * This is an optimal place to get a database connection.  Note that whatever
   * is loaned at the beginning of the request will not be returned until the end
   * of the request.  It's super-important to (1) not do anything related
   * to state or touch the request objects or anything else at the beginning or
   * end of the loan wrapper phase; (2) make sure that your code does not throw
   * exceptions as exceptions can cause major problems.
   */
  val allAround = RulesSeq[LoanWrapper]


  private[http] def dispatchTable(req: HTTPRequest): List[DispatchPF] = {
    req match {
      case null => dispatch.toList
      case _ => SessionMaster.getSession(req, Empty) match {
        case Full(s) => S.initIfUninitted(s) {
          S.highLevelSessionDispatchList.map(_.dispatch) :::
                  dispatch.toList
        }
        case _ => dispatch.toList
      }
    }
  }

  /**
   * Contains the URI path under which all built-in Lift-handled requests are
   * scoped. It does not include the context path and should not begin with a
   * /.
   */
  @volatile var liftContextRelativePath = "lift"

  /**
    * Returns a complete URI, including the context path, under which all
    * built-in Lift-handled requests are scoped.
    */
  def liftPath: String = S.contextPath + "/" + liftContextRelativePath

  /**
   * If there is an alternative way of calculating the context path
   * (by default returning Empty)
   *
   * If this function returns an Empty, the contextPath provided by the container will be used.
   *
   */
  @volatile var calculateContextPath: () => Box[String] = () => Empty


  @volatile private var _context: HTTPContext = _

  /**
   * Should an exception be thrown on out of scope Session and RequestVar
   * access.  By default, no.
   */
  @volatile var throwOnOutOfScopeVarAccess: Boolean = false

  /**
   * In Dev mode and Test mode, return a non-200 response code
   * if there is an error on the page (one that would result in
   * the red box with the error message being displayed).  This
   * helps in testing automation.
   */
  @volatile var devModeFailureResponseCodeOverride: Box[Int] = Empty

  /**
   * Returns the HTTPContext
   */
  def context: HTTPContext = synchronized {_context}

  /**
   * Sets the HTTPContext
   */
  def setContext(in: HTTPContext): Unit = synchronized {
    if (in ne _context) {
      _context = in
    }
  }

  private var otherPackages: List[String] = Nil

  /**
   * Used by Lift to construct full package names from the packages provided to addToPackages function
   */
  def buildPackage(end: String) = otherPackages.map(_ + "." + end)

  /**
   * Tells Lift where to find Snippets,Views, Comet Actors and Lift ORM Model object
   */
  def addToPackages(what: String) {
    if (doneBoot) throw new IllegalStateException("Cannot modify after boot.");
    otherPackages = what :: otherPackages
  }

  /**
   * Tells Lift where to find Snippets, Views, Comet Actors and Lift ORM Model object
   */
  def addToPackages(what: Package) {
    if (doneBoot) throw new IllegalStateException("Cannot modify after boot.");
    otherPackages = what.getName :: otherPackages
  }

  private val defaultFinder = getClass.getResource _

  private def resourceFinder(name: String): java.net.URL = if (null eq _context) null else _context.resource(name)

  /**
   * Obtain the resource URL by name
   */
  @volatile var getResource: String => Box[java.net.URL] = defaultGetResource _

  /**
   * Obtain the resource URL by name
   */
  def defaultGetResource(name: String): Box[java.net.URL] =
    for{
      rf <- (Box !! resourceFinder(name)) or (Box !! defaultFinder(name))
    } yield rf

  /**
   * Open a resource by name and process its contents using the supplied function.
   */
  def doWithResource[T](name: String)(f: InputStream => T): Box[T] =
    getResource(name) map { _.openStream } map { is => try { f(is) } finally { is.close } }

  /**
   * Obtain the resource as an array of bytes by name
   */
  def loadResource(name: String): Box[Array[Byte]] = doWithResource(name) { stream =>
    val buffer = new Array[Byte](2048)
    val out = new ByteArrayOutputStream
    def reader {
      val len = stream.read(buffer)
      if (len < 0) return
      else if (len > 0) out.write(buffer, 0, len)
      reader
    }
    reader
    out.toByteArray
  }

  /**
   * Obtain the resource as an XML by name. If you're using this to load a template, consider using
   * the Template object instead.
   *
   * @see Template
   */
  def loadResourceAsXml(name: String): Box[NodeSeq] = loadResourceAsString(name).flatMap(s => PCDataXmlParser(s))

  /**
   * Obtain the resource as a String by name
   */
  def loadResourceAsString(name: String): Box[String] = loadResource(name).map(s => new String(s, "UTF-8"))

  /**
   * Get the partial function that defines if a request should be handled by
   * the application (rather than the default container handler)
   */
  val liftRequest = RulesSeq[LiftRequestPF]

  /**
   * Holds the user's DispatchPF functions that will be executed in stateful context
   */
  val dispatch = RulesSeq[DispatchPF].append(LiftJavaScript.servePageJs)

  /**
   * Holds the user's rewrite functions that can alter the URI parts and query parameters.  This rewrite
   * is performed very early in the HTTP request cycle and may not include any state.  This rewrite is meant
   * to rewrite requests for statelessDispatch. <br/>
   * Note also that rewrites should not have side effects except
   * to memoize database query results.  No side effects means that you should not change SessionVars
   * in a rewrite.
   */
  val statelessRewrite = RulesSeq[RewritePF]

  /**
   *  Holds the user's rewrite functions that can alter the URI parts and query parameters.
   * This rewrite takes place within the scope of the S state so SessionVars and other session-related
   * information is available. <br/>
   * Note also that rewrites should not have side effects except
   * to memoize database query results.  No side effects means that you should not change SessionVars
   * in a rewrite. <br/>
   * In general, rewrites should be considered low level access.  Rather than using a rewrite to extract
   * parameters out of a URL, you'll be much better off using SiteMap generally and Menu.param and Menu.params
   * specifically for extracting parameters from URLs.
   */
  val statefulRewrite = RulesSeq[RewritePF]

  /**
   * Holds the user's snippet functions that will be executed by lift given a certain path.
   */
  val snippets = RulesSeq[SnippetPF]

  /**
   * Execute certain functions early in a Stateful Request
   * This is called early in a stateful request (one that's not serviced by a stateless REST request and
   * one that's not marked as a stateless HTML page request).
   * @dpp strongly recommends that everything that you do related to user state is done with earlyInStateful,
   * instead of using onBeginServicing.
   */
  val earlyInStateful = RulesSeq[Box[Req] => Unit]

  /**
   * Execute certain functions early in a Stateful Request
   */
  val earlyInStateless = RulesSeq[Box[Req] => Unit]


  private var _configureLogging: () => Unit = _

  /**
   * Holds the function that configures logging. Must be set before any loggers are created
   */
  def configureLogging: () => Unit = _configureLogging

  /**
   * Holds the function that configures logging. Must be set before any loggers are created
   */
  def configureLogging_=(newConfigurer: () => Unit): Unit = {
    _configureLogging = newConfigurer
    Logger.setup = Full(newConfigurer)
  }

  configureLogging = net.liftweb.util.LoggingAutoConfigurer()

  private val _cometLogger: FatLazy[Logger] = FatLazy({
    val ret = Logger("comet_trace")
    ret
  })

  /**
   * Holds the CometLogger that will be used to log comet activity
   */
  def cometLogger: Logger = _cometLogger.get

  /**
   * Holds the CometLogger that will be used to log comet activity
   */
  def cometLogger_=(newLogger: Logger): Unit = _cometLogger.set(newLogger)


  /**
   * Sometimes the comet logger (which is really the Ajax logger)
   * needs to have the string cleaned up to remove stuff like passwords. That's
   * done by this function.
   */
  @volatile var cometLoggerStringSecurer: String => String = s => s

  /**
   * Takes a Node, headers, cookies, and a session and turns it into an XhtmlResponse.
   */
  private def cvt(ns: Node, headers: List[(String, String)], cookies: List[HTTPCookie], req: Req, code:Int) =
    convertResponse({
      val ret = XhtmlResponse(ns,
        /*LiftRules.docType.vend(req)*/S.htmlProperties.docType,
        headers, cookies, code,
        S.legacyIeCompatibilityMode)
      ret._includeXmlVersion = !S.skipDocType
      ret
    }, headers, cookies, req)

  @volatile var defaultHeaders: PartialFunction[(NodeSeq, Req), List[(String, String)]] = {
    case _ =>
      val d = Helpers.nowAsInternetDate
      List("Expires" -> d,
           "Date" -> d,
           "Cache-Control" ->
           "no-cache, private, no-store",
           "Pragma" -> "no-cache" )
  }

  /**
   * Runs responseTransformers
   */
  def performTransform(in: LiftResponse): LiftResponse = responseTransformers.toList.foldLeft(in) {
    case (in, pf: PartialFunction[_, _]) =>
      if (pf.isDefinedAt(in)) pf(in) else in
    case (in, f) => f(in)
  }

  /**
   * Holds the user's transformer functions allowing the user to modify a LiftResponse before sending it to client.
   */
  val responseTransformers = RulesSeq[LiftResponse => LiftResponse]


  /**
   * convertResponse is a PartialFunction that reduces a given Tuple4 into a
   * LiftResponse that can then be sent to the browser.
   */
  var convertResponse: PartialFunction[(Any, List[(String, String)], List[HTTPCookie], Req), LiftResponse] = {
    case (r: LiftResponse, _, _, _) => r
    case (ns: Group, headers, cookies, req) => cvt(ns, headers, cookies, req, 200)
    case (ns: Node, headers, cookies, req) => cvt(ns, headers, cookies, req, 200)
    case (ns: NodeSeq, headers, cookies, req) => cvt(Group(ns), headers, cookies, req, 200)
    case ((ns: NodeSeq, code: Int), headers, cookies, req) => cvt(Group(ns), headers, cookies, req, code)
    case (SafeNodeSeq(n), headers, cookies, req) => cvt(Group(n), headers, cookies, req, 200)

    case (Full(o), headers, cookies, req) => convertResponse((o, headers, cookies, req))

    case (Some(o), headers, cookies, req) => convertResponse((o, headers, cookies, req))
    case (bad, _, _, req) => req.createNotFound
  }

  /**
   * Set a snippet failure handler here.  The class and method for the snippet are passed in
   */
  val snippetFailedFunc = RulesSeq[SnippetFailure => Unit].prepend(logSnippetFailure _)

  private def logSnippetFailure(sf: SnippetFailure) = logger.info("Snippet Failure: " + sf)

  /**
   * Set to false if you do not want ajax/comet requests that are not
   * associated with a session to call their respective session
   * loss handlers (set via LiftRules.noAjaxSessionCmd and
   * LiftRules.noCometSessionCmd).
   */
  @volatile var redirectAsyncOnSessionLoss = true

  /**
   * The sequence of partial functions (pattern matching) for handling converting an exception to something to
   * be sent to the browser depending on the current RunMode (development, etc.)
   *
   * By default it returns an XhtmlResponse containing a predefined markup. You can overwrite this by calling
   * LiftRules.exceptionHandler.prepend(...). If you are calling append then your code will not be called since
   * a default implementation is already appended.
   *
   */
  val exceptionHandler = RulesSeq[ExceptionHandlerPF].append {
    case (Props.RunModes.Development, r, e) =>
      logger.error("Exception being returned to browser when processing " + r.uri.toString, e)
      XhtmlResponse((<html> <body>Exception occured while processing {r.uri}<pre>{showException(e)}</pre> </body> </html>), S.htmlProperties.docType, List("Content-Type" -> "text/html; charset=utf-8"), Nil, 500, S.legacyIeCompatibilityMode)

    case (_, r, e) =>
      logger.error("Exception being returned to browser when processing " + r.uri.toString, e)
      XhtmlResponse((<html> <body>Something unexpected happened while serving the page at {r.uri}</body> </html>), S.htmlProperties.docType, List("Content-Type" -> "text/html; charset=utf-8"), Nil, 500, S.legacyIeCompatibilityMode)
  }

  /**
   * The list of partial function for defining the behavior of what happens when
   * URI is invalid and you're not using a site map
   *
   */
  val uriNotFound = RulesSeq[URINotFoundPF].prepend(NamedPF("default") {
    case (r, _) => DefaultNotFound
  })

  /**
   * If you use the form attribute in a snippet invocation, what attributes should
   * be copied from the snippet invocation tag to the form tag.  The
   * default list is "class", "id", "target", "style", "onsubmit"
   */
  val formAttrs: FactoryMaker[List[String]] = new FactoryMaker(() => List("class", "id", "target", "style", "onsubmit")) {}

  /**
   * By default, Http response headers are appended.  However, there are
   * some headers that should only appear once (for example "expires").  This
   * Vendor vends the list of header responses that can only appear once.
   */
  val overwrittenReponseHeaders: FactoryMaker[List[String]] = new FactoryMaker(() => List("expires")) {}

  /**
   * A utility method to convert an exception to a string of stack traces
   * @param le the exception
   *
   * @return the stack trace
   */
  private def showException(le: Throwable): String = {
    val ret = "Message: " + le.toString + "\n\t" +
            le.getStackTrace.map(_.toString).mkString("\n\t") + "\n"

    val also = le.getCause match {
      case null => ""
      case sub: Throwable => "\nCaught and thrown by:\n" + showException(sub)
    }

    ret + also
  }

  /**
   * Modifies the root relative paths from the css url-s
   *
   * @param path - the path of the css resource
   * @prefix - the prefix to be added on the root relative paths. If this is Empty
   * 	       the prefix will be the application context path.
   */
  def fixCSS(path: List[String], prefix: Box[String]) {

    val liftReq: LiftRules.LiftRequestPF = new LiftRules.LiftRequestPF {
      def functionName = "Default CSS Fixer"

      def isDefinedAt(r: Req): Boolean = {
        r.path.partPath == path
      }

      def apply(r: Req): Boolean = {
        r.path.partPath == path
      }
    }

    val cssFixer: LiftRules.DispatchPF = new LiftRules.DispatchPF {
      def functionName = "default css fixer"

      def isDefinedAt(r: Req): Boolean = {
        r.path.partPath == path
      }

      def apply(r: Req): () => Box[LiftResponse] = {
        val cssPath = path.mkString("/", "/", ".css")
        val css = LiftRules.loadResourceAsString(cssPath);

        () => {
          css.map(str => CSSHelpers.fixCSS(new BufferedReader(
            new StringReader(str)), prefix openOr (S.contextPath)) match {
            case (Full(c), _) => CSSResponse(c)
            case (x, input) => {
              logger.info("Fixing " + cssPath + " failed with result %s".format(x));
              CSSResponse(input)
            }
          })
        }
      }
    }
    LiftRules.dispatch.prepend(cssFixer)
    LiftRules.liftRequest.append(liftReq)
  }

  /**
   * Holds user function hooks when the request is about to be processed
   * It's legacy from when Lift was a lot more Rails-like. It's called literally at the very
   * beginning of the servicing of the HTTP request.
   * The S scope is not available nor is the DB connection available in onBeginServicing.
   * We recommend using earlyInStateful.
   */
  val onBeginServicing = RulesSeq[Req => Unit]

  val preAccessControlResponse_!! = new RulesSeq[Req => Box[LiftResponse]] with FirstBox[Req, LiftResponse]

  val earlyResponse = new RulesSeq[Req => Box[LiftResponse]] with FirstBox[Req, LiftResponse]

  /**
   * Holds user function hooks when the request was processed
   */
  val onEndServicing = RulesSeq[(Req, Box[LiftResponse]) => Unit]

  /**
   * Tells Lift if the Comet JavaScript should be included. By default it is set to true.
   */
  @volatile var autoIncludeComet: LiftSession => Boolean = session => true

  val autoIncludeAjaxCalc: FactoryMaker[() => LiftSession => Boolean] =
  new FactoryMaker(() => () => (session: LiftSession) => true) {}

  /**
   * Tells Lift which JavaScript settings to use. If Empty, does not
   * include the JS settings.
   */
  val javaScriptSettings: FactoryMaker[() => Box[LiftSession => JsObj]] =
  new FactoryMaker(() => () => (Full((session: LiftSession) => LiftJavaScript.settings): Box[LiftSession => JsObj])) {}

  /**
   * Define the XHTML validator
   */
  @volatile var xhtmlValidator: Box[XHtmlValidator] = Empty // Full(TransitionalXHTML1_0Validator)

  @volatile var ajaxPostTimeout = 5000

  @volatile var cometGetTimeout = 140000

  /**
   * Compute the headers to be sent to the browser in addition to anything else that's sent
   */
  val supplementalHeaders: FactoryMaker[List[(String, String)]] = new FactoryMaker(() => List(("X-Lift-Version", liftVersion), ("X-Frame-Options", "SAMEORIGIN"))) {}

  @volatile var calcIE6ForResponse: () => Boolean = () => S.request.map(_.isIE6) openOr false

  @volatile var flipDocTypeForIE6 = true

  /**
   * By default lift uses a garbage-collection mechanism of removing unused bound functions from LiftSesssion.
   * Setting this to false will disable this mechanisms and there will be no Ajax polling requests attempted.
   */
  @volatile var enableLiftGC = true;

  /**
   * If Lift garbage collection is enabled, functions that are not seen in the page for this period of time
   * (given in milliseconds) will be discarded, hence eligible for garbage collection.
   * The default value is 10 minutes.
   */
  @volatile var unusedFunctionsLifeTime: Long = 10.minutes

  /**
   * The polling interval for background Ajax requests to prevent functions of being garbage collected.
   * Default value is set to 75 seconds.
   */
  @volatile var liftGCPollingInterval: Long = 75.seconds

  /**
   * Put a test for being logged in into this function
   */
  @volatile var loggedInTest: Box[() => Boolean] = Empty

  /**
   * The polling interval for background Ajax requests to keep functions to not be garbage collected.
   * This will be applied if the Ajax request will fail. Default value is set to 15 seconds.
   */
  @volatile var liftGCFailureRetryTimeout: Long = 15.seconds

  /**
   * If this is Full, comet updates (partialUpdates or reRenders) are
   * wrapped in a try/catch statement. The provided JsCmd is the body of
   * the catch statement. Within that JsCmd, the variable "e" refers to the
   * caught exception.
   *
   * In development mode, this defaults to Full and the command within
   * invokes lift.cometOnError with the exception;
   * lift.cometOnError rethrows the exception by default. In production
   * mode, this defaults to Empty.
   *
   * Note that if you set this to Full, it is highly advised that you
   * rethrow the exception. If you fail to rethrow the exception, you
   * run the risk of dropping an unpredictable number of updates (i.e.,
   * if the third of 20 updates that are sent to the client in a single
   * response throws an exception, none of the subsequent ones will run;
   * failing to rethrow the exception means any updates that did not run
   * will never be run).
   */
  val cometUpdateExceptionHandler: FactoryMaker[Box[JsCmd]] =
    new FactoryMaker[Box[JsCmd]]( () => {
      if (Props.devMode)
        Full(JE.Call("lift.cometOnError", JE.JsVar("e")).cmd)
      else
        Empty
    } ) {}

  /**
   * Holds the last update time of the Ajax request. Based on this server may return HTTP 304 status
   * indicating the client to used the cached information.
   */
  @volatile var ajaxScriptUpdateTime: LiftSession => Long = session => {
    object when extends SessionVar[Long](millis)
    when.is
  }

  /**
   * Determines the path parts and suffix from given path parts
   */
  val suffixSplitters = RulesSeq[SplitSuffixPF].append {
    case parts =>
      val last = parts.last
      val idx: Int = {
        val firstDot = last.indexOf(".")
        val len = last.length
        if (firstDot + 1 == len) -1 // if the dot is the last character, don't split
        else {
          if (last.indexOf(".", firstDot + 1) != -1) -1 // if there are multiple dots, don't split out
          else {
            val suffix = last.substring(firstDot + 1)
            // if the suffix isn't in the list of suffixes we care about, don't split it
            if (!LiftRules.explicitlyParsedSuffixes.contains(suffix.toLowerCase)) -1
            else firstDot
          }
        }
      }

      if (idx == -1) (parts, "")
      else (parts.dropRight(1) ::: List(last.substring(0, idx)), last.substring(idx + 1))

  }

  /**
   * The list of suffixes that Lift looks for in templates.
   * By default, html, xhtml, and htm
   */
  @volatile var templateSuffixes: List[String] = List("html", "xhtml", "htm", "md")

  /**
   * When a request is parsed into a Req object, certain suffixes are explicitly split from
   * the last part of the request URI.  If the suffix is contained in this list, it is explicitly split.
   * The default list is: "html", "htm", "jpg", "png", "gif", "xml", "rss", "json" ...
   */
  @volatile var explicitlyParsedSuffixes: Set[String] = knownSuffixes

  /**
   * The global multipart progress listener:
   *    pBytesRead - The total number of bytes, which have been read so far.
   *    pContentLength - The total number of bytes, which are being read. May be -1, if this number is unknown.
   *    pItems - The number of the field, which is currently being read. (0 = no item so far, 1 = first item is being read, ...)
   */
  @volatile var progressListener: (Long, Long, Int) => Unit = (_, _, _) => ()

  /**
   * The function that converts a fieldName, contentType, fileName and an InputStream into
   * a FileParamHolder.  By default, create an in-memory instance.  Use OnDiskFileParamHolder
   * to create an on-disk version
   */
  @volatile var handleMimeFile: (String, String, String, InputStream) => FileParamHolder =
  (fieldName, contentType, fileName, inputStream) =>
          new InMemFileParamHolder(fieldName, contentType, fileName, Helpers.readWholeStream(inputStream))

  private object _mimeHeaders extends TransientRequestVar[Box[Map[String, List[String]]]](Empty)

  /**
   * Returns any mimeHeaders for the currently invoked handleMimeFile.
   */
  def mimeHeaders = _mimeHeaders.get

  private[http] def withMimeHeaders[T](map: Map[String, List[String]])(f: => T): T = _mimeHeaders.doWith(Full(map))(f)

  @volatile var templateCache: Box[TemplateCache[(Locale, List[String]), NodeSeq]] = Empty

  val dateTimeConverter: FactoryMaker[DateTimeConverter] = new FactoryMaker[DateTimeConverter]( () => DefaultDateTimeConverter ) {}

  /**
   * This variable controls whether RequestVars that have been set but not subsequently
   * read will be logged in Dev mode. Logging can be disabled at the per-RequestVar level
   * via RequestVar.logUnreadVal
   *
   * @see RequestVar#logUnreadVal
   */
  @volatile var logUnreadRequestVars = true

  /** Controls whether or not the service handling timing messages (Service request (GET) ... took ... Milliseconds) are logged. Defaults to true. */
  @volatile var logServiceRequestTiming = true

  /** Provides a function that returns random names for form variables, page ids, callbacks, etc. */
  @volatile var funcNameGenerator: () => String = defaultFuncNameGenerator(Props.mode)

  import provider.servlet._
  import containers._

  /**
   * The meta for the detected AsyncProvider given the container we're running in
   */
  lazy val asyncProviderMeta: Box[AsyncProviderMeta] =
    asyncMetaList.find(_.suspendResumeSupport_?)

  /**
   * A function that converts the current Request into an AsyncProvider.
   */
  lazy val theServletAsyncProvider: Box[HTTPRequest => ServletAsyncProvider] =
    asyncProviderMeta.flatMap(_.providerFunction)

  private var asyncMetaList: List[AsyncProviderMeta] =
    List(Servlet30AsyncProvider, Jetty6AsyncProvider, Jetty7AsyncProvider)

  /**
   * Register an AsyncMeta provider in addition to the default
   * Jetty6, Jetty7, and Servlet 3.0 providers
   */
  def addSyncProvider(asyncMeta: AsyncProviderMeta) {
    if (doneBoot) throw new IllegalStateException("Cannot modify after boot.")
    asyncMetaList ::= asyncMeta
  }

  def updateAsyncMetaList(f: List[AsyncProviderMeta] => List[AsyncProviderMeta]) {
    if (doneBoot) throw new IllegalStateException("Cannot modify after boot.")
    asyncMetaList = f(asyncMetaList)

  }


  private def ctor() {
    appendGlobalFormBuilder(FormBuilderLocator[String]((value, setter) => SHtml.text(value, setter)))
    appendGlobalFormBuilder(FormBuilderLocator[Int]((value, setter) => SHtml.text(value.toString, s => Helpers.asInt(s).foreach((setter)))))
    appendGlobalFormBuilder(FormBuilderLocator[Boolean]((value, setter) => SHtml.checkbox(value, s => setter(s))))

    import net.liftweb.builtin.snippet._

    snippetDispatch.append(
      Map("CSS" -> CSS, "Msgs" -> Msgs, "Msg" -> Msg,
        "Menu" -> Menu, "css" -> CSS, "msgs" -> Msgs, "msg" -> Msg,
        "menu" -> Menu,
        "children" -> Children,
        "comet" -> Comet, "form" -> Form, "ignore" -> Ignore, "loc" -> Loc,
        "surround" -> Surround,
        "test_cond" -> TestCond,
        "TestCond" -> TestCond,
        "testcond" -> TestCond,
        "embed" -> Embed,
        "tail" -> Tail,
        "head" -> Head,
        "Head" -> Head,
        "with-param" -> WithParam,
        "withparam" -> WithParam,
        "WithParam" -> WithParam,
        "bind-at" -> WithParam,
        "VersionInfo" -> VersionInfo,
        "versioninfo" -> VersionInfo,
        "version_info" -> VersionInfo,
        "SkipDocType" -> SkipDocType,
        "skipdoctype" -> SkipDocType,
        "skip_doc_type" -> SkipDocType,
        "xml_group" -> XmlGroup,
        "XmlGroup" -> XmlGroup,
        "xmlgroup" -> XmlGroup,
        "lazy-load" -> LazyLoad,
        "LazyLoad" -> LazyLoad,
        "lazyload" -> LazyLoad,
        "html5" -> HTML5,
        "HTML5" -> HTML5,
        "with-resource-id" -> WithResourceId
        ))
  }
  ctor()

  object RulesSeq {
    def apply[T]: RulesSeq[T] = new RulesSeq[T]()
  }

/**
 * Generic container used mainly for adding functions
 *
 */
class RulesSeq[T] {
  @volatile private var rules: List[T] = Nil
  private val pre = new ThreadGlobal[List[T]]
  private val app = new ThreadGlobal[List[T]]
  private val cur = new ThreadGlobal[List[T]]

  private def safe_?(f: => Any) {
    doneBoot match {
      case false => f
      case _ => throw new IllegalStateException("Cannot modify after boot.");
    }
  }

  /**
   * Sometimes it's useful to change the rule for the duration of
   * a thread... prepend a rule and execute the code within
   * a scope with the prepended rule
   */
  def prependWith[A](what: T)(f: => A): A = prependWith(List(what))(f)

  /**
   * Sometimes it's useful to change the rule for the duration of
   * a thread... append a rule and execute the code within
   * a scope with the appended rule
   */
  def appendWith[A](what: T)(f: => A): A = appendWith(List(what))(f)

  /**
   * Sometimes it's useful to change the rule for the duration of
   * a thread... prepend rules and execute the code within
   * a scope with the prepended rules
   */
  def prependWith[A](what: List[T])(f: => A): A = {
    val newList = pre.value match {
      case null => what
      case Nil => what
      case x => what ::: x
    }
    pre.doWith(newList)(doCur(f))
  }

  /**
   * Sometimes it's useful to change the rules for the duration of
   * a thread... append rules and execute the code within
   * a scope with the appended rules
   */
  def appendWith[A](what: List[T])(f: => A): A = {
    val newList = pre.value match {
      case null => what
      case Nil => what
      case x => x ::: what
    }
    app.doWith(newList)(doCur(f))
  }

  /**
   * Precompute the current rule set
   */
  private def doCur[A](f: => A): A = {
    cur.doWith((pre.value, app.value) match {
    case (null, null) | (null, Nil) | (Nil, null) | (Nil, Nil) => rules
    case (null, xs) => rules ::: xs
    case (xs, null) => xs ::: rules
    case (p, a) => p ::: rules ::: a
  })(f)
  }

  def toList: List[T] = cur.value match {
    case null => rules
    case xs => xs
  }

  def prepend(r: T): RulesSeq[T] = {
    safe_? {
      rules = r :: rules
    }
    this
  }

  private[http] def remove(f: T => Boolean) {
    safe_? {
      rules = rules.filterNot(f)
    }
  }

  def append(r: T): RulesSeq[T] = {
    safe_? {
      rules = rules ::: List(r)
    }
    this
  }
}

trait FirstBox[F, T] {
  self: RulesSeq[F => Box[T]] =>

  def firstFull(param: F): Box[T] = {
    def finder(in: List[F => Box[T]]): Box[T] = in match {
      case Nil => Empty
      case x :: xs => x(param) match {
        case Full(r) => Full(r)
        case _ => finder(xs)
      }
    }

    finder(toList)
  }
}

}

sealed trait NotFound

case object DefaultNotFound extends NotFound

final case class NotFoundAsResponse(response: LiftResponse) extends NotFound

final case class NotFoundAsTemplate(path: ParsePath) extends NotFound

final case class NotFoundAsNode(node: NodeSeq) extends NotFound

final case class BreakOut()

abstract class Bootable {
  def boot(): Unit;
}

/*
/**
 * Factory object for RulesSeq instances
 */
object RulesSeq {
  def apply[T]: RulesSeq[T] = new RulesSeq[T]
}
*/


private[http] case object DefaultBootstrap extends Bootable {
  def boot(): Unit = {
    val f = createInvoker("boot", Class.forName("bootstrap.liftweb.Boot").newInstance.asInstanceOf[AnyRef])
    f.map {f => f()}
  }
}

/**
 * Holds the Comet identification information
 */
trait CometVersionPair {
  def guid: String

  def version: Long
}
object CometVersionPair {
  def unapply(pair: CometVersionPair): Option[(String, Long)] = {
    Some((pair.guid, pair.version))
  }
}

case class CVP(guid: String, version: Long) extends CometVersionPair

case class XHTMLValidationError(msg: String, line: Int, col: Int)

trait XHtmlValidator extends Function1[Node, List[XHTMLValidationError]]

object StrictXHTML1_0Validator extends GenericValidator {
  def ngurl = "http://www.w3.org/2002/08/xhtml/xhtml1-strict.xsd"
}

abstract class GenericValidator extends XHtmlValidator with Loggable {
  import javax.xml.validation._
  import javax.xml._
  import XMLConstants._
  import java.net.URL
  import javax.xml.transform.dom._
  import javax.xml.transform.stream._
  import java.io.ByteArrayInputStream

  private lazy val sf = SchemaFactory.newInstance(W3C_XML_SCHEMA_NS_URI)

  protected def ngurl: String

  private lazy val schema = tryo(sf.newSchema(new URL(ngurl)))

  def apply(in: Node): List[XHTMLValidationError] = {
    (for{
      sc <- schema
      v <- tryo(sc.newValidator)
      source = new StreamSource(new ByteArrayInputStream(in.toString.getBytes("UTF-8")))
    } yield try {
        v.validate(source)
        Nil
      } catch {
        case e: org.xml.sax.SAXParseException =>
          List(XHTMLValidationError(e.getMessage, e.getLineNumber, e.getColumnNumber))
      }) match {
      case Full(x) => x
      case Failure(msg, _, _) =>
        logger.info("XHTML Validation Failure: " + msg)
        Nil
      case _ => Nil
    }
  }
}


object TransitionalXHTML1_0Validator extends GenericValidator {
  def ngurl = "http://www.w3.org/2002/08/xhtml/xhtml1-transitional.xsd"
}


trait FormVendor {
  /**
   * Given a type manifest, vend a form
   */
  def vendForm[T](implicit man: Manifest[T]): Box[(T, T => Any) => NodeSeq] = {
    val name = man.toString
    val first: Option[List[FormBuilderLocator[_]]] = requestForms.is.get(name) orElse sessionForms.is.get(name)

    first match {
      case Some(x :: _) => Full(x.func.asInstanceOf[(T, T => Any) => NodeSeq])
      case _ => if (globalForms.containsKey(name)) {
        globalForms.get(name).headOption.map(_.func.asInstanceOf[(T, T => Any) => NodeSeq])
      } else Empty
    }
  }

  private val globalForms: CHash[String, List[FormBuilderLocator[_]]] = new CHash

  def prependGlobalFormBuilder[T](builder: FormBuilderLocator[T]) {
    globalForms.synchronized {
      val name = builder.manifest.toString
      if (globalForms.containsKey(name)) {
        globalForms.put(name, builder :: globalForms.get(name))
      } else {
        globalForms.put(name, List(builder))
      }
    }
  }

  def appendGlobalFormBuilder[T](builder: FormBuilderLocator[T]) {
    globalForms.synchronized {
      val name = builder.manifest.toString
      if (globalForms.containsKey(name)) {
        globalForms.put(name, builder :: globalForms.get(name))
      } else {
        globalForms.put(name, List(builder))
      }
    }
  }

  def prependSessionFormBuilder[T](builder: FormBuilderLocator[T]) {
    sessionForms.set(prependBuilder(builder, sessionForms))
  }

  def appendSessionFormBuilder[T](builder: FormBuilderLocator[T]) {
    sessionForms.set(appendBuilder(builder, sessionForms))
  }

  def prependRequestFormBuilder[T](builder: FormBuilderLocator[T]) {
    requestForms.set(prependBuilder(builder, requestForms))
  }

  def appendRequestFormBuilder[T](builder: FormBuilderLocator[T]) {
    requestForms.set(appendBuilder(builder, requestForms))
  }

  def doWith[F, T](builder: FormBuilderLocator[T])(f: => F): F =
    requestForms.doWith(prependBuilder(builder, requestForms))(f)


  private def prependBuilder(builder: FormBuilderLocator[_], to: Map[String, List[FormBuilderLocator[_]]]):
  Map[String, List[FormBuilderLocator[_]]] = {
    val name = builder.manifest.toString
    to + (name -> (builder :: to.getOrElse(name, Nil)))
  }

  private def appendBuilder(builder: FormBuilderLocator[_], to: Map[String, List[FormBuilderLocator[_]]]):
  Map[String, List[FormBuilderLocator[_]]] = {
    val name = builder.manifest.toString
    to + (name -> (builder :: to.getOrElse(name, Nil)))
  }


  private object sessionForms extends SessionVar[Map[String, List[FormBuilderLocator[_]]]](Map())
  private object requestForms extends SessionVar[Map[String, List[FormBuilderLocator[_]]]](Map())
}

