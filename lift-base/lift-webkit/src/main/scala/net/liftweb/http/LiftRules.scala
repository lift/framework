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

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.http.js.JSArtifacts
import _root_.net.liftweb.http.js.jquery._
import _root_.net.liftweb.http.provider._
import _root_.scala.xml._
import _root_.scala.collection.mutable.{ListBuffer}
import _root_.java.util.{Locale, TimeZone, ResourceBundle, Date}
import _root_.java.io.{InputStream, ByteArrayOutputStream, BufferedReader, StringReader}
import js._
import JE._
import JsCmds._
import auth._
import _root_.java.util.concurrent.{ConcurrentHashMap => CHash}
import _root_.scala.reflect.Manifest

import _root_.java.util.concurrent.atomic.AtomicInteger

object LiftRules extends Factory with FormVendor with LazyLoggable {
  val noticesContainerId = "lift__noticesContainer__"
  private val pageResourceId = Helpers.nextFuncName

  type DispatchPF = PartialFunction[Req, () => Box[LiftResponse]];

  /**
   * The test between the path of a request and whether that path
   * should result in stateless servicing of that path
   */
  type StatelessTestPF = PartialFunction[List[String], Boolean]

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

  /**
   * Set the default fadeout mechanism for Lift notices. Thus you provide a function that take a NoticeType.Value
   * and decide the duration after which the fade out will start and the actual fadeout time. This is applicable
   * for general notices (not associated with id-s) regardless if they are set for the page rendering, ajax
   * response or Comet response.
   */
  var noticesAutoFadeOut = new FactoryMaker[(NoticeType.Value) => Box[(TimeSpan, TimeSpan)]]((notice : NoticeType.Value) => Empty){}

  /**
   * Use this to apply various effects to the notices. The user function receives the NoticeType
   * and the id of the element containing the specific notice. Thus it is the function's responsability to form
   * the javascript code for the visual effects. This is applicable for both ajax and non ajax contexts.
   * For notices associated with ID's the user type will receive an Empty notice type. That's because the effect
   * is applied on the real estate holding the notices for this ID. Typically this contains a single message.
   */
  var noticesEffects = new FactoryMaker[(Box[NoticeType.Value], String) => Box[JsCmd]]((notice: Box[NoticeType.Value], id: String) => Empty){}


  /**
   * Holds user functions that willbe executed very early in the request processing. The functions'
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
   * is protected by authentication,and authenticated subjed must be assigned to the role returned by
   * this function or to a role that is child-of this role. If this function returns Empty it means that
   * this resource is protected by authentication but no authorization is performed meaning that roles are
   * not verified.
   */
  val httpAuthProtectedResource = RulesSeq[HttpAuthProtectedResourcePF]

  /**
   * The HTTP authentication mechanism that ift will perform. See <i>LiftRules.protectedResource</i>
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

  @volatile var enableContainerSessions = true

  @volatile var getLiftSession: (Req) => LiftSession = (req) => _getLiftSession(req)

  /**
   * Attached an ID entity for resource URI specified in
   * link or script tags. This allows controlling browser
   * resource caching. By default this just adds a query string
   * parameter unique per application lifetime. More complex
   * implementation could user per resource MD5 sequences thus
   * "forcing" browsers to refresh the resource only when the resource
   * file changes. Users can define other rules as well. Inside user's
   * function it is safe to use S context as attachResourceId is called
   * from inside the &lt;lift:with-resource-id> snippet
   *
   */
  @volatile var attachResourceId: (String) => String = (name) => {
    name + (if (name contains ("?")) "&" else "?") + pageResourceId + "=_"
  }

  /**
   * Returns a LiftSession instance.
   */
  private def _getLiftSession(req: Req): LiftSession = {
    val wp = req.path.wholePath
    val cometSessionId =
    if (wp.length >= 3 && wp.head == LiftRules.cometPath)
      Full(wp(2))
    else
      Empty

    val ret = SessionMaster.getSession(req.request, cometSessionId) match {
      case Full(ret) =>
        ret.fixSessionTime()
        ret

      case _ =>
        val ret = LiftSession(req)
        ret.fixSessionTime()
        SessionMaster.addSession(ret, req.request.userAgent, SessionMaster.getIpFromReq(req))
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
    // Comet requests will not be counted
    val which = session.cometForHost(req.hostAndPath)

    // get the maximum requests given the browser type
    val max = maxConcurrentRequests.vend(req) - 2 // this request and any open comet requests

    // dump the oldest requests
    which.drop(max).foreach {
      case (actor, req) => actor ! BreakOut()
    }
  }

  /**
   * The path to handle served resources
   */
  @volatile var resourceServerPath = "classpath"

  /**
   * Holds the JS library specific UI artifacts. By efault it uses JQuery's artifacts
   */
  @volatile var jsArtifacts: JSArtifacts = JQuery13Artifacts

  /**
   * Use this PartialFunction to to automatically add static URL parameters
   * to any URL reference from the markup of Ajax request.
   */
  val urlDecorate = RulesSeq[URLDecoratorPF]

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
   * Certain paths within your application can be marked as stateless
   * and if there is access to Lift's stateful facilities (setting
   * SessionVars, updating function tables, etc.) the developer will
   * receive a notice and the operation will not complete
   */
  val statelessTest = RulesSeq[StatelessTestPF]

  val statelessSession: FactoryMaker[Req => LiftSession with StatelessSession] =
    new FactoryMaker((req: Req) => new LiftSession(req.contextPath, 
                                                   Helpers.randomString(20),
                                                   Empty) with
                     StatelessSession) {}


  /**
   * Holds user functions that are executed after the response was sent to client. The functions' result
   * will be ignored.
   */
  val afterSend = RulesSeq[(BasicResponse, HTTPResponse, List[(String, String)], Box[Req]) => Any]

  /**
   * Calculate the Comet Server (by default, the server that
   * the request was made on, but can do the multi-server thing
   * as well)
   */
  @volatile var cometServer: () => String = () => S.contextPath

  /**
   * The maximum concurrent requests.  If this number of
   * requests are being serviced for a given session, messages
   * will be sent to all Comet requests to terminate
   */
  val maxConcurrentRequests: FactoryMaker[Req => Int] = new FactoryMaker((x: Req) => x match {
    case r if r.isFirefox35_+ || r.isIE8 || r.isChrome3_+ || r.isOpera9 || r.isSafari3_+ => 6
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
      val newUrl = new _root_.java.net.URL(url.toExternalForm.split("!")(0) + "!" + "/META-INF/MANIFEST.MF")
      str <- tryo(new String(readWholeStream(newUrl.openConnection.getInputStream), "UTF-8"))
      ma <- """lift_version: (.*)""".r.findFirstMatchIn(str)
    } yield ma.group(1)

    ret openOr "Unknown Lift Version"
  }

  lazy val liftBuildDate: Date = {
    val cn = """\.""".r.replaceAllIn(LiftRules.getClass.getName, "/")
    val ret: Box[Date] =
    for{
      url <- Box !! LiftRules.getClass.getResource("/" + cn + ".class")
      val newUrl = new _root_.java.net.URL(url.toExternalForm.split("!")(0) + "!" + "/META-INF/MANIFEST.MF")
      str <- tryo(new String(readWholeStream(newUrl.openConnection.getInputStream), "UTF-8"))
      ma <- """Bnd-LastModified: (.*)""".r.findFirstMatchIn(str)
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
   * Set the doc type used.
   */
  val docType: FactoryMaker[Req => Box[String]] = new FactoryMaker( (r: Req) => r  match {
    case _ if S.skipDocType => Empty
    case _ if S.getDocType._1 => S.getDocType._2
    case _ => Full(DocType.xhtmlTransitional)
  }){}

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
   * Each LiftTagPF function will be called with the folowing parameters:
   * <pre>
   *  - Element label,
   *  - The Element itselft,
   *  - The attrbutes
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

  @volatile var noticesToJsCmd: () => JsCmd = () => {
    import builtin.snippet._


    def noticesFadeOut(noticeType: NoticeType.Value, id: String): JsCmd =
      (LiftRules.noticesAutoFadeOut()(noticeType) map {
        case (duration, fadeTime) => LiftRules.jsArtifacts.fadeOut(id, duration, fadeTime)
      }) openOr Noop

    def effects(noticeType: Box[NoticeType.Value], id: String): JsCmd =
      LiftRules.noticesEffects()(noticeType, id) match {
        case Full(jsCmd) => jsCmd
        case _ => Noop
      }


    val func: (() => List[NodeSeq], String, MetaData) => NodeSeq = (f, title, attr) => f() map (e => <li>{e}</li>) match {
      case Nil => Nil
      case list => <div>{title}<ul>{list}</ul> </div> % attr
    }

    val f = if (ShowAll.get)
      S.messages _
    else
      S.noIdMessages _

    def makeList(meta: Box[AjaxMessageMeta], notices: List[NodeSeq], title: String, id: String):
      List[(Box[AjaxMessageMeta], List[NodeSeq], String, String)] =
        if (notices.isEmpty) Nil else List((meta, notices, title, id))

    val xml =
      ((makeList(MsgsErrorMeta.get, f(S.errors), S.??("msg.error"), LiftRules.noticesContainerId + "_error")) ++
       (makeList(MsgsWarningMeta.get, f(S.warnings), S.??("msg.warning"), LiftRules.noticesContainerId + "_warn")) ++
       (makeList(MsgsNoticeMeta.get, f(S.notices), S.??("msg.notice"), LiftRules.noticesContainerId + "_notice"))) flatMap {
         msg => msg._1 match {
           case Full(meta) => <div id={msg._4}>{func(msg._2 _, meta.title openOr "",
             meta.cssClass.map(new UnprefixedAttribute("class",_, Null)) openOr Null)}</div>
           case _ => <div id={msg._4}>{func(msg._2 _, msg._3, Null)}</div>
        }
      }

    val groupMessages = xml match {
      case Nil => JsCmds.Noop
      case _ => LiftRules.jsArtifacts.setHtml(LiftRules.noticesContainerId, xml) &
        noticesFadeOut(NoticeType.Notice, LiftRules.noticesContainerId + "_notice") &
        noticesFadeOut(NoticeType.Warning, LiftRules.noticesContainerId + "_warn") &
        noticesFadeOut(NoticeType.Error, LiftRules.noticesContainerId + "_error") &
        effects(Full(NoticeType.Notice), LiftRules.noticesContainerId + "_notice") &
        effects(Full(NoticeType.Warning), LiftRules.noticesContainerId + "_warn") &
        effects(Full(NoticeType.Error), LiftRules.noticesContainerId + "_error")
    }

    val g = S.idMessages _
    List((MsgErrorMeta.get, g(S.errors)),
      (MsgWarningMeta.get, g(S.warnings)),
      (MsgNoticeMeta.get, g(S.notices))).foldLeft(groupMessages)((car, cdr) => cdr match {
      case (meta, m) => m.foldLeft(car)((left, r) =>
              left & LiftRules.jsArtifacts.setHtml(r._1, <span>{r._2 flatMap (node => node)}</span> %
                      (Box(meta.get(r._1)).map(new UnprefixedAttribute("class", _, Null)) openOr Null)) & effects(Empty, r._1))
    })
  }

  /**
   * The base name of the resource bundle of the lift core code
   */
  @volatile var liftCoreResourceName = "i18n.lift-core"

  /**
   * Where to send the user if there's no comet session
   */
  @volatile var noCometSessionPage = "/"

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
  @volatile var cometFailureRetryTimeout: Long = 10 seconds

  /**
   * The dispatcher that takes a Snippet and converts it to a
   * DispatchSnippet instance
   */
  val snippetDispatch = RulesSeq[SnippetDispatchPF]

  private def setupSnippetDispatch() {
    import net.liftweb.builtin.snippet._

    snippetDispatch.append(
      Map("CSS" -> CSS, "Msgs" -> Msgs, "Msg" -> Msg,
        "Menu" -> Menu, "css" -> CSS, "msgs" -> Msgs, "msg" -> Msg,
        "menu" -> Menu,
        "a" -> A, "children" -> Children,
        "comet" -> Comet, "form" -> Form, "ignore" -> Ignore, "loc" -> Loc,
        "surround" -> Surround,
        "test_cond" -> TestCond,
        "TestCond" -> TestCond,
        "embed" -> Embed,
        "tail" -> Tail,
        "with-param" -> WithParam,
        "bind-at" -> WithParam,
        "VersionInfo" -> VersionInfo,
        "version_info" -> VersionInfo,
        "SkipDocType" -> SkipDocType,
        "skip_doc_type" -> SkipDocType,
        "xml_group" -> XmlGroup,
        "XmlGroup" -> XmlGroup,
        "lazy-load" -> LazyLoad,
        "html5" -> HTML5,
        "HTML5" -> HTML5,
        "with-resource-id" -> WithResourceId
        ))
  }
  setupSnippetDispatch()

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
   * The JavaScript to execute at the begining of an
   * Ajax request (for example, showing the spinning working thingy)
   */
  @volatile var ajaxStart: Box[() => JsCmd] = Empty

  /**
   * The function that calculates if the response should be rendered in
   * IE6/7 compatibility mode
   */
  @volatile var calcIEMode: () => Boolean =
  () => (for (r <- S.request) yield r.isIE6 || r.isIE7 ||
          r.isIE8) openOr true

  /**
   * The JavaScript to execute at the end of an
   * Ajax request (for example, removing the spinning working thingy)
   */
  @volatile var ajaxEnd: Box[() => JsCmd] = Empty

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
  Full(() => JsCmds.Alert(S.??("ajax.error")))

  /**
   * A function that takes the current HTTP request and returns the current
   */
  @volatile var localeCalculator: Box[HTTPRequest] => Locale = defaultLocaleCalculator _

  def defaultLocaleCalculator(request: Box[HTTPRequest]) =
    request.flatMap(_.locale).openOr(Locale.getDefault())

  val resourceBundleFactories = RulesSeq[ResourceBundleFactoryPF]

  private var _sitemap: Box[SiteMap] = Empty

  private var sitemapFunc: Box[() => SiteMap] = Empty

  private object sitemapRequestVar extends TransientRequestVar(resolveSitemap())

  /**
  * Set the sitemap to a function that will be run to generate the sitemap.
  *
  * This allows for changing the SiteMap when in development mode and having
  * the function re-run for each request.
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
  def setSiteMap(sm: => SiteMap) {
    this.setSiteMapFunc(() => sm)
  }

  private def runAsSafe[T](f: => T): T = synchronized {
     val old = LiftRules.doneBoot
     try {
        LiftRules.doneBoot = false
        f
     } finally {
        LiftRules.doneBoot = old
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
               val loc = menu.loc;
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
   * How long should we wait for all the lazy snippets to render
   */
  val lazySnippetTimeout: FactoryMaker[TimeSpan] = new FactoryMaker(() => 30 seconds) {}

  /**
   * Does the current context support parallel snippet execution
   */
  val allowParallelSnippets: FactoryMaker[Boolean] = new FactoryMaker(() => false) {}

  /**
   * Update the function here that calculates particular paths to
   * exclused from context path rewriting
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

  @volatile private[http] var doneBoot = false;

  /**
   * Holds user's DispatchPF functions that will be executed in a stateless context. This means that
   * S object is not availble yet.
   */
  val statelessDispatchTable = RulesSeq[DispatchPF]

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
   * Contains the Ajax URI path used by Lift to process Ajax requests.
   */
  @volatile var ajaxPath = "ajax_request"

  /**
   * Contains the Comet URI path used by Lift to process Comet requests.
   */
  @volatile var cometPath = "comet_request"

  /**
   * Computes the Comet path by adding additional tokens on top of cometPath
   */
  @volatile var calcCometPath: String => JsExp = prefix => {
    Str(prefix + "/" + cometPath + "/") +
            JsRaw("Math.floor(Math.random() * 100000000000)") +
            Str(S.session.map(s => S.encodeURL("/" + s.uniqueId)) openOr "")
  }


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
   * Used by Lift to construct full pacakge names fromthe packages provided to addToPackages function
   */
  def buildPackage(end: String) = synchronized(otherPackages.map(_ + "." + end))

  /**
   * Tells Lift where to find Snippets,Views, Comet Actors and Lift ORM Model object
   */
  def addToPackages(what: String) {
    synchronized {otherPackages = what :: otherPackages}
  }

  /**
   * Tells Lift where to find Snippets,Views, Comet Actors and Lift ORM Model object
   */
  def addToPackages(what: Package) {
    synchronized {otherPackages = what.getName :: otherPackages}
  }

  private val defaultFinder = getClass.getResource _

  private def resourceFinder(name: String): _root_.java.net.URL = _context.resource(name)

  /**
   * Obtain the resource URL by name
   */
  var getResource: String => Box[_root_.java.net.URL] = defaultGetResource _

  /**
   * Obtain the resource URL by name
   */
  def defaultGetResource(name: String): Box[_root_.java.net.URL] =
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
   * the TemplateFinder object instead.
   *
   * @see TemplateFinder
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
  val dispatch = RulesSeq[DispatchPF]

  /**
   * Holds the user's rewrite functions that can alter the URI parts and query parameters.  This rewrite
   * is performed very early in the HTTP request cycle and may not include any state.  This rewrite is meant
   * to rewrite requests for statelessDispatch
   */
  val statelessRewrite = RulesSeq[RewritePF]

  /**
   * Use statelessRewrite or statefuleRewrite
   */
  @deprecated
  val rewrite = statelessRewrite

  /**
   *  Holds the user's rewrite functions that can alter the URI parts and query parameters.
   * This rewrite takes place within the scope of the S state so SessionVars and other session-related
   * information is available.
   */
  val statefulRewrite = RulesSeq[RewritePF]

  /**
   * Holds the user's snippet functions that will be executed by lift given a certain path.
   */
  val snippets = RulesSeq[SnippetPF]

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
   * Takes a Node, headers, cookies, and a session and turns it into an XhtmlResponse.
   */
  private def cvt(ns: Node, headers: List[(String, String)], cookies: List[HTTPCookie], req: Req, code:Int) =
    convertResponse({
      val ret = XhtmlResponse(ns,
        LiftRules.docType.vend(req),
        headers, cookies, code,
        S.ieMode)
      ret._includeXmlVersion = !S.skipDocType
      ret
    }, headers, cookies, req)

  @volatile var defaultHeaders: PartialFunction[(NodeSeq, Req), List[(String, String)]] = {
    case _ =>
      val d = Helpers.nowAsInternetDate
      List("Expires" -> d,
           "Date" -> d,
           "Cache-Control" ->
           "no-cache; private; no-store",
           "Pragma" -> "no-cache" /*,
           "Keep-Alive" -> "timeout=3, max=993" */ )
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
   * Set to false if you do not want Ajax/Comet requests that are not associated with a session
   * to cause a page reload
   */
  @volatile var redirectAjaxOnSessionLoss = true

  /**
   * Holds the falure information when a snippet can not be executed.
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
  }

  /**
   * The sequence of partial functions (pattern matching) for handling converting an exception to something to
   * be sent to the browser depending on the current RunMode (development, etc.)
   *
   * By default it returns an XhtmlResponse containing a predefined markup. You can overwrite this by calling
   * LiftRules.exceptionHandler.prepend(...). If you are calling append then your code will not be calle since
   * a default implementation is already appended.
   *
   */
  @volatile var exceptionHandler = RulesSeq[ExceptionHandlerPF].append {
    case (Props.RunModes.Development, r, e) =>
      logger.error("Exception being returned to browser when processing " + r.uri.toString + ": " + showException(e))
      XhtmlResponse((<html> <body>Exception occured while processing {r.uri}<pre>{showException(e)}</pre> </body> </html>), LiftRules.docType.vend(r), List("Content-Type" -> "text/html; charset=utf-8"), Nil, 500, S.ieMode)

    case (_, r, e) =>
      logger.error("Exception being returned to browser when processing " + r, e)
      XhtmlResponse((<html> <body>Something unexpected happened while serving the page at {r.uri}</body> </html>), LiftRules.docType.vend(r), List("Content-Type" -> "text/html; charset=utf-8"), Nil, 500, S.ieMode)
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
            case (_, input) => {
              logger.info("Fixing " + cssPath + " failed");
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
   */
  val onBeginServicing = RulesSeq[Req => Unit]

  val preAccessControlResponse_!! = new RulesSeq[Req => Box[LiftResponse]] with FirstBox[Req, LiftResponse]

  val earlyResponse = new RulesSeq[Req => Box[LiftResponse]] with FirstBox[Req, LiftResponse]

  /**
   * Holds user function hooks when the request was processed
   */
  val onEndServicing = RulesSeq[(Req, Box[LiftResponse]) => Unit]

  /**
   * Tells Lift if the Comet JavaScript shoukd be included. By default it is set to true.
   */
  @volatile var autoIncludeComet: LiftSession => Boolean = session => true

  /**
   * Tells Lift if the Ajax JavaScript shoukd be included. By default it is set to true.
   */
  @volatile var autoIncludeAjax: LiftSession => Boolean = session => true

  /**
   * Define the XHTML validator
   */
  @volatile var xhtmlValidator: Box[XHtmlValidator] = Empty // Full(TransitionalXHTML1_0Validator)

  /**
   * Returns the JavaScript that manages Ajax requests.
   */
  @volatile var renderAjaxScript: LiftSession => JsCmd = session => ScriptRenderer.ajaxScript

  @volatile var ajaxPostTimeout = 5000

  @volatile var cometGetTimeout = 140000

  @volatile var supplimentalHeaders: HTTPResponse => Unit = s => s.addHeaders(List(HTTPParam("X-Lift-Version", liftVersion)))

  @volatile var calcIE6ForResponse: () => Boolean = () => S.request.map(_.isIE6) openOr false

  @volatile var flipDocTypeForIE6 = true

  /**
   * By default lift uses a garbage-collection mechanism of removing unused bound functions from LiftSesssion.
   * Setting this to false will disable this mechanims and there will be no Ajax polling requests attempted.
   */
  @volatile var enableLiftGC = true;

  /**
   * If Lift garbage collection is enabled, functions that are not seen in the page for this period of time
   * (given in milliseonds) will be discarded, hence eligibe for garbage collection.
   * The default value is 10 minutes.
   */
  @volatile var unusedFunctionsLifeTime: Long = 10 minutes

  /**
   * The polling interval for background Ajax requests to prevent functions of being garbage collected.
   * Default value is set to 75 seconds.
   */
  @volatile var liftGCPollingInterval: Long = 75 seconds

  /**
   * Put a test for being logged in into this function
   */
  @volatile var loggedInTest: Box[() => Boolean] = Empty

  /**
   * The polling interval for background Ajax requests to keep functions to not be garbage collected.
   * This will be applied if the Ajax request will fail. Default value is set to 15 seconds.
   */
  @volatile var liftGCFailureRetryTimeout: Long = 15 seconds

  /**
   * Returns the JavaScript that manages Comet requests.
   */
  @volatile var renderCometScript: LiftSession => JsCmd = session => ScriptRenderer.cometScript

  /**
   * Renders that JavaScript that holds Comet identification information
   */
  @volatile var renderCometPageContents: (LiftSession, Seq[CometVersionPair]) => JsCmd =
  (session, vp) => JsCmds.Run(
    "var lift_toWatch = " + vp.map(p => p.guid.encJs + ": " + p.version).mkString("{", " , ", "}") + ";"
    )

  /**
   * Holds the last update time of the Ajax request. Based on this server mayreturn HTTP 304 status
   * indicating the client to used the cached information.
   */
  @volatile var ajaxScriptUpdateTime: LiftSession => Long = session => {
    object when extends SessionVar[Long](millis)
    when.is
  }

  /**
   * Determins the path parts and suffix from given path parts
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

  /**
   * Holds the last update time of the Comet request. Based on this server mayreturn HTTP 304 status
   * indicating the client to used the cached information.
   */
  @volatile var cometScriptUpdateTime: LiftSession => Long = session => {
    object when extends SessionVar[Long](millis)
    when.is
  }

  /**
   * The name of the Ajax script that manages Ajax rewuests.
   */
  @volatile var ajaxScriptName: () => String = () => "liftAjax.js"

  /**
   * The name of the Comet script that manages Comet rewuests.
   */
  @volatile var cometScriptName: () => String = () => "cometAjax.js"

  /**
   * Returns the Comet script as a JavaScript response
   */
  @volatile var serveCometScript: (LiftSession, Req) => Box[LiftResponse] =
  (liftSession, requestState) => {
    val modTime = cometScriptUpdateTime(liftSession)

    requestState.testFor304(modTime) or
            Full(JavaScriptResponse(renderCometScript(liftSession),
              List("Last-Modified" -> toInternetDate(modTime),
                   "Expires" -> toInternetDate(modTime + 10.minutes),
                   "Date" -> Helpers.nowAsInternetDate,
                   "Pragma" -> "",
                   "Cache-Control" -> ""),
              Nil, 200))
  }

  /**
   * Returns the Ajax script as a JavaScript response
   */
  @volatile var serveAjaxScript: (LiftSession, Req) => Box[LiftResponse] =
  (liftSession, requestState) => {
    val modTime = ajaxScriptUpdateTime(liftSession)

    requestState.testFor304(modTime) or
            Full(JavaScriptResponse(renderAjaxScript(liftSession),
              List("Last-Modified" -> toInternetDate(modTime),
                "Expires" -> toInternetDate(modTime + 10.minutes)),
              Nil, 200))
  }

  @volatile var templateCache: Box[TemplateCache[(Locale, List[String]), NodeSeq]] = Empty

  /**
   * A function to format a Date... can be replaced by a function that is user-specific
   Replaced by dateTimeConverter
   */
  @deprecated @volatile var formatDate: Date => String = date => date match {case null => LiftRules.formatDate(new Date(0L)) case s => toInternetDate(s)}

  /**
   * A function that parses a String into a Date... can be replaced by something that's user-specific
   Replaced by dateTimeConverter
   */
  @deprecated @volatile var parseDate: String => Box[Date] = str => str match {
    case null => Empty
    case s => Helpers.toDate(s)
  }

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

  import provider.servlet._
  import containers._

  /**
   * Provides the async provider instance responsible for suspending/resuming requests
   */
  @volatile var servletAsyncProvider: (HTTPRequest) => ServletAsyncProvider = (req) => new Jetty6AsyncProvider(req)


  private def ctor() {
    appendGlobalFormBuilder(FormBuilderLocator[String]((value, setter) => SHtml.text(value, setter)))
    appendGlobalFormBuilder(FormBuilderLocator[Int]((value, setter) => SHtml.text(value.toString, s => Helpers.asInt(s).foreach((setter)))))
    appendGlobalFormBuilder(FormBuilderLocator[Boolean]((value, setter) => SHtml.checkbox(value, s => setter(s))))
  }
  ctor()
}

sealed trait NotFound

final case object DefaultNotFound extends NotFound

final case class NotFoundAsResponse(response: LiftResponse) extends NotFound

final case class NotFoundAsTemplate(path: ParsePath) extends NotFound

final case class NotFoundAsNode(node: NodeSeq) extends NotFound

final case class BreakOut()

abstract class Bootable {
  def boot(): Unit;
}

/**
 * Factory object for RulesSeq instances
 */
object RulesSeq {
  def apply[T]: RulesSeq[T] = new RulesSeq[T]
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
    LiftRules.doneBoot match {
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

  def toList = cur.value match {
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
      rules = rules.remove(f)
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

case class CVP(guid: String, version: Long) extends CometVersionPair

case class XHTMLValidationError(msg: String, line: Int, col: Int)

trait XHtmlValidator extends Function1[Node, List[XHTMLValidationError]]

object StrictXHTML1_0Validator extends GenericValidtor {
  val ngurl = "http://www.w3.org/2002/08/xhtml/xhtml1-strict.xsd"
}

abstract class GenericValidtor extends XHtmlValidator with Loggable {
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


object TransitionalXHTML1_0Validator extends GenericValidtor {
  val ngurl = "http://www.w3.org/2002/08/xhtml/xhtml1-transitional.xsd"
}


trait FormVendor {
  /**
   * Given a type manifest, vend a form
   */
  def vendForm[T](implicit man: Manifest[T]): Box[(T, T => Unit) => NodeSeq] = {
    val name = man.toString
    val first: Option[List[FormBuilderLocator[_]]] = requestForms.is.get(name) orElse sessionForms.is.get(name)

    first match {
      case Some(x :: _) => Full(x.func.asInstanceOf[(T, T => Unit) => NodeSeq])
      case _ => if (globalForms.containsKey(name)) {
        globalForms.get(name).headOption.map(_.func.asInstanceOf[(T, T => Unit) => NodeSeq])
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


}
}
