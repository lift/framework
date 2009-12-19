package net.liftweb.http.provider.servlet

import _root_.java.io.{InputStream}
import _root_.java.util.{Locale}
import _root_.javax.servlet.http.{HttpServletRequest}
import _root_.org.apache.commons.fileupload.servlet._
import _root_.org.apache.commons.fileupload.ProgressListener
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._

class HTTPRequestServlet(val req: HttpServletRequest) extends HTTPRequest {
  private lazy val ctx = new HTTPServletContext(req.getSession.getServletContext)

  private val (hasContinuations_?, contSupport, getContinuation, getObject, setObject, suspend, resume) = {
    try {
      val cc = Class.forName("org.mortbay.util.ajax.ContinuationSupport")
      val meth = cc.getMethod("getContinuation", classOf[HTTPRequest], classOf[AnyRef])
      val cci = Class.forName("org.mortbay.util.ajax.Continuation")
      val getObj = cci.getMethod("getObject")
      val setObj = cci.getMethod("setObject", classOf[AnyRef])
      val suspend = cci.getMethod("suspend", _root_.java.lang.Long.TYPE)
      val resume = cci.getMethod("resume")
      (true, (cc), (meth), (getObj), (setObj), (suspend), resume)
    } catch {
      case e => (false, null, null, null, null, null, null)
    }
  }


  lazy val cookies: List[HTTPCookie] = {
    req.getSession(false) // do this to make sure we capture the JSESSIONID cookie
    (Box !! req.getCookies).map(_.toList.map(c => HTTPCookie(c.getName,
      Box !! (c.getValue),
      Box !! (c.getDomain),
      Box !! (c.getPath),
      Box !! (c.getMaxAge),
      Box !! (c.getVersion),
      Box !! (c.getSecure)))) openOr Nil
  }

  lazy val authType: Box[String] = Box !! req.getAuthType

  def headers(name: String): List[String] = enumToList[String](req.getHeaders(name).asInstanceOf[_root_.java.util.Enumeration[String]])

  lazy val headers: List[HTTPParam] = enumToList[String](req.getHeaderNames.asInstanceOf[_root_.java.util.Enumeration[String]]).
          map(n => HTTPParam(n, headers(n)))

  def contextPath: String = req.getContextPath

  def context: HTTPContext = ctx

  def contentType = Box !! req.getContentType

  lazy val session = new HTTPServletSession(req getSession)

  def uri = req.getRequestURI

  def url = req.getRequestURL.toString

  lazy val queryString: Box[String] = Box !! req.getQueryString

  def param(name: String): List[String] = req.getParameterValues(name) match {case null => Nil case x => x.toList}

  lazy val params: List[HTTPParam] = enumToList[String](req.getParameterNames.asInstanceOf[_root_.java.util.Enumeration[String]]).
          map(n => HTTPParam(n, param(n)))

  lazy val paramNames: List[String] = params map (_.name)

  def remoteAddress: String = req.getRemoteAddr

  def remotePort: Int = req.getRemotePort

  def remoteHost: String = req.getRemoteHost

  def serverName = req getServerName

  def scheme: String = req getScheme

  def serverPort = req getServerPort

  def method: String = req.getMethod

  def locale: Box[Locale] = Box !! req.getLocale

  def inputStream: InputStream = req.getInputStream

  def multipartContent_? = ServletFileUpload.isMultipartContent(req)

  /**
   * @return the sessionID (if there is one) for this request.  This will *NOT* create
   * a new session if one does not already exist
   */
  def sessionId: Box[String] =
    for{
      httpSession <- Box !! req.getSession(false)
      id <- Box !! httpSession.getId
    } yield id

  def extractFiles: List[ParamHolder] = (new Iterator[ParamHolder] {
    val mimeUpload = (new ServletFileUpload)
    mimeUpload.setProgressListener(new ProgressListener {
      lazy val progList: (Long, Long, Int) => Unit = S.session.flatMap(_.progressListener) openOr LiftRules.progressListener

      def update(a: Long, b: Long, c: Int) {progList(a, b, c)}
    })

    mimeUpload.setSizeMax(LiftRules.maxMimeSize)
    mimeUpload.setFileSizeMax(LiftRules.maxMimeFileSize)
    val what = mimeUpload.getItemIterator(req)

    def hasNext = what.hasNext

    def next = what.next match {
      case f if (f.isFormField) => NormalParamHolder(f.getFieldName, new String(readWholeStream(f.openStream), "UTF-8"))
      case f => LiftRules.handleMimeFile(f.getFieldName, f.getContentType, f.getName, f.openStream)
    }
  }).toList

  def hasSuspendResumeSupport_? =
    if (!hasContinuations_?) None
    else if (Props.inGAE) None
    else {
      val cont = getContinuation.invoke(contSupport, req, LiftRules)
      val ret = getObject.invoke(cont)
      try {
        setObject.invoke(cont, null)
        Some(ret)
      }
      catch {
        case e: Exception => None
      }
    }


  def suspend(timeout: Long): Nothing = {
    try {
      val cont = getContinuation.invoke(contSupport, req, LiftRules)
      Log.trace("About to suspend continuation")
      suspend.invoke(cont, new _root_.java.lang.Long(timeout))
      throw new Exception("Bail")
    } catch {
      case e: _root_.java.lang.reflect.InvocationTargetException if e.getCause.getClass.getName.endsWith("RetryRequest") =>
        throw e.getCause
    }
  }

  def resume(what: AnyRef) {
    val cont = getContinuation.invoke(contSupport, req, LiftRules)
    setObject.invoke(cont, what)
    resume.invoke(cont)
  }

  def setCharacterEncoding(encoding: String) = req.setCharacterEncoding(encoding)

  def snapshot: HTTPRequest  = new OfflineRequestSnapshot(this)

}

private class OfflineRequestSnapshot(req: HTTPRequest) extends HTTPRequest {

  private val _cookies = List(req.cookies :_*)

  private val _headers = List(req.headers :_*)

  private val _params = List(req.params :_*)

 
  def cookies: List[HTTPCookie] = _cookies

  val authType: Box[String] = req.authType

  def headers(name: String): List[String] = _headers.filter(_.name == name).map(_.name)

  def headers: List[HTTPParam] = _headers

  val contextPath: String = req.contextPath

  val context: HTTPContext = req.context

  val contentType: Box[String] = req.contentType

  val uri: String = req.uri

  val url: String = req.url

  val queryString: Box[String] = req.queryString

  def param(name: String): List[String] = _params.filter(_.name == name).map(_.name)

  def params: List[HTTPParam] = _params

  def paramNames: List[String] = _params.map(_.name)

  val session: HTTPSession = req.session

  val sessionId: Box[String] = req.sessionId

  val remoteAddress: String = req.remoteAddress

  val remotePort: Int = req.remotePort

  val remoteHost: String = req.remoteHost

  val serverName: String = req.serverName

  val scheme: String = req.scheme

  val serverPort: Int = req.serverPort

  val method: String = req.method

  val hasSuspendResumeSupport_? : Option[Any] = req.hasSuspendResumeSupport_?

  def suspend(timeout: Long): Nothing = 
    throw new UnsupportedOperationException("Cannot suspend a snapshot")

  def resume(what: AnyRef): Unit = 
    throw new UnsupportedOperationException("Cannot resume a snapshot")

  def inputStream: InputStream = 
    throw new UnsupportedOperationException("InputStream is not available")

  val multipartContent_? : Boolean = req.multipartContent_?

  def extractFiles: List[ParamHolder] = 
    throw new UnsupportedOperationException("It is unsafe to extract files")

  val locale: Box[Locale] = req.locale

  def setCharacterEncoding(encoding: String) = 
    throw new UnsupportedOperationException("It is unsafe to set the character encoding ")

  def snapshot = this

}
