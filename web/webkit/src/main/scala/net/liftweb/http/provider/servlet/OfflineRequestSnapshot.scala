package net.liftweb.http.provider.servlet

import java.io.InputStream
import java.util.Locale

import net.liftweb.common.Box
import net.liftweb.http.provider._
import net.liftweb.http.{LiftResponse, ParamHolder, Req}
import net.liftweb.util.Helpers

private [servlet] class OfflineRequestSnapshot(req: HTTPRequest, val provider: HTTPProvider) extends HTTPRequest {

  private[this] val _cookies = List(req.cookies :_*)

  private[this] val _headers = List(req.headers :_*)

  private[this] val _params = List(req.params :_*)

  private[this] val _serverPort = req.serverPort


  def cookies: List[HTTPCookie] = _cookies

  val authType: Box[String] = req.authType

  def headers(name: String): List[String] = {
    _headers
      .find(_.name.equalsIgnoreCase(name))
      .map(_.values)
      .getOrElse(Nil)
  }

  def headers: List[HTTPParam] = _headers

  val contextPath: String = req.contextPath

  val context: HTTPContext = req.context

  val contentType: Box[String] = req.contentType

  val uri: String = req.uri

  val url: String = req.url

  val queryString: Box[String] = req.queryString

  def param(name: String): List[String] = {
    _params
      .find(_.name == name)
      .map(_.values)
      .getOrElse(Nil)
  }

  def params: List[HTTPParam] = _params

  def paramNames: List[String] = _params.map(_.name)

  /**
   * Destroy the underlying servlet session... null for offline requests
   */
  def destroyServletSession(): Unit = {
    // do nothing here
  }

  val session: HTTPSession = req.session

  val sessionId: Box[String] = req.sessionId

  val remoteAddress: String = req.remoteAddress

  val remotePort: Int = req.remotePort

  val remoteHost: String = req.remoteHost

  val serverName: String = req.serverName

  val scheme: String = req.scheme

  lazy val serverPort: Int = _serverPort match {
    case 80 =>
      headers("X-SSL")
        .flatMap(Helpers.asBoolean)
        .filter(identity)
        .map(_ => 443)
        .headOption
        .getOrElse(80)
    case x => x
  }

  val method: String = req.method

  val resumeInfo : Option[(Req, LiftResponse)] = req.resumeInfo

  def suspend(timeout: Long): RetryState.Value =
    throw new UnsupportedOperationException("Cannot suspend a snapshot")

  def resume(what: (Req, LiftResponse)): Boolean =
    throw new UnsupportedOperationException("Cannot resume a snapshot")

  def suspendResumeSupport_? = false

  def inputStream: InputStream =
    throw new UnsupportedOperationException("InputStream is not available")

  val multipartContent_? : Boolean = req.multipartContent_?

  def extractFiles: List[ParamHolder] =
    throw new UnsupportedOperationException("It is unsafe to extract files")

  val locale: Box[Locale] = req.locale

  def setCharacterEncoding(encoding: String) =
    throw new UnsupportedOperationException("It is unsafe to set the character encoding ")

  def snapshot = this

  /**
   * The User-Agent of the request
   */
  lazy val userAgent: Box[String] =  headers find (_.name equalsIgnoreCase "user-agent") flatMap (_.values.headOption)

}
