package net.liftweb.http.provider.servlet


import _root_.scala.collection.mutable.{ListBuffer}
import _root_.java.io.{OutputStream}
import _root_.javax.servlet.http.{HttpServletResponse, Cookie}
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._

class HTTPResponseServlet(resp: HttpServletResponse) extends HTTPResponse {
  def addCookies(cookies: List[HTTPCookie]) = cookies.foreach {
    case c =>
      val cookie = new javax.servlet.http.Cookie(c.name, c.value openOr null)
      c.domain map (cookie.setDomain(_))
      c.path map (cookie.setPath(_))
      c.maxAge map (cookie.setMaxAge(_))
      c.version map (cookie.setVersion(_))
      c.secure_? map (cookie.setSecure(_))
      resp.addCookie(cookie)
  }

  def encodeUrl(url: String): String = resp encodeURL url

  def addHeaders(headers: List[HTTPParam]) {
    val appearOnce = Set(LiftRules.overwrittenReponseHeaders.vend.map(_.toLowerCase): _*)
    for (h <- headers;
         value <- h.values) {
      if (appearOnce.contains(h.name.toLowerCase)) resp.setHeader(h.name, value)
      else
        resp.addHeader(h.name, value)
    }
  }

  def setStatus(status: Int) = resp setStatus status

  def outputStream: OutputStream = resp getOutputStream
}
