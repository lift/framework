package net.liftweb.mocks

import _root_.scala.collection.mutable.HashMap
import _root_.java.io.PrintWriter
import _root_.java.io.StringReader
import _root_.java.io.BufferedReader
import _root_.java.io.ByteArrayOutputStream
import _root_.java.io.ByteArrayInputStream
import _root_.java.io.FileInputStream
import _root_.java.io.InputStream
import _root_.java.io.StringBufferInputStream
import _root_.java.io.File
import _root_.java.util.Arrays
import _root_.java.util.Date
import _root_.java.util.Locale
import _root_.java.util.Vector
import _root_.javax.servlet._
import _root_.javax.servlet.http._
import _root_.net.liftweb.util.Helpers

/**
 * A Mock ServletRequest. Change it's state to to create the request you are
 * interested in. At the very least, you will need to change method and path.
 *
 * @author Steve Jenson (stevej@pobox.com)
 */
class MockHttpServletRequest extends HttpServletRequest {
  var session : HttpSession = new MockHttpSession
  var queryString: String = ""
  var contextPath = ""
  var path = ""
  var method = "GET"
  val headers: _root_.scala.collection.jcl.HashMap[String, String] =
    new _root_.scala.collection.jcl.HashMap[String, String](new _root_.java.util.HashMap)
  val attr: _root_.scala.collection.jcl.HashMap[String, Any] =
    new _root_.scala.collection.jcl.HashMap[String, Any](new _root_.java.util.HashMap)
  var cookies: List[Cookie] = Nil
  var authType = null
  var localPort = 0
  var localAddr = null
  var localName = null
  var remotePort = 0
  var remoteHost = null
  var remoteAddr = null
  var locale = Locale.getDefault
  var reader: BufferedReader = new BufferedReader(new StringReader(method + " " + path +  "/\r\n\r\n"))
  var serverPort = 0
  var serverName = null
  var scheme = "http"
  var protocol = "http 1.0"
  var parameterMap: _root_.scala.collection.jcl.HashMap[String, String] =
    new _root_.scala.collection.jcl.HashMap[String, String](new _root_.java.util.HashMap)
  val sbis = new StringBufferInputStream("")
  var inputStream: ServletInputStream = new MockServletInputStream(sbis)
  var contentType = null
  var contentLength = 0
  var charEncoding = "ISO-8859-1" // HTTP's default encoding

  def isRequestedSessionIdFromURL = false
  def isRequestedSessionIdFromUrl = false
  def isRequestedSessionIdFromCookie = false
  def isRequestedSessionIdValid = false
  def getSession(p: Boolean) = {
    session
  }
  def getSession = getSession(false)
  def getServletPath = ""
  def getRequestURL = new StringBuffer(path)
  def getRequestURI = path
  def getRequestedSessionId = null
  def getUserPrincipal = null
  def isUserInRole(user: String): Boolean = false
  def getRemoteUser = ""
  def getQueryString = queryString
  def getContextPath = contextPath
  def getPathTranslated = path
  def getPathInfo = path
  def getMethod = method
  def getIntHeader(h: String): Int = {
    Helpers.toInt(headers(h))
  }
  def getHeaderNames = {
    new Vector[AnyRef](headers.underlying.keySet).elements
  }
  def getHeaders = headers
  def getHeaders(s: String) = {
    val v = new Vector[AnyRef]()
    v.add(headers(s))
    v.elements
  }
  def getHeader(h: String) = headers.get(h) match {
    case Some(v) => v
    case None => null
  }
  def getDateHeader(h: String): Long = {
    Helpers.toLong(headers(h))
  }
  def setDateHeader(s: String, l: Long) {
    headers += (s -> l.toString)
  }
  def getCookies = cookies.toArray
  def getAuthType = authType
  def getLocalPort = localPort
  def getLocalAddr = localAddr
  def getLocalName = localName
  def getRemotePort = remotePort
  def getRealPath(s: String) = s
  def getRequestDispatcher(s: String): RequestDispatcher = null
  def isSecure = false
  type ZZ = Q forSome {type Q}
  def getLocales = new Vector[ZZ](Arrays.asList(Locale.getAvailableLocales : _*)).elements
  def getLocale = locale
  def removeAttribute(key: String) = attr -= key
  def setAttribute(key: String, value: Any) = attr += (key -> value)
  def getRemoteHost = remoteHost
  def getRemoteAddr = remoteAddr
  def getReader = reader
  def getServerPort = serverPort
  def getServerName = serverName
  def getScheme = scheme
  def getProtocol = protocol
  def getParameterMap = parameterMap.underlying
  def getParameterValues(key: String) =
    parameterMap.underlying.values.toArray.asInstanceOf[Array[String]]
  def getParameterNames = new Vector[ZZ](parameterMap.underlying.keySet.asInstanceOf[_root_.java.util.Set[ZZ]]).elements
  def getParameter(key: String) = parameterMap(key)
  def getInputStream = inputStream
  def getContentType = contentType
  def getContentLength = contentLength
  def getCharacterEncoding = charEncoding
  def setCharacterEncoding(enc: String) = charEncoding = enc
  def getAttributeNames = new Vector[ZZ](attr.underlying.keySet.asInstanceOf[_root_.java.util.Set[ZZ]]).elements
  def getAttribute(key: String) = attr(key).asInstanceOf[Object]
}
