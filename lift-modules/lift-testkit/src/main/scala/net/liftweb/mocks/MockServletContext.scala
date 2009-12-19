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

/**
 * An example of how to use these mock classes in your unit tests:
 *
 *   def testLiftCore = {
 *     val output = new ByteArrayOutputStream
 *     val outputStream = new MockServletOutputStream(output)
 *     val writer = new PrintWriter(outputStream)
 *
 *     val req = new MockHttpServletRequest
 *     req.method = "GET"
 *     req.path = "/"
 *     val res = new MockHttpServletResponse(writer, outputStream)
 *
 *     val filter = new LiftFilter
 *     filter.init(new MockFilterConfig(new MockServletContext("target/test1-1.0-SNAPSHOT")))
 *     filter.doFilter(req, res,new DoNothingFilterChain)
 *     assertTrue(output.toString.startsWith("<?xml"))
 *   }
 */



/**
 * A Mock ServletContext. LiftFilter expects a ServletContext inside a FilterConfig
 *
 * @param target the target directory where your template files live
 *
 * @author Steve Jenson (stevej@pobox.com)
 */
class MockServletContext(var target: String) extends ServletContext {
  def getInitParameter(f: String) = null
  def getInitParameterNames = new Vector[AnyRef]().elements
  def getAttribute(f: String) = null
  def getAttributeNames = new Vector[AnyRef]().elements
  def removeAttribute(name: String) {}
  def setAttribute(name: String, o: Object) {}
  def getContext(path: String) = this
  def getMajorVersion = 2
  def getMimeType(file: String) = null
  def getMinorVersion = 3
  def getRealPath(path: String) = null
  def getNamedDispatcher(name: String) = null
  def getRequestDispatcher(path: String) = null
  def getResource(path: String) = null
  def getResourceAsStream(path: String) = {
    val file = new File(target + path)
    if (file.exists) {
      new FileInputStream(file)
    } else {
      null
    }
  }

  def getResourcePaths(path: String) = null
  def getServerInfo = null
  def getServlet(name: String) = null
  def getServletContextName = null
  def getServletNames = new Vector[AnyRef]().elements
  def getServlets = new Vector[AnyRef]().elements
  def log(msg: String, t: Throwable) {
    t.printStackTrace
    log(msg)
  }
  def log(e: Exception, msg: String) {
    e.printStackTrace
    log(msg)
  }
  def log(msg: String) = println("MockServletContext.log: " + msg)
  def getContextPath = null
}


/**
 * A Mock FilterConfig. Construct with a MockServletContext and pass into
 * LiftFilter.init
 */
class MockFilterConfig(servletContext: ServletContext) extends FilterConfig {
  def getFilterName = "LiftFilter" // as in lift's default web.xml
  def getInitParameter(key: String) = null
  def getInitParameterNames  = new Vector[AnyRef]().elements
  def getServletContext = servletContext
}

/**
 * A FilterChain that does nothing.
 *
 * @author Steve Jenson (stevej@pobox.com)
 */
class DoNothingFilterChain extends FilterChain {
  def doFilter(req: ServletRequest, res: ServletResponse) {println("doing nothing")}
}

/**
 * A Mock ServletInputStream. Pass in any ol InputStream like a ByteArrayInputStream.
 *
 * @author Steve Jenson (stevej@pobox.com)
 */
class MockServletInputStream(is: InputStream) extends ServletInputStream {
  def read = is.read()
}

/**
 * A Mock ServletOutputStream. Pass in any ol' OutputStream like a ByteArrayOuputStream.
 *
 * @author Steve Jenson (stevej@pobox.com)
 */
class MockServletOutputStream(os: ByteArrayOutputStream) extends ServletOutputStream {
  def write(b: Int) {
    os.write(b)
  }
}

/**
 * A Mock HttpSession implementation.
 *
 * @author Steve Jenson (stevej@pobox.com)
 */
class MockHttpSession extends HttpSession {
  val values = new _root_.scala.collection.jcl.HashMap[String, Any](new _root_.java.util.HashMap)
  val attr = new _root_.scala.collection.jcl.HashMap[String, Any](new _root_.java.util.HashMap)
  val sessionContext = null
  var maxii = 0
  var servletContext = null
  var creationTime = System.currentTimeMillis
  def isNew = false
  def invalidate {}
  def getValue(key: String) = values.get(key) match {
    case Some(v) => v.asInstanceOf[Object]
    case None => Nil
  }
  def removeValue(key: String) = values -= key
  def putValue(key: String, value: Any) = values += (key -> value)
  def getAttribute(key: String) = attr.get(key) match {
      case Some(v) => v.asInstanceOf[Object]
      case None => Nil
    }
  def removeAttribute(key: String) = attr -= key
  def setAttribute(key: String, value: Any) = attr += (key -> value)
  def getValueNames: Array[String] = values.keySet.toArray
  def getAttributeNames = new Vector[AnyRef](attr.underlying.keySet).elements
  def getSessionContext = sessionContext
  def getMaxInactiveInterval = maxii
  def setMaxInactiveInterval(i: Int) = maxii = i
  def getServletContext = servletContext
  def getLastAccessedTime = 0L
  def getId = null
  def getCreationTime = creationTime
}

