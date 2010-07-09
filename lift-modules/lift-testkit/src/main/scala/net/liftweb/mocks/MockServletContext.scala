/*
 * Copyright 2008-2010 WorldWide Conferencing, LLC
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
package mocks {

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
  def getInitParameter(f: String): String = null
  def getInitParameterNames(): java.util.Enumeration[Object] = new Vector[AnyRef]().elements
  def getAttribute(f: String): Object = null
  def getAttributeNames(): java.util.Enumeration[Object]  = new Vector[AnyRef]().elements
  def removeAttribute(name: String) {}
  def setAttribute(name: String, o: Object) {}
  def getContext(path: String): ServletContext  = this
  def getMajorVersion() = 2
  def getMimeType(file: String): String = null
  def getMinorVersion() = 3
  def getRealPath(path: String): String = null
  def getNamedDispatcher(name: String): RequestDispatcher = null
  def getRequestDispatcher(path: String): RequestDispatcher = null
  def getResource(path: String): java.net.URL = null
  def getResourceAsStream(path: String): java.io.InputStream = {
    val file = new File(target + path)
    if (file.exists) {
      new FileInputStream(file)
    } else {
      null
    }
  }

  def getResourcePaths(path: String): java.util.Set[Object] = null
  def getServerInfo(): String = null
  def getServlet(name: String): Servlet = null
  def getServletContextName(): String = null
  def getServletNames(): java.util.Enumeration[Object] = new Vector[AnyRef]().elements
  def getServlets(): java.util.Enumeration[Object] = new Vector[AnyRef]().elements
  def log(msg: String, t: Throwable) {
    t.printStackTrace
    log(msg)
  }
  def log(e: Exception, msg: String) {
    e.printStackTrace
    log(msg)
  }
  def log(msg: String) = println("MockServletContext.log: " + msg)
  def getContextPath(): String = null
}


/**
 * A Mock FilterConfig. Construct with a MockServletContext and pass into
 * LiftFilter.init
 */
class MockFilterConfig(servletContext: ServletContext) extends FilterConfig {
  def getFilterName(): String = "LiftFilter" // as in lift's default web.xml
  def getInitParameter(key: String): String = null
  def getInitParameterNames(): java.util.Enumeration[Object]  = new Vector[AnyRef]().elements
  def getServletContext(): ServletContext = servletContext
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
  def read() = is.read()
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
  @volatile protected var values: Map[String, Object] = Map()
  @volatile protected var attr: Map[String, Object] = Map()

  import scala.collection.JavaConversions._

  protected var maxii: Int = 0
  protected var creationTime: Long = System.currentTimeMillis
  def isNew = false
  def invalidate {}
  def getValue(key: String): Object = values.get(key) match {
    case Some(v) => v
    case None => null
  }
  def removeValue(key: String): Unit = values -= key
  def putValue(key: String, value: Object): Unit = values += (key -> value)
  def getAttribute(key: String): Object = attr.get(key) match {
      case Some(v) => v
      case None => null
    }
  def removeAttribute(key: String): Unit = attr -= key
  def setAttribute(key: String, value: Object): Unit = attr += (key -> value)
  def getValueNames(): Array[String] = values.keys.toList.toArray
  def getAttributeNames(): java.util.Enumeration[Object] = new java.util.Enumeration[Object] {
    private val keys = attr.keys.iterator
    def hasMoreElements() = keys.hasNext
    def nextElement(): Object = keys.next
  }
  def getSessionContext(): HttpSessionContext = null
  def getMaxInactiveInterval(): Int = maxii
  def setMaxInactiveInterval(i: Int): Unit = maxii = i
  def getServletContext(): ServletContext = null
  def getLastAccessedTime(): Long = 0L
  def getId(): String = null
  def getCreationTime(): Long = creationTime
}

}
}
