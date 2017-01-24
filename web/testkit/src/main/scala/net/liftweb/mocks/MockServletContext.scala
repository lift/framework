/*
 * Copyright 2008-2011 WorldWide Conferencing, LLC
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
package mocks

import common.Logger

import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._

import java.io.PrintWriter
import java.io.StringReader
import java.io.BufferedReader
import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream
import java.io.FileInputStream
import java.io.InputStream
import java.io.StringBufferInputStream
import java.io.File
import java.util.Arrays
import java.util.Date
import java.util.Locale
import java.util.Vector
import javax.servlet._
import javax.servlet.http._

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
  def getInitParameterNames(): java.util.Enumeration[String] = new Vector[String]().elements
  def getAttribute(f: String): Object = null
  def getAttributeNames(): java.util.Enumeration[String]  = new Vector[String]().elements
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

  def getResourcePaths(path: String): java.util.Set[String] = null
  def getServerInfo(): String = null
  def getServlet(name: String): Servlet = null
  def getServletContextName(): String = null
  def getServletNames(): java.util.Enumeration[String] = new Vector[String]().elements
  def getServlets(): java.util.Enumeration[Servlet] = new Vector[Servlet]().elements
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

  def addFilter(x$1: String,x$2: Class[_ <: javax.servlet.Filter]): FilterRegistration.Dynamic = null
  def addFilter(x$1: String,x$2: javax.servlet.Filter): FilterRegistration.Dynamic = null
  def addFilter(x$1: String,x$2: String): FilterRegistration.Dynamic = null

  def addListener(listenerClass: Class[_ <: java.util.EventListener]): Unit = ()
  def addListener[T <: java.util.EventListener](listener: T): Unit = ()
  def addListener(listenerClass: String): Unit = ()

  def addServlet(servletNAme: String, servletClass: Class[_ <: javax.servlet.Servlet]): ServletRegistration.Dynamic = null
  def addServlet(servletName: String, servlet: javax.servlet.Servlet): ServletRegistration.Dynamic = null
  def addServlet(servletName: String, servletClass: String): ServletRegistration.Dynamic = null

  // This remain unimplemented since we can't provide a Null here due toe type restrictions.
  def createFilter[T <: javax.servlet.Filter](filter: Class[T]): T = ???
  def createListener[T <: java.util.EventListener](listener: Class[T]): T = ???
  def createServlet[T <: javax.servlet.Servlet](servletClass: Class[T]): T = ???

  def getDefaultSessionTrackingModes(): java.util.Set[SessionTrackingMode] = Set.empty[SessionTrackingMode]

  def declareRoles(roles: String*): Unit = ()
  def getClassLoader(): ClassLoader = getClass.getClassLoader
  def getEffectiveMajorVersion(): Int = 0
  def getEffectiveMinorVersion(): Int = 0
  def getEffectiveSessionTrackingModes(): java.util.Set[javax.servlet.SessionTrackingMode] = null
  def getFilterRegistration(x$1: String): javax.servlet.FilterRegistration = null
  def getFilterRegistrations(): java.util.Map[String, _ <: javax.servlet.FilterRegistration] = null
  def getJspConfigDescriptor(): javax.servlet.descriptor.JspConfigDescriptor = null
  def getServletRegistration(x$1: String): javax.servlet.ServletRegistration = null
  def getServletRegistrations(): java.util.Map[String, _ <: javax.servlet.ServletRegistration] = null
  def getSessionCookieConfig(): javax.servlet.SessionCookieConfig = null
  def setInitParameter(key: String,value: String): Boolean = true
  def setSessionTrackingModes(trackingModes: java.util.Set[javax.servlet.SessionTrackingMode]): Unit = ()
  def getVirtualServerName(): String = null
}


/**
 * A Mock FilterConfig. Construct with a MockServletContext and pass into
 * LiftFilter.init
 */
class MockFilterConfig(servletContext: ServletContext) extends FilterConfig {
  def getFilterName(): String = "LiftFilter" // as in lift's default web.xml
  def getInitParameter(key: String): String = null
  def getInitParameterNames(): java.util.Enumeration[String]  = new Vector[String]().elements
  def getServletContext(): ServletContext = servletContext
}

/**
 * A FilterChain that does nothing.
 *
 * @author Steve Jenson (stevej@pobox.com)
 */
class DoNothingFilterChain extends FilterChain with Logger {
  def doFilter(req: ServletRequest, res: ServletResponse) { debug("Doing nothing on filter chain") }
}

/**
 * A Mock ServletInputStream. Pass in any ol InputStream like a ByteArrayInputStream.
 *
 * @author Steve Jenson (stevej@pobox.com)
 */
class MockServletInputStream(is: InputStream) extends ServletInputStream {
  def read() = is.read()
  def isFinished(): Boolean = is.available() > 0
  def isReady(): Boolean = true
  def setReadListener(x$1: javax.servlet.ReadListener): Unit = ()
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

  def isReady(): Boolean = true
  def setWriteListener(x$1: javax.servlet.WriteListener): Unit = ()
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
  def getAttributeNames(): java.util.Enumeration[String] = new java.util.Enumeration[String] {
    private val keys = attr.keys.iterator
    def hasMoreElements() = keys.hasNext
    def nextElement(): String = keys.next
  }
  def getSessionContext(): HttpSessionContext = null
  def getMaxInactiveInterval(): Int = maxii
  def setMaxInactiveInterval(i: Int): Unit = maxii = i
  def getServletContext(): ServletContext = null
  def getLastAccessedTime(): Long = 0L
  def getId(): String = null
  def getCreationTime(): Long = creationTime
}
