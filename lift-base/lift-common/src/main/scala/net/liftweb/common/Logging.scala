/*
 * Copyright 2010 WorldWide Conferencing, LLC
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
package common {
  
import _root_.org.slf4j.{MDC => SLF4JMDC, Marker, Logger => SLF4JLogger, LoggerFactory}

object Logger {
  private[common] lazy val checkConfig: Boolean = {
    setup.foreach {_()}; 
    true
  }
  
  /**
   * This function, if set, will be called before any loggers are created
   * 
   * Useful for initializing the logging backend with a non-default configuration
   * 
   * Helpers exists for log4j & logback:
   * 
   *   Logger.setup = Full(Log4j.withFile(url)
   * 
   * or
   * 
   *   Logger.setup = Full(Logback.withFile(url))
   * 
   */
  var setup: Box[() => Unit] = Empty
  
  def loggerNameFor(cls: Class[_]) = {
    val className = cls.getName
    if (className endsWith "$") 
      className.substring(0, className.length - 1)
    else 
      className
  }

  def apply(cls: Class[_]): Logger = if (checkConfig) new WrappedLogger(LoggerFactory.getLogger(loggerNameFor(cls))) else null
  def apply(name: String): Logger = if (checkConfig) new WrappedLogger(LoggerFactory.getLogger(name)) else null
  
 /**
   * Set the Mapped Diagnostic Context for the thread and execute
   * the function f
   *
   * Upon return, the MDC is cleared of the values passed (any
   * MDC values that existed prior to this call remains)
   */
  def logWith[F](mdcValues: (String,Any)*)(f: => F): F = {
    val old = SLF4JMDC.getCopyOfContextMap
    MDC.put(mdcValues:_*)
    try {
      f
    } finally {
      if (old eq null) 
        MDC.clear
      else  
        SLF4JMDC.setContextMap(old)
    }
  }
}

/**
 * The Mapped Diagnostics Context can hold values per thread and output them
 * with each logged output.
 * 
 * The logging backend needs to be configured to log these values
 */
object MDC {
  /**
   * Put a (key,value) pair into the Mapped Diagnostic Context
   */
  def put(kvs: (String,Any)*) = {
    kvs foreach {v => SLF4JMDC.put(v._1, v._2.toString)}
  }

  /**
   * Clear key from the Mapped Diagnostic Context
   */
  def remove(keys: String*) = {
    keys foreach {k => SLF4JMDC.remove(k)}
  }

  /**
   * Clear all entries from the Mapped Diagnostic Context
   */
  def clear() = org.slf4j.MDC.clear
}

/**
 * Logger is a thin wrapper on top of an SLF4J Logger
 *
 * The main purpose is to utilize Scala features for logging
 * 
 * Note that the dynamic type of "this" is used when this trait is
 * mixed in. 
 * 
 * This may not always be what you want. If you need the static type, you have to declare your
 * own Logger:
 * 
 * class MyClass {
 *   val logger = Logger(classOf[MyClass])
 * }
 * 
 */
trait Logger  {
  private lazy val logger: SLF4JLogger = _logger // removed @transient 'cause there's no reason for transient on val
  // changed to lazy val so it only gets initialized on use rather than on instantiation
  protected def _logger = if (Logger.checkConfig) LoggerFactory.getLogger(Logger.loggerNameFor(this.getClass)) else null
  
  def assertLog(assertion: Boolean, msg: => String) = if (assertion) info(msg)

  /**
   * Log the value of v with trace and return v. Useful for tracing values in expressions
   */
  def trace[T](msg: String, v: T): T = {
    logger.trace(msg+": "+v.toString)
    v
  }
  
  def trace(msg: => AnyRef) = if (logger.isTraceEnabled) logger.trace(String.valueOf(msg))
  def trace(msg: => AnyRef, t: Throwable) = if (logger.isTraceEnabled) logger.trace(String.valueOf(msg), t)
  def trace(msg: => AnyRef, marker:  Marker) = if (logger.isTraceEnabled) logger.trace(marker,String.valueOf(msg))
  def trace(msg: => AnyRef, t: Throwable, marker: => Marker) = if (logger.isTraceEnabled) logger.trace(marker,String.valueOf(msg), t)
  def isTraceEnabled = logger.isTraceEnabled
  
  def debug(msg: => AnyRef) = if (logger.isDebugEnabled) logger.debug(String.valueOf(msg))
  def debug(msg: => AnyRef, t:  Throwable) = if (logger.isDebugEnabled) logger.debug(String.valueOf(msg), t)
  def debug(msg: => AnyRef, marker: Marker) = if (logger.isDebugEnabled) logger.debug(marker, String.valueOf(msg))
  def debug(msg: => AnyRef, t: Throwable, marker: Marker) = if (logger.isDebugEnabled) logger.debug(marker, String.valueOf(msg), t)
  def isDebugEnabled = logger.isDebugEnabled
  
  def info(msg: => AnyRef) = if (logger.isInfoEnabled) logger.info(String.valueOf(msg))
  def info(msg: => AnyRef, t: => Throwable) = if (logger.isInfoEnabled) logger.info(String.valueOf(msg), t)
  def info(msg: => AnyRef, marker: Marker) = if (logger.isInfoEnabled) logger.info(marker,String.valueOf(msg))
  def info(msg: => AnyRef, t: Throwable, marker: Marker) = if (logger.isInfoEnabled) logger.info(marker,String.valueOf(msg), t)
  def isInfoEnabled = logger.isInfoEnabled
  
  def warn(msg: => AnyRef) = if (logger.isWarnEnabled) logger.warn(String.valueOf(msg))
  def warn(msg: => AnyRef, t: Throwable) = if (logger.isWarnEnabled) logger.warn(String.valueOf(msg), t)
  def warn(msg: => AnyRef, marker: Marker) = if (logger.isWarnEnabled) logger.warn(marker,String.valueOf(msg))
  def warn(msg: => AnyRef, t: Throwable, marker: Marker) = if (logger.isWarnEnabled) logger.warn(marker,String.valueOf(msg), t)
  def isWarnEnabled = logger.isWarnEnabled
  
  def error(msg: => AnyRef) = if (logger.isErrorEnabled) logger.error(String.valueOf(msg))
  def error(msg: => AnyRef, t: Throwable) = if (logger.isErrorEnabled) logger.error(String.valueOf(msg), t)
  def error(msg: => AnyRef, marker: Marker) = if (logger.isErrorEnabled) logger.error(marker,String.valueOf(msg))
  def error(msg: => AnyRef, t: Throwable, marker: Marker) = if (logger.isErrorEnabled) logger.error(marker,String.valueOf(msg), t)
  def isErrorEnabled = logger.isErrorEnabled
  
}

class WrappedLogger(l: SLF4JLogger) extends Logger {
  override def _logger = l
}

/**
 * Mixin with a nested Logger
 */
trait Loggable {
  @transient protected val logger = Logger(this.getClass)
}

/**
 * Mixin with a nested lazy Logger
 * 
 * Useful for mixin to objects that are created before Lift has booted (and thus Logging is not yet configured)
 */
trait LazyLoggable {
  @transient protected lazy val logger = Logger(this.getClass)
}

/**
 * Configuration helpers for the log4j logging backend
 */
object Log4j {
  import org.apache.log4j.{LogManager,PropertyConfigurator}
  import org.apache.log4j.xml.DOMConfigurator
  
  val defaultProps =
    """<?xml version="1.0" encoding="UTF-8" ?>
    <!DOCTYPE log4j:configuration SYSTEM "log4j.dtd">
    <log4j:configuration xmlns:log4j="http://jakarta.apache.org/log4j/">
    <appender name="appender" class="org.apache.log4j.ConsoleAppender">
    <layout class="org.apache.log4j.SimpleLayout"/>
    </appender>
    <root>
    <priority value ="INFO"/>
    <appender-ref ref="appender"/>
    </root>
    </log4j:configuration>
    """
  
  /**
   * Configure with contents of the specified filed (either .xml or .properties)
   */
  def withFile(url: java.net.URL)() = {
    if (url.getPath.endsWith(".xml")) {
      val domConf = new DOMConfigurator
      domConf.doConfigure(url, LogManager.getLoggerRepository())
    } else 
      PropertyConfigurator.configure(url)
  }
  /**
   * Configure with the specified configuration. config must contain a valid XML document
   */
  def withConfig(config: String)() = {
    val domConf = new DOMConfigurator
    val is = new java.io.ByteArrayInputStream(config.getBytes("UTF-8"))
    domConf.doConfigure(is, LogManager.getLoggerRepository())
  }
  
  /**
   * Configure with simple defaults
   */
  def withDefault() = withConfig(defaultProps)
}

/**
 * Configuration helpers for the Logback logging backend
 */
object Logback  {
  import ch.qos.logback.classic.LoggerContext;
  import ch.qos.logback.core.util.StatusPrinter;
  import ch.qos.logback.classic.joran.JoranConfigurator;

  /**
   * Configure with contents of the specified XML filed 
   */
  def withFile(url: java.net.URL)() = {
    val lc = LoggerFactory.getILoggerFactory().asInstanceOf[LoggerContext];
    val configurator = new JoranConfigurator();
    configurator.setContext(lc);
    // the context was probably already configured by default configuration rules
    lc.reset(); 
    configurator.doConfigure(url);
  }
}

}
}

