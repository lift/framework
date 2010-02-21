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
  def loggerNameFor(cls: Class[_]) = {
    val className = cls.getName
    if (className endsWith "$") 
      className.substring(0, className.length - 1)
    else 
      className
  }

  def apply(cls: Class[_]) = new WrappedLogger(LoggerFactory.getLogger(loggerNameFor(cls)))
  def apply(name: String) = new WrappedLogger(LoggerFactory.getLogger(name))
  
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

trait Logged {
  @transient val logger: SLF4JLogger
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
  @transient private val logger: SLF4JLogger = _logger
  protected def _logger = LoggerFactory.getLogger(Logger.loggerNameFor(this.getClass))
  
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

  def debug(msg: => AnyRef) = if (logger.isDebugEnabled) logger.debug(String.valueOf(msg))
  def debug(msg: => AnyRef, t:  Throwable) = if (logger.isDebugEnabled) logger.debug(String.valueOf(msg), t)
  def debug(msg: => AnyRef, marker: Marker) = if (logger.isDebugEnabled) logger.debug(marker, String.valueOf(msg))
  def debug(msg: => AnyRef, t: Throwable, marker: Marker) = if (logger.isDebugEnabled) logger.debug(marker, String.valueOf(msg), t)

  def info(msg: => AnyRef) = if (logger.isInfoEnabled) logger.info(String.valueOf(msg))
  def info(msg: => AnyRef, t: => Throwable) = if (logger.isInfoEnabled) logger.info(String.valueOf(msg), t)
  def info(msg: => AnyRef, marker: Marker) = if (logger.isInfoEnabled) logger.info(marker,String.valueOf(msg))
  def info(msg: => AnyRef, t: Throwable, marker: Marker) = if (logger.isInfoEnabled) logger.info(marker,String.valueOf(msg), t)

  def warn(msg: => AnyRef) = if (logger.isWarnEnabled) logger.warn(String.valueOf(msg))
  def warn(msg: => AnyRef, t: Throwable) = if (logger.isWarnEnabled) logger.warn(String.valueOf(msg), t)
  def warn(msg: => AnyRef, marker: Marker) = if (logger.isWarnEnabled) logger.warn(marker,String.valueOf(msg))
  def warn(msg: => AnyRef, t: Throwable, marker: Marker) = if (logger.isWarnEnabled) logger.warn(marker,String.valueOf(msg), t)

  def error(msg: => AnyRef) = if (logger.isErrorEnabled) logger.error(String.valueOf(msg))
  def error(msg: => AnyRef, t: Throwable) = if (logger.isErrorEnabled) logger.error(String.valueOf(msg), t)
  def error(msg: => AnyRef, marker: Marker) = if (logger.isErrorEnabled) logger.error(marker,String.valueOf(msg))
  def error(msg: => AnyRef, t: Throwable, marker: Marker) = if (logger.isErrorEnabled) logger.error(marker,String.valueOf(msg), t)
}

class WrappedLogger(l: SLF4JLogger) extends Logger {
  override val _logger = l
}

/**
 * Mixin with a nested Logger
 */
trait Loggable {
  @transient val logger = Logger(this.getClass)
}

}
}

