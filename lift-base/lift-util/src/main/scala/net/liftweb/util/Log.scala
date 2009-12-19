package net.liftweb.util

/*
 * Copyright 2006-2009 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import _root_.java.util.Properties
import Helpers._
import _root_.org.apache.log4j._
import _root_.org.apache.log4j.xml._
import common._

/**
 * A thin wrapper around log4j.
 */
object Log extends LiftLogger {
  lazy val rootLogger: LiftLogger = LogBoot.loggerByName("lift")

  override def trace(msg: => AnyRef) = rootLogger.trace(msg)
  override def trace(msg: => AnyRef, t: => Throwable) = rootLogger.trace(msg, t)

  override def assertLog(assertion: Boolean, msg: => String) = rootLogger.assertLog(assertion, msg)

  override def isEnabledFor(level: LiftLogLevels.Value) = rootLogger.isEnabledFor(level)
  override def isDebugEnabled = rootLogger.isDebugEnabled
  override def debug(msg: => AnyRef) = rootLogger.debug(msg)
  // override def debugF(msg: => AnyRef) = debug(msg)
  override def debug(msg: => AnyRef, t: => Throwable) = rootLogger.debug(msg, t)

  override def isErrorEnabled = rootLogger.isEnabledFor(LiftLogLevels.Error)
  override def error(msg: => AnyRef) = rootLogger.error(msg)
  // override def errorF(msg: => AnyRef) = error(msg)
  override def error(msg: => AnyRef, t: => Throwable) = rootLogger.error(msg, t)

  override def fatal(msg: AnyRef) = rootLogger.fatal(msg)
  // override def fatalF(msg: AnyRef) = fatal(msg)
  override def fatal(msg: AnyRef, t: Throwable) = rootLogger.fatal(msg, t)

  override def level = rootLogger.level
  override def level_=(level: LiftLogLevels.Value) = rootLogger.level = level
  override def name = rootLogger.name
  // def parent = rootLogger.parent

  override def isInfoEnabled = rootLogger.isInfoEnabled
  override def info(msg: => AnyRef) = rootLogger.info(msg)
  def infoF(msg: => AnyRef) = info(msg)
  override def info(msg: => AnyRef, t: => Throwable) = rootLogger.info(msg, t)


  // def isEnabledFor(level: Priority) = rootLogger.isEnabledFor(level)

  override def isWarnEnabled = rootLogger.isWarnEnabled
  override def warn(msg: => AnyRef) = rootLogger.warn(msg)
  // def warnF(msg: => AnyRef) = warn(msg)
  override def warn(msg: => AnyRef, t: => Throwable) = rootLogger.warn(msg, t)

  def never(msg: => AnyRef) {}
  def neverF(msg: => AnyRef) {}
  def never(msg: => AnyRef, t: => Throwable) {}

  override def isTraceEnabled = rootLogger.isTraceEnabled
}

/**
 * This object provides logging setup utilities.
 *
 * To provide your own log4j configuration,add either a log4j.props file or log4j.xml
 * file to your classpath.
 *
 * If you want to provide a configuration file for a subset of your application
 * or for a specifig environment, Lift expects configuration files to be named
 * in a manner relating to the context in which they are being used. The standard
 * name format is:
 *
 * <pre>
 *   modeName.hostName.userName.filename.extension
 * </pre>
 *
 * with hostName and userName being optional, and modeName being one of
 * 'test', 'staging', 'production', 'pilot', 'profile', or 'default.
 * Thus, if you name your log4j config file 'default.log4j.xml' or
 * 'default.log4j.props' it will be picked up correctly.
 */
object LogBoot {
  lazy val checkConfig: Boolean = loggerSetup()

  var loggerSetup: () => Boolean = _log4JSetup _

  var defaultProps =
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

  def _log4JSetup() =
  {
    def log4jIsConfigured = LogManager.getLoggerRepository.getCurrentLoggers.hasMoreElements
    def findTheFile: Box[(_root_.java.net.URL, String)] = (first(Props.toTry.flatMap(f => List(f()+"log4j.props", f()+"log4j.xml")))
    (name =>tryo(getClass.getResource(name)).filter(_ ne null).map(s => (s, name))))

    val (log4jUrl, fileName) = findTheFile match {
      case Full((url, name)) => (Full(url), Full(name))
      case _ => (Empty, Empty)
    }

    for (url <- log4jUrl; name <- fileName) {
      if (name.endsWith(".xml")) {
        val domConf = new DOMConfigurator
        domConf.doConfigure(url, LogManager.getLoggerRepository())
      } else PropertyConfigurator.configure(url)
    }

    if (!log4jUrl.isDefined && !log4jIsConfigured) {
      val domConf = new DOMConfigurator
      val defPropBytes = defaultProps.toString.getBytes("UTF-8")
      val is = new _root_.java.io.ByteArrayInputStream(defPropBytes)
      domConf.doConfigure(is, LogManager.getLoggerRepository())
    }
    true
  }

  private def _loggerCls(clz: Class[AnyRef]): LiftLogger = if (checkConfig) new Log4JLogger(Logger.getLogger(clz)) else NullLogger
  private def _logger(name: String): LiftLogger = if (checkConfig) new Log4JLogger(Logger.getLogger(name)) else NullLogger

  var loggerByName: String => LiftLogger = _logger
  var loggerByClass: Class[AnyRef] => LiftLogger = _loggerCls _
}

object NullLogger extends LiftLogger {

}

trait LiftLogger {
  def isTraceEnabled: Boolean = false
  def trace(msg: => AnyRef): Unit = ()
  def trace(msg: => AnyRef, t: => Throwable): Unit = ()

  def assertLog(assertion: Boolean, msg: => String): Unit = ()

  def isDebugEnabled: Boolean = false
  def debug(msg: => AnyRef): Unit = ()
  def debug(msg: => AnyRef, t: => Throwable): Unit = ()

  def isErrorEnabled: Boolean = false
  def error(msg: => AnyRef): Unit = ()
  def error(msg: => AnyRef, t: => Throwable): Unit = ()

  def fatal(msg: AnyRef): Unit = ()
  def fatal(msg: AnyRef, t: Throwable): Unit = ()

  def level: LiftLogLevels.Value = LiftLogLevels.Off
  def level_=(level: LiftLogLevels.Value): Unit = ()
  def name: String = "Null"
  // def parent = logger.getParent

  def isInfoEnabled: Boolean = false
  def info(msg: => AnyRef): Unit = ()
  def info(msg: => AnyRef, t: => Throwable): Unit = ()

  def isEnabledFor(level: LiftLogLevels.Value): Boolean = false

  def isWarnEnabled: Boolean = false
  def warn(msg: => AnyRef): Unit = ()
  def warn(msg: => AnyRef, t: => Throwable): Unit = ()
}

object LiftLogLevels extends Enumeration {
  val All = Value(1, "All")
  val Trace = Value(3, "Trace")
  val Debug = Value(5, "Debug")
  val Warn = Value(7, "Warn")
  val Error = Value(9, "Error")
  val Fatal = Value(11, "Fatal")
  val Info = Value(13, "Info")
  val Off = Value(15, "Off")
}

class Log4JLogger(val logger: Logger) extends LiftLogger {
  override def isTraceEnabled = logger.isTraceEnabled
  override def trace(msg: => AnyRef) = if (isTraceEnabled) logger.trace(msg)
  override def trace(msg: => AnyRef, t: => Throwable) = if (isTraceEnabled) logger.trace(msg, t)

  override def assertLog(assertion: Boolean, msg: => String) = if (assertion) logger.assertLog(assertion, msg)

  override def isDebugEnabled = logger.isDebugEnabled
  override def debug(msg: => AnyRef) = if (isDebugEnabled) logger.debug(msg)
  override def debug(msg: => AnyRef, t: => Throwable) = if (isDebugEnabled) logger.debug(msg, t)

  override def isErrorEnabled = logger.isEnabledFor(Level.ERROR)
  override def error(msg: => AnyRef) = if (isErrorEnabled) logger.error(msg)
  override def error(msg: => AnyRef, t: => Throwable) = if (isErrorEnabled) logger.error(msg, t)

  override def fatal(msg: AnyRef) = logger.fatal(msg)
  override def fatal(msg: AnyRef, t: Throwable) = logger.fatal(msg, t)

  override def level = logger.getLevel match {
    case Level.ALL => LiftLogLevels.All
    case Level.DEBUG => LiftLogLevels.Debug
    case Level.ERROR => LiftLogLevels.Error
    case Level.WARN => LiftLogLevels.Warn
    case Level.FATAL => LiftLogLevels.Fatal
    case Level.INFO => LiftLogLevels.Info
    case Level.TRACE => LiftLogLevels.Trace
    case Level.OFF => LiftLogLevels.Off
  }

  val liftToLog4J: PartialFunction[LiftLogLevels.Value, Level] = {
    case LiftLogLevels.All => Level.ALL
    case LiftLogLevels.Debug => Level.DEBUG
    case LiftLogLevels.Error => Level.ERROR
    case LiftLogLevels.Warn => Level.WARN
    case LiftLogLevels.Fatal => Level.FATAL
    case LiftLogLevels.Info => Level.INFO
    case LiftLogLevels.Trace => Level.TRACE
    case LiftLogLevels.Off => Level.OFF
  }

  override def isEnabledFor(level: LiftLogLevels.Value): Boolean = logger.isEnabledFor(liftToLog4J(level))
  override def level_=(level: LiftLogLevels.Value) = logger.setLevel(liftToLog4J(level) )
  override def name = logger.getName

  override def isInfoEnabled = logger.isInfoEnabled
  override def info(msg: => AnyRef) = if (isInfoEnabled) logger.info(msg)
  override def info(msg: => AnyRef, t: => Throwable) = if (isInfoEnabled) logger.info(msg, t)

  def isEnabledFor(level: Priority) = logger.isEnabledFor(level)

  override def isWarnEnabled = isEnabledFor(Level.WARN)
  override def warn(msg: => AnyRef) = if (isWarnEnabled) logger.warn(msg)
  override def warn(msg: => AnyRef, t: => Throwable) = if (isWarnEnabled) logger.warn(msg, t)
}
