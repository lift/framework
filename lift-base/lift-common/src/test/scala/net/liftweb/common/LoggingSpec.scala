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

import _root_.org.specs._
import _root_.org.specs.log.{Log => _}
import _root_.net.liftweb.common.Box._
import _root_.org.specs.runner._
import _root_.org.specs.Sugar._
import _root_.org.specs.ScalaCheck
import _root_.org.scalacheck.Gen._
import _root_.org.scalacheck._
import _root_.org.scalacheck.Arbitrary._
import _root_.org.scalacheck.Prop.{forAll}

class LoggingTest extends Runner(LoggingUnit) with JUnit
class LoggingUnitTest extends JUnit4(LoggingUnit)

class MyTopClass extends Logger {
  val x=1
  debug("Top level class logging")
}

object MyTopObj extends Logger {
  val x=1
  debug("Top level object logging")
}

object LoggingUnit extends Specification {
  "Logging" can {
    doFirst {
      import _root_.org.apache.log4j._
      import _root_.org.apache.log4j.xml._
      val  defaultProps =
        """<?xml version="1.0" encoding="UTF-8" ?>
        <!DOCTYPE log4j:configuration SYSTEM "log4j.dtd">
        <log4j:configuration xmlns:log4j="http://jakarta.apache.org/log4j/">
        <appender name="appender" class="org.apache.log4j.ConsoleAppender">
        <layout class="org.apache.log4j.PatternLayout">
          <param name="ConversionPattern" value="%-5p %c M1:%X{mdc1} M2:%X{mdc2}- %m%n"/>
        </layout>
        </appender>
        <root>
        <priority value ="trace"/>
        <appender-ref ref="appender"/>
        </root>
        </log4j:configuration>
        """
      val domConf = new DOMConfigurator
      val defPropBytes = defaultProps.toString.getBytes("UTF-8")
      val is = new _root_.java.io.ByteArrayInputStream(defPropBytes)
      domConf.doConfigure(is, LogManager.getLoggerRepository())
    }

    "be mixed directly into object" in {
      object MyObj extends Logger {
        info("direct Hello")
        val x = 2
      }
      MyObj.x must_== 2

      (new MyTopClass).x must_== 1
      MyTopObj.x must_==1
    }
    /* FIXME: 280
    "be nested in object" in {
      object MyObj extends Loggable {
        logger.info("nested Hello")
        val x = 2
      }

      MyObj.x must_== 2

    }

    "create named loggers" in {
      val logger = Logger("MyLogger")

      logger.info("Logged with my named logger")
      1 must_== 1
    }

    "log static MDC values" in {
      val logger = Logger("StaticMDC")

      logger.info("Logged with no MDC")
      MDC.put("mdc1" -> (1,2))
      logger.info("Logged with mdc1=(1,2)")
      MDC.put("mdc2" -> "yy")
      logger.info("Logged with mdc1=(1,2), mdc2=yy")
      MDC.put("mdc1" -> 99)
      logger.info("Logged with mdc1=99, mdc2=yy")
      MDC.remove("mdc1")
      logger.info("Logged with mdc2=yy")
      MDC.clear()
      logger.info("Logged with no MDC")
      1 must_== 1
    }

    "save MDC context with logWith" in {
      val logger = Logger("logWith")

      logger.info("Logged with no MDC")
      MDC.put("mdc1" -> (1,2), "mdc2" -> "yy")
      logger.info("Logged with mdc1=(1,2), mdc2=yy")
      Logger.logWith("mdc2" -> "xx") {
        logger.info("Logged with mdc1=(1,2), mdc2=xx")
        Logger.logWith("mdc1" -> 99) {
           logger.info("Logged with mdc1=99, mdc2=xx")
        }
        logger.info("Logged with mdc1=(1,2), mdc2=xx")
      }
      logger.info("Logged with mdc1=(1,2), mdc2=yy")
      MDC.clear
      logger.info("No MDC values")
      1 must_== 1
    }
    "trace function results" in {
      object MyObj extends Logger {
          val l = 1 to 10
          info("Starting test")
          trace("result",l.foldLeft(0)(trace("lhs",_) + trace("rhs",_))) must_== l.foldLeft(0)(_+_)
          val x = 1
      }
      MyObj.x
    }

    "be used in different levels and yield different loggers" in {
      class First  {
        First.info("In first")
      }
      object First extends Logger

      trait Second {
        private val logger = Logger(classOf[Second])
        logger.info("In second")
      }

      class C extends First with Second with Logger {
        info("In C")
        val x = 2
      }
      (new C).x must_== 2
    }
    */
  }
}

}
}
