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

class MyTopClass extends Logger {
  val x=1
  debug("Top level class logging")
}

object MyTopObj extends Logger {
  val x=1
  debug("Top level object logging")
}

/**
 * Test relies on logback being on the classpath, so no configuration necessary
 */
object LoggingUnit extends Specification {
  "Logging" can {
    "be mixed directly into object" in {
      object MyObj extends Logger {
        info("direct Hello")
        val x = 2
      }
      MyObj.x must_== 2

      (new MyTopClass).x must_== 1
      MyTopObj.x must_==1
    }
    
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
  }
}

}
}
