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
package webapptest {

import _root_.org.specs._
import _root_.org.specs.runner.JUnit3
import _root_.org.specs.runner.ConsoleRunner
import _root_.net.sourceforge.jwebunit.junit.WebTester
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.testing._
import _root_.net.liftweb.util._
import Helpers._

import net.liftweb.webapptest.snippet.Counter

class OneShotTest extends JUnit3(OneShot)
object OneShotRunner extends ConsoleRunner(OneShot)


object OneShot extends Specification with RequestKit {
  JettyTestServer.start()

  def baseUrl = JettyTestServer.baseUrl

  "OneShot" should {
    "fire once for oneshot" >> {
      Counter.x = 0

      for {
        resp <- get("/oneshot")
        xml <- resp.xml
        span <- (xml \\ "span").filter(x => (x \ "@id").text == "one")
        in <- (span \\ "input")
        name <- in \ "@name"
      } {

        resp.get("/oneshot?"+urlEncode(name.text)+"=3")
        resp.get("/oneshot?"+urlEncode(name.text)+"=3")
      }

      Counter.x must_== 1
    }

    "fire multiple times for normal" >> {
      Counter.x = 0

      for {
        resp <- get("/oneshot")
        xml <- resp.xml
        span <- (xml \\ "span").filter(x => (x \ "@id").text == "two")
        in <- (span \\ "input")
        name <- in \ "@name"
      } {
        resp.get("/oneshot?"+urlEncode(name.text)+"=3")
        resp.get("/oneshot?"+urlEncode(name.text)+"=3")
      }


      Counter.x must_== 2
    }
  }
  //  JettyTestServer.stop()
}

}
}
