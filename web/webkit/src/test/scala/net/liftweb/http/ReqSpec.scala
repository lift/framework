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
package http {

import _root_.net.liftweb.util.Helpers._
import _root_.org.specs._
import _root_.org.specs.runner._
import _root_.org.specs.Sugar._
import scala.xml.NodeSeq
import scala.xml.Text
import Bindings._

import common._

class ReqSpecTest extends Runner(ReqSpec) with JUnit with Console
object ReqSpec extends Specification {
  private val iPhoneUserAgents = 
    List("Mozilla/5.0 (iPhone Simulator; U; CPU iPhone OS 3_0 like Mac OS X; en-us) AppleWebKit/528.18 (KHTML, like Gecko) Version/4.0 Mobile/7A341 Safari/528.16",
         "Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_2_1 like Mac OS X; en-us) AppleWebKit/533.17.9 (KHTML, like Gecko) Version/5.0.2 Mobile/8C148 Safari/6533.18.5")

  private val iPadUserAgents = 
    List("Mozilla/5.0 (iPad; U; CPU OS 3_2 like Mac OS X; en-us) AppleWebKit/531.21.10 (KHTML, like Gecko) Version/4.0.4 Mobile/7B367 Safari/531.21.10",
         "Mozilla/5.0 (iPad; U; CPU OS 4_2_1 like Mac OS X; en-us) AppleWebKit/533.17.9 (KHTML, like Gecko) Version/5.0.2 Mobile/8C148 Safari/6533.18.5")

  "Req" should {
    "recognize safari 5" in {
      val uac = new UserAgentCalculator {
        def userAgent = Full("Mozilla/5.0 (Windows; U; Windows NT 6.1; zh-HK) AppleWebKit/533.18.1 (KHTML, like Gecko) Version/5.0.2 Safari/533.18.5")
      }
      
      uac.safariVersion.open_! must_== 5
    }

    "Do the right thing with iPhone" in {
      iPhoneUserAgents.foreach {
        agent => {
          val uac = new UserAgentCalculator {
            def userAgent = Full(agent)
          }

          uac.isIPhone must_== true
          uac.isIPad must_== false
        }
      }
    }

    "Do the right thing with iPad" in {
      iPadUserAgents.foreach {
        agent => {
          val uac = new UserAgentCalculator {
            def userAgent = Full(agent)
          }

          uac.isIPhone must_== false
          uac.isIPad must_== true
        }
      }
    }
  }
}

}}
