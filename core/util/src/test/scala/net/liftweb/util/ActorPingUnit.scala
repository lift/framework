/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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
package util {

import _root_.org.specs._
import _root_.org.specs.runner._
import _root_.org.specs.util.WaitFor
import _root_.java.util.concurrent._
import Helpers._
import common._

class ActorPingUnitTest extends JUnit4(ActorPingUnit)
object ActorPingUnit extends Specification with PingedService with WaitFor {
  def pingService = {
    // service.start
    ActorPing.schedule(service, Alive, TimeSpan(10))
    waitFor(100.ms)
    service.pinged must beTrue
  }
  "The ActorPing object" can {
    "be restarted twice" in {
      ActorPing.restart
      ActorPing.restart
      pingService
    }
    "be shutdown and restarted" in {
      ActorPing.shutdown
      ActorPing.restart
      pingService
    }
    /* THe ActorPing stuff gracefully restarts
    "be shutdown twice" in {
      ActorPing.shutdown
      ActorPing.shutdown
      pingService must throwAn[ActorPingException]
    }
    */
  }
}

}
}
