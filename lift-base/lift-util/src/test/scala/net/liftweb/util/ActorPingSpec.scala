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
import _root_.org.specs.util.WaitFor
import _root_.org.specs.runner._
import _root_.java.util.concurrent._
import Helpers._
import common._
import net.liftweb.actor._

class ActorPingSpecTest extends JUnit4(ActorPingSpec)
object ActorPingSpec extends Specification with PingedService with WaitFor {
  "The ActorPing object" should { doBefore { ActorPing.restart }
    "provide a schedule method to ping an actor regularly" in {
      // service.start
      ActorPing.schedule(service, Alive, TimeSpan(10))
      waitFor(100.ms)
      service.pinged must beTrue
    }
  }
}
trait PingedService {
  case object Alive
  val service = new Service

  class Service extends LiftActor {
    @volatile var pinged = false
    /*
    def act() {
      while (true) {
        receive {
          case Alive => {pinged = true; exit()}
        }
      }
    }
    */
    protected def messageHandler = {
          case Alive => {pinged = true /*; exit() */}
    }
  }
}

}
}
