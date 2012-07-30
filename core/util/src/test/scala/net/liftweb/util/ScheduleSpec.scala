/*
 * Copyright 2007-2011 WorldWide Conferencing, LLC
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
package util

import org.specs2.mutable.Specification
import org.specs2.specification.BeforeExample
import org.specs2.execute.PendingUntilFixed

import actor._
import Helpers._


/**
 * Systems under specification for Lift Schedule.
 */
object ScheduleSpec extends Specification with PendingUntilFixed with PingedService with BeforeExample {
  "Schedule Specification".title

  def before = Schedule.restart

  "The Schedule object" should {
    "provide a schedule method to ping an actor regularly" in {
      Schedule.schedule(service, Alive, TimeSpan(10))
      service.pinged must eventually(beTrue)
    }
    "honor multiple restarts" in {
      Schedule.restart
      Schedule.restart
      Schedule.restart
      Schedule.schedule(service, Alive, TimeSpan(10))
      service.pinged must eventually(beTrue)
    }
    "honor shutdown followed by restart" in {
      Schedule.shutdown
      Schedule.restart
      Schedule.schedule(service, Alive, TimeSpan(10))
      service.pinged must eventually(beTrue)
    }
    "not honor multiple shutdowns" in {
      Schedule.shutdown
      Schedule.shutdown
//      service.pinged must eventually(beFalse)
      service.pinged must throwA[ActorPingException]
    }.pendingUntilFixed
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
