/*
 * Copyright 2010-2015 WorldWide Conferencing, LLC
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
package http

import scala.xml.NodeSeq
import net.liftweb.common.{Full, Empty}
import org.specs2.specification.BeforeEach
import org.specs2.mutable.Specification

object LiftSessionSpec extends Specification with BeforeEach {
  sequential

  private var receivedMessages = Vector[Int]()
  private object NoOp

  override def before = receivedMessages = Vector[Int]()

  private class TestCometActor extends CometActor {
    def render = NodeSeq.Empty

    override def lowPriority = {
      case n: Int =>
        receivedMessages :+= n
      case NoOp =>
        reply(NoOp)
      case _ =>
    }
  }

  "A LiftSession" should {

    "Send accumulated messages to a newly-created comet actor in the order in which they arrived" in {
      val session = new LiftSession("Test Session", "", Empty)

      S.init(Empty, session) {
        val cometName = "TestCometActor"
        val sendingMessages = 1 to 20

        sendingMessages.foreach { message =>
          session.sendCometMessage(cometName, Full(cometName), message)
        }

        session.findOrCreateComet[TestCometActor](Full(cometName), NodeSeq.Empty, Map.empty).map { comet =>
          comet !? NoOp /* Block to allow time for all messages to be collected */
        }

        receivedMessages mustEqual sendingMessages
      }
    }

    "Send messages to all comets of a particular type, regardless of name" in {
      val session = new LiftSession("Test Session", "", Empty)

      S.init(Empty, session) {
        val cometType = "TestCometActor"
        val cometName = "Comet1"

        // Spin up two comets: one with a name and one without
        session.sendCometMessage(cometType, Full(cometName), NoOp)
        session.sendCometMessage(cometType, Empty, NoOp)

        // Send a message to both
        session.sendCometMessage(cometType, 1)

        // Ensure both process the message
        session.findOrCreateComet[TestCometActor](Full(cometName), NodeSeq.Empty, Map.empty).map { comet =>
          comet !? NoOp
        }
        session.findOrCreateComet[TestCometActor](Empty, NodeSeq.Empty, Map.empty).map { comet =>
          comet !? NoOp
        }

        // Assert that the message was seen twice
        receivedMessages mustEqual Vector(1, 1)
      }
    }
  }
}
