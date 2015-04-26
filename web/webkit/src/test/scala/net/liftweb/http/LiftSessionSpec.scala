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
import net.liftweb.common.{Loggable, Full, Empty}
import org.specs2.mutable.Specification

object LiftSessionSpec extends Specification with Loggable {

  class TestComet extends CometActor with Loggable {
    logger.info("TestComet created")
    def render = NodeSeq.Empty
    override def !(msg: Any) = {
      logger.info("Message received: " + msg)
    }
  }

  "A LiftSession" should {

    "Send accumulated messages to a newly-created comet actor in the order in which they arrived" in {

      val testSession = new LiftSession("Test Session", "", Empty) {
        override private[liftweb] def set[T](name: String, value: T) = {
          logger.info(s"set called. $name $value")
          super.set(name, value)
        }
      }

      val cometName = "TestComet"
      1 to 3 foreach(n => testSession.sendCometActorMessage("CometType", Full(cometName), n))
      val bc = testSession.findOrCreateComet[TestComet](Empty, NodeSeq.Empty, Map.empty)
      logger.info("Comet: " + bc)
      bc.foreach(_.!(1))
      bc.isDefined mustEqual true
    }
  }
}
