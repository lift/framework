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

import net.liftweb.common.{Loggable, Full, Empty}
import org.specs2.mutable.Specification

import scala.xml.NodeSeq

object LiftSessionSpec extends Specification with Loggable {

  val testSession = new LiftSession("Test Session", "", Empty) {
    override private[liftweb] def set[T](name: String, value: T) = {
      logger.info(s"set called. $name $value")
      super.set(name, value)
    }
  }


  "A LiftSession" should {
    "Render messages in the order collected" in {
      val cometName = "TestComet"
      1 to 3 foreach(n => testSession.sendCometActorMessage("CometType", Full(cometName), n))
      logger.info(testSession.nmyVariables)
      val bc = testSession.findOrCreateComet(cometName)
      logger.info("Comet: " + bc)
      bc.foreach(_.!(1))
      bc.isDefined mustEqual true
    }
  }
}
