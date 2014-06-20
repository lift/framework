/*
 * Copyright 2010-2014 WorldWide Conferencing, LLC
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

import org.specs2.mutable.Specification

import actor.LAScheduler
import common._
import js.JsCmds._

object CometActorSpec extends Specification {
  private case object TestMessage

  private val testSession = new LiftSession("Test Session", "", Empty)

  private class SpecCometActor extends CometActor {
    var receivedMessages = List[Any]()

    def render = NodeSeq.Empty
    override def theSession = testSession

    override def !(msg: Any) = {
      receivedMessages ::= msg

      LAScheduler.onSameThread = true

      super.!(msg)

      LAScheduler.onSameThread = false
    }
  }

  "A CometActor" should {
    class RedirectingComet extends SpecCometActor {
      override def lowPriority = {
        case TestMessage =>
          S.redirectTo("place")
      }
    }

    "redirect the user when a ResponseShortcutException with redirect occurs" in {
      val comet = new RedirectingComet

      comet ! TestMessage

      comet.receivedMessages.exists {
        case PartialUpdateMsg(update) if update() == RedirectTo("place") =>
          true
        case _ =>
          false
      } must beTrue
    }

    class FunctionRedirectingComet extends SpecCometActor {
      override def lowPriority = {
        case TestMessage =>
          S.redirectTo("place", () => "do stuff")
      }
    }

    "redirect the user with a function when a ResponseShortcutException with redirect+function occurs" in {
      val comet = new FunctionRedirectingComet

      comet ! TestMessage

      val matchingMessage =
        comet.receivedMessages.collect {
          case PartialUpdateMsg(update) =>
            update()
        }

      matchingMessage must beLike {
        case List(RedirectTo(redirectUri)) =>
          redirectUri must startWith("place")
          redirectUri must beMatching("^[^?]+\\?F[^=]+=_$".r)
      }
    }
  }
}
