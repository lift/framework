/*
 * Copyright 2010-2012 WorldWide Conferencing, LLC
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

import scala.xml._

import org.specs2.mutable.Specification

import common._
import js.JsCmds

/**
 * System under specification for NamedComet* files.
 */
object NamedCometPerTabSpec extends Specification  {
  "NamedCometPerTabSpec Specification".title

  class CometA extends NamedCometActorTrait{
    override def lowPriority = {
      case msg => JsCmds.Noop
    }
    def render = {
      "nada" #> Text("nada")
    }
  }

  "A NamedCometDispatcher" should {
    step {
      val cometA= new CometA{override def name= Full("1")}
      cometA.localSetup

      // HACK! to ensure tests doesn't fail when trying to access actor before they've been registered
      Thread.sleep(500)
    }

    "be created for a comet" in {
      NamedCometListener.getDispatchersFor(Full("1")).foreach(
        actor => actor.map(_.toString must startWith("net.liftweb.http.NamedCometDispatcher"))
      )
      success
    }
    "be created even if no comet is present when calling getOrAddDispatchersFor" in {
      NamedCometListener.getOrAddDispatchersFor(Full("3")).foreach(
        actor => actor.toString must startWith("net.liftweb.http.NamedCometDispatcher")
      )
      success
    }
    "not be created for a non existing key" in {
      NamedCometListener.getDispatchersFor(Full("2")).foreach(
        actor => actor must_== Empty
      )
      success
    }
  }


}
