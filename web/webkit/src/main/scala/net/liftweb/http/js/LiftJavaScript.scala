/*
 * Copyright 2013 WorldWide Conferencing, LLC
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
package js

// import scala.collection.mutable.ListBuffer

import common._
import util.Props
import http.js._
import http.js.jquery.JQueryArtifacts

import JsCmds._
import JE._

/**
  * Create a javascript command that will initialize lift.js using LiftRules.
  *
  * This is
  */
object LiftJavaScript {

  def settings: JsObj = JsObj(
    "ajaxPath" -> LiftRules.ajaxPath,
    "ajaxRetryCount" -> Num(LiftRules.ajaxRetryCount.openOr(3)),
    "ajaxPostTimeout" -> LiftRules.ajaxPostTimeout,
    "enableGc" -> LiftRules.enableLiftGC,
    "gcPollingInterval" -> LiftRules.liftGCPollingInterval,
    "gcFailureRetryTimeout" -> LiftRules.liftGCFailureRetryTimeout,
    "cometPath" -> LiftRules.cometPath,
    "cometGetTimeout" -> LiftRules.cometGetTimeout,
    "cometFailureRetryTimeout" -> LiftRules.cometFailureRetryTimeout,
    "cometServer" -> LiftRules.cometServer(),
    "logError" -> LiftRules.jsLogFunc.map(fnc => AnonFunc("msg", fnc(JsVar("msg")))).openOr(AnonFunc("msg", Noop)),
    "ajaxOnFailure" -> LiftRules.ajaxDefaultFailure.map(fnc => AnonFunc(fnc())).openOr(AnonFunc(Noop)),
    "ajaxOnStart" -> LiftRules.ajaxStart.map(fnc => AnonFunc(fnc())).openOr(AnonFunc(Noop)),
    "ajaxOnEnd" -> LiftRules.ajaxEnd.map(fnc => AnonFunc(fnc())).openOr(AnonFunc(Noop))
  )

  def initCmd(settings: JsObj): JsCmd = {
    val extendCmd = LiftRules.jsArtifacts match {
      case jsa: JQueryArtifacts => Call("window.lift.extend", JsRaw("lift_settings"), JsRaw("window.liftJQuery"))
      case _ => Call("window.lift.extend", JsRaw("lift_settings"), JsRaw("window.liftVanilla"))
    }

    JsCrVar("lift_settings", settings) &
    extendCmd &
    Call("window.lift.init", JsRaw("lift_settings"))
  }
}

// Overridable is js code
// ajaxOnSessionLost
// cometOnSessionLost
// cometOnError
