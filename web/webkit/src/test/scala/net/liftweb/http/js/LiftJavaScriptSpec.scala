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

import org.specs2.mutable.Specification

import common._
import http.js._
import http.js.JsCmds._
import http.js.JE._
import util.Props
import util.Helpers._

/**
 * System under specification for LiftJavaScript.
 */
object LiftJavaScriptSpec extends Specification  {
  sequential
  "LiftJavaScript Specification".title

  private def session = new LiftSession("", randomString(20), Empty)

  "LiftJavaScript" should {
    "create default settings" in {
      S.initIfUninitted(session) {
        val settings = LiftJavaScript.settings
        settings.toJsCmd must_== """{"liftPath": "/lift", "ajaxRetryCount": 3, "ajaxPostTimeout": 5000, "gcPollingInterval": 75000, "gcFailureRetryTimeout": 15000, "cometGetTimeout": 140000, "cometFailureRetryTimeout": 10000, "cometServer": null, "logError": function(msg) {}, "ajaxOnFailure": function() {alert("The server cannot be contacted at this time");}, "ajaxOnStart": function() {}, "ajaxOnEnd": function() {}}"""
      }
    }
    "create custom static settings" in {
      S.initIfUninitted(session) {
        LiftRules.ajaxRetryCount = Full(4)
        val settings = LiftJavaScript.settings
        settings.toJsCmd must_== """{"liftPath": "/lift", "ajaxRetryCount": 4, "ajaxPostTimeout": 5000, "gcPollingInterval": 75000, "gcFailureRetryTimeout": 15000, "cometGetTimeout": 140000, "cometFailureRetryTimeout": 10000, "cometServer": null, "logError": function(msg) {}, "ajaxOnFailure": function() {alert("The server cannot be contacted at this time");}, "ajaxOnStart": function() {}, "ajaxOnEnd": function() {}}"""
      }
    }
    "create custom dynamic settings" in {
      S.initIfUninitted(session) {
        LiftRules.cometServer = () => Some("srvr1")
        val settings = LiftJavaScript.settings
        settings.toJsCmd must_== """{"liftPath": "/lift", "ajaxRetryCount": 4, "ajaxPostTimeout": 5000, "gcPollingInterval": 75000, "gcFailureRetryTimeout": 15000, "cometGetTimeout": 140000, "cometFailureRetryTimeout": 10000, "cometServer": "srvr1", "logError": function(msg) {}, "ajaxOnFailure": function() {alert("The server cannot be contacted at this time");}, "ajaxOnStart": function() {}, "ajaxOnEnd": function() {}}"""
      }
    }
    "create custom function settings" in {
      S.initIfUninitted(session) {
        LiftRules.jsLogFunc = Full(v => JE.Call("lift.logError", v))
        val settings = LiftJavaScript.settings
        settings.toJsCmd must_== """{"liftPath": "/lift", "ajaxRetryCount": 4, "ajaxPostTimeout": 5000, "gcPollingInterval": 75000, "gcFailureRetryTimeout": 15000, "cometGetTimeout": 140000, "cometFailureRetryTimeout": 10000, "cometServer": "srvr1", "logError": function(msg) {lift.logError(msg);}, "ajaxOnFailure": function() {alert("The server cannot be contacted at this time");}, "ajaxOnStart": function() {}, "ajaxOnEnd": function() {}}"""
      }
    }
    "create init command" in {
      S.initIfUninitted(session) {
        val init = LiftRules.javaScriptSettings.vend().map(_.apply(session)).map(LiftJavaScript.initCmd(_).toJsCmd)
        init must_==
          Full("""var lift_settings = {"liftPath": "/lift", "ajaxRetryCount": 4, "ajaxPostTimeout": 5000, "gcPollingInterval": 75000, "gcFailureRetryTimeout": 15000, "cometGetTimeout": 140000, "cometFailureRetryTimeout": 10000, "cometServer": "srvr1", "logError": function(msg) {lift.logError(msg);}, "ajaxOnFailure": function() {alert("The server cannot be contacted at this time");}, "ajaxOnStart": function() {}, "ajaxOnEnd": function() {}};
            |window.lift.extend(lift_settings,window.liftJQuery);
            |window.lift.init(lift_settings);""".stripMargin)
      }
    }
    "create init command with custom setting" in {
      S.initIfUninitted(session) {
        val settings = LiftJavaScript.settings.extend(JsObj("liftPath" -> "liftyStuff", "mysetting" -> 99))
        val init = LiftJavaScript.initCmd(settings)
        println(init.toJsCmd)
        init.toJsCmd must_==
          """var lift_settings = {"liftPath": "liftyStuff", "ajaxRetryCount": 4, "ajaxPostTimeout": 5000, "gcPollingInterval": 75000, "gcFailureRetryTimeout": 15000, "cometGetTimeout": 140000, "cometFailureRetryTimeout": 10000, "cometServer": "srvr1", "logError": function(msg) {lift.logError(msg);}, "ajaxOnFailure": function() {alert("The server cannot be contacted at this time");}, "ajaxOnStart": function() {}, "ajaxOnEnd": function() {}, "mysetting": 99};
            |window.lift.extend(lift_settings,window.liftJQuery);
            |window.lift.init(lift_settings);""".stripMargin
      }
    }
  }
}
