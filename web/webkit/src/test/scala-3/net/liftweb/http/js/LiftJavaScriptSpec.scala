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

import java.util.Locale

import net.liftweb.http.js.extcore.ExtCoreArtifacts
import net.liftweb.http.js.jquery.JQueryArtifacts
import org.specs2.execute.{Result, AsResult}
import org.specs2.execute.Scope
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
class LiftJavaScriptSpec extends Specification  {
  sequential
  "LiftJavaScript Specification".title

  private def session = new LiftSession("", randomString(20), Empty)

  "LiftJavaScript" should {
    "create default settings" in new WithLocale(Locale.ENGLISH) {
      S.initIfUninitted(session) {
        val settings = LiftJavaScript.settings
        settings.toJsCmd === formatjs(
          """{"liftPath": "/lift",
            |"ajaxRetryCount": 3,
            |"ajaxPostTimeout": 5000,
            |"gcPollingInterval": 75000,
            |"gcFailureRetryTimeout": 15000,
            |"cometGetTimeout": 140000,
            |"cometFailureRetryTimeout": 10000,
            |"cometServer": null,
            |"logError": function(msg) {},
            |"ajaxOnFailure": function() {alert("The server cannot be contacted at this time");},
            |"ajaxOnStart": function() {},
            |"ajaxOnEnd": function() {}}"""
        )
      }
    }
    "create internationalized default settings" in new WithLocale(Locale.forLanguageTag("pl-PL")) {
      S.initIfUninitted(session) {
        val settings = LiftJavaScript.settings
        val internationalizedMessage = "Nie mo\\u017cna skontaktowa\\u0107 si\\u0119 z serwerem"
        settings.toJsCmd === formatjs(
          s"""{"liftPath": "/lift",
              |"ajaxRetryCount": 3,
              |"ajaxPostTimeout": 5000,
              |"gcPollingInterval": 75000,
              |"gcFailureRetryTimeout": 15000,
              |"cometGetTimeout": 140000,
              |"cometFailureRetryTimeout": 10000,
              |"cometServer": null,
              |"logError": function(msg) {},
              |"ajaxOnFailure": function() {alert("$internationalizedMessage");},
              |"ajaxOnStart": function() {},
              |"ajaxOnEnd": function() {}}"""
        )
      }
    }
    "create custom static settings" in new WithLocale(Locale.ENGLISH) {
      S.initIfUninitted(session) {
        LiftRules.ajaxRetryCount = Full(4)
        val settings = LiftJavaScript.settings
        settings.toJsCmd === formatjs(
          """{"liftPath": "/lift",
            |"ajaxRetryCount": 4,
            |"ajaxPostTimeout": 5000,
            |"gcPollingInterval": 75000,
            |"gcFailureRetryTimeout": 15000,
            |"cometGetTimeout": 140000,
            |"cometFailureRetryTimeout": 10000,
            |"cometServer": null,
            |"logError": function(msg) {},
            |"ajaxOnFailure": function() {alert("The server cannot be contacted at this time");},
            |"ajaxOnStart": function() {},
            |"ajaxOnEnd": function() {}}"""
        )
      }
    }
    "create custom dynamic settings" in new WithLocale(Locale.ENGLISH) {
      S.initIfUninitted(session) {
        LiftRules.cometServer = () => Some("srvr1")
        val settings = LiftJavaScript.settings
        settings.toJsCmd === formatjs(
          """{"liftPath": "/lift",
            |"ajaxRetryCount": 4,
            |"ajaxPostTimeout": 5000,
            |"gcPollingInterval": 75000,
            |"gcFailureRetryTimeout": 15000,
            |"cometGetTimeout": 140000,
            |"cometFailureRetryTimeout": 10000,
            |"cometServer": "srvr1",
            |"logError": function(msg) {},
            |"ajaxOnFailure": function() {alert("The server cannot be contacted at this time");},
            |"ajaxOnStart": function() {},
            |"ajaxOnEnd": function() {}}"""
        )
      }
    }
    "create custom function settings" in new WithLocale(Locale.ENGLISH) {
      S.initIfUninitted(session) {
        LiftRules.jsLogFunc = Full(v => JE.Call("lift.logError", v))
        val settings = LiftJavaScript.settings
        settings.toJsCmd === formatjs(
          """{"liftPath": "/lift",
            |"ajaxRetryCount": 4,
            |"ajaxPostTimeout": 5000,
            |"gcPollingInterval": 75000,
            |"gcFailureRetryTimeout": 15000,
            |"cometGetTimeout": 140000,
            |"cometFailureRetryTimeout": 10000,
            |"cometServer": "srvr1",
            |"logError": function(msg) {lift.logError(msg);},
            |"ajaxOnFailure": function() {alert("The server cannot be contacted at this time");},
            |"ajaxOnStart": function() {},
            |"ajaxOnEnd": function() {}}"""
        )
      }
    }
    "create init command" in new WithLocale(Locale.ENGLISH) {
      S.initIfUninitted(session) {
        val init = LiftRules.javaScriptSettings.vend().map(_.apply(session)).map(LiftJavaScript.initCmd(_).toJsCmd)
        init === Full(formatjs(List(
          "var lift_settings = {};",
          "window.lift.extend(lift_settings,window.liftJQuery);",
          """window.lift.extend(lift_settings,{"liftPath": "/lift",
            |"ajaxRetryCount": 4,
            |"ajaxPostTimeout": 5000,
            |"gcPollingInterval": 75000,
            |"gcFailureRetryTimeout": 15000,
            |"cometGetTimeout": 140000,
            |"cometFailureRetryTimeout": 10000,
            |"cometServer": "srvr1",
            |"logError": function(msg) {lift.logError(msg);},
            |"ajaxOnFailure": function() {alert("The server cannot be contacted at this time");},
            |"ajaxOnStart": function() {},
            |"ajaxOnEnd": function() {}});""",
          "window.lift.init(lift_settings);"
        )))
      }
    }
    "create init command with VanillaJS" in new WithLocale(Locale.ENGLISH) {
      S.initIfUninitted(session) {
        LiftRules.jsArtifacts = ExtCoreArtifacts
        val init = LiftRules.javaScriptSettings.vend().map(_.apply(session)).map(LiftJavaScript.initCmd(_).toJsCmd)
        init === Full(formatjs(List(
          "var lift_settings = {};",
          "window.lift.extend(lift_settings,window.liftVanilla);",
          """window.lift.extend(lift_settings,{"liftPath": "/lift",
            |"ajaxRetryCount": 4,
            |"ajaxPostTimeout": 5000,
            |"gcPollingInterval": 75000,
            |"gcFailureRetryTimeout": 15000,
            |"cometGetTimeout": 140000,
            |"cometFailureRetryTimeout": 10000,
            |"cometServer": "srvr1",
            |"logError": function(msg) {lift.logError(msg);},
            |"ajaxOnFailure": function() {alert("The server cannot be contacted at this time");},
            |"ajaxOnStart": function() {},
            |"ajaxOnEnd": function() {}});""",
          "window.lift.init(lift_settings);"
        )))
      }
    }
    "create init command with custom setting" in new WithLocale(Locale.ENGLISH) {
      S.initIfUninitted(session) {
        LiftRules.jsArtifacts = JQueryArtifacts
        val settings = LiftJavaScript.settings.extend(JsObj("liftPath" -> "liftyStuff", "mysetting" -> 99))
        val init = LiftJavaScript.initCmd(settings)
        init.toJsCmd === formatjs(List(
          "var lift_settings = {};",
          "window.lift.extend(lift_settings,window.liftJQuery);",
          """window.lift.extend(lift_settings,{"liftPath": "liftyStuff",
            |"ajaxRetryCount": 4,
            |"ajaxPostTimeout": 5000,
            |"gcPollingInterval": 75000,
            |"gcFailureRetryTimeout": 15000,
            |"cometGetTimeout": 140000,
            |"cometFailureRetryTimeout": 10000,
            |"cometServer": "srvr1",
            |"logError": function(msg) {lift.logError(msg);},
            |"ajaxOnFailure": function() {alert("The server cannot be contacted at this time");},
            |"ajaxOnStart": function() {},
            |"ajaxOnEnd": function() {},
            |"mysetting": 99});""",
          "window.lift.init(lift_settings);"
        ))
      }
    }
  }

  def formatjs(line:String):String = formatjs(line :: Nil)
  def formatjs(lines:List[String]):String = lines.map { _.stripMargin.linesIterator.toList match {
    case init :+ last => (init.map(_ + " ") :+ last).mkString
    case Nil => ""
  }}.mkString("\n")

  object withEnglishLocale extends WithLocale(Locale.ENGLISH)

  object withPolishLocale extends WithLocale(Locale.forLanguageTag("pl-PL"))

  class WithLocale(locale: Locale) extends Scope {
    val savedDefaultLocale = Locale.getDefault
    Locale.setDefault(locale)

    // Cleanup happens automatically when scope exits via try/finally in specs2
    override def toString = {
      try {
        super.toString
      } finally {
        Locale.setDefault(savedDefaultLocale)
      }
    }
  }
}
