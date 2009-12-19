/*
 * Copyright 2007-2009 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */

package net.liftweb.http.jquery

import _root_.net.liftweb.http.S._
import _root_.net.liftweb.http.SHtml._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.http.js.jquery._
import JE._
import JqJsCmds._
import _root_.scala.xml._

/**
 * This contains Html artifacts that are heavily relying on JQuery
 */
object JqSHtml {
  def fadeOutErrors(duration: TimeSpan, fadeTime: TimeSpan): JsCmd = {
    FadeOut(LiftRules.noticesContainerId + "_error", duration, fadeTime)
  }

  def fadeOutWarnings(duration: TimeSpan, fadeTime: TimeSpan): JsCmd = {
    FadeOut(LiftRules.noticesContainerId + "_warn", duration, fadeTime)
  }

  def fadeOutNotices(duration: TimeSpan, fadeTime: TimeSpan): JsCmd = {
    FadeOut(LiftRules.noticesContainerId + "_notice", duration, fadeTime)
  }


}
