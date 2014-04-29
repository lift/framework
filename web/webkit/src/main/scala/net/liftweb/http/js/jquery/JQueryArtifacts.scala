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
package http
package js
package jquery

import scala.xml.{Elem, NodeSeq}

import net.liftweb.http.S
import net.liftweb.http.js.JE
import net.liftweb.http.js.JsCmds
import JE._
import JqJE._
import JqJsCmds._
import util.Helpers._
import util.Props

trait JQueryArtifacts extends JSArtifacts {
  /**
   * Toggles between current JS object and the object denominated by id
   */
  def toggle(id: String) = JqId(id) ~> new JsMember {
    def toJsCmd = "toggle()"
  }

  /**
   * Hides the element denominated by id
   */
  def hide(id: String) = JqId(id) ~> new JsMember {
    def toJsCmd = "hide()"
  }

  /**
   * Shows the element denominated by this id
   */
  def show(id: String) = JqId(id) ~> new JsMember {
    def toJsCmd = "show()"
  }

  /**
   * Shows the element denominated by id and puts the focus on it
   */
  def showAndFocus(id: String) = JqId(id) ~> new JsMember {
    def toJsCmd = "show().each(function(i) {var t = this; setTimeout(function() { t.focus(); }, 200);})"
  }

  /**
   * Serializes a form denominated by the id. It returns a query string
   * containing the fields that are to be submitted
   */
  def serialize(id: String) = JqId(id) ~> new JsMember {
    def toJsCmd = "serialize()"
  }

  /**
   * Replaces the content of the node with the provided id with the markup given by content
   */
  def replace(id: String, content: NodeSeq): JsCmd = JqJsCmds.JqReplace(id, content)

  /**
   * Sets the inner HTML of the element denominated by the id
   */
  def setHtml(id: String, content: NodeSeq): JsCmd = JqJsCmds.JqSetHtml(id, content)

  /**
   * Sets the JavScript that will be executed when document is ready
   * for processing
   */
  def onLoad(cmd: JsCmd): JsCmd = JqJsCmds.JqOnLoad(cmd)

  /**
   * Fades out the element having the provided id, by waiting
   * for the given duration and fades out during fadeTime
   */
  def fadeOut(id: String, duration: TimeSpan, fadeTime: TimeSpan) =
    FadeOut(id, duration, fadeTime)

  /**
   * Transforms a JSON object in to its string representation
   */
  def jsonStringify(in: JsExp): JsExp = new JsExp {
    def toJsCmd = "JSON.stringify(" + in.toJsCmd + ")"
  }

  /**
   * Converts a form denominated by formId into a JSON object
   */
  def formToJSON(formId: String): JsExp = new JsExp() {
    def toJsCmd = "lift$.formToJSON('" + formId + "')";
  }

  private def toJson(info: AjaxInfo, server: String, path: String => JsExp): String =
    (("url : " + path(server).toJsCmd) ::
            "data : " + info.data.toJsCmd ::
            ("type : " + info.action.encJs) ::
            ("dataType : " + info.dataType.encJs) ::
            "timeout : " + info.timeout ::
            "cache : " + info.cache :: Nil) ++
            info.successFunc.map("success : " + _).toList ++
            info.failFunc.map("error : " + _).toList mkString ("{ ", ", ", " }")
}

case object JQueryArtifacts extends JQueryArtifacts
