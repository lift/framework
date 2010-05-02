/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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

package net.liftweb {
package http {
package js {
package jquery {

import _root_.scala.xml.{Elem, NodeSeq}

import _root_.net.liftweb.http.S
import _root_.net.liftweb.http.js.JE
import _root_.net.liftweb.http.js.JsCmds
import JE._
import JqJE._
import JqJsCmds._
import util.Helpers._
import util.Props

trait JQueryArtifacts extends JSArtifacts {
  def toggle(id: String) = JqId(id) ~> new JsMember {
    def toJsCmd = "toggle()"
  }

  def hide(id: String) = JqId(id) ~> new JsMember {
    def toJsCmd = "hide()"
  }

  def show(id: String) = JqId(id) ~> new JsMember {
    def toJsCmd = "show()"
  }

  def showAndFocus(id: String) = JqId(id) ~> new JsMember {
    def toJsCmd = "show().each(function(i) {var t = this; setTimeout(function() { t.focus(); }, 200);})"
  }

  def serialize(id: String) = JqId(id) ~> new JsMember {
    def toJsCmd = "serialize()"
  }

  def setHtml(id: String, xml: NodeSeq): JsCmd = JqJsCmds.JqSetHtml(id, xml)

  def onLoad(cmd: JsCmd): JsCmd = JqJsCmds.JqOnLoad(cmd)

  def fadeOut(id: String, duration: TimeSpan, fadeTime: TimeSpan) = 
    FadeOut(id, duration, fadeTime)

  def ajax(data: AjaxInfo): String = {
    "jQuery.ajax(" + toJson(data, S.contextPath,
      prefix =>
              JsRaw("liftAjax.addPageName(" + S.encodeURL(prefix + "/" + LiftRules.ajaxPath + "/").encJs + ")")) + ");"
  }

  def comet(data: AjaxInfo): String = {
    "jQuery.ajax(" + toJson(data, LiftRules.cometServer(), LiftRules.calcCometPath) + ");"
  }

  def jsonStringify(in: JsExp): JsExp = new JsExp {
    def toJsCmd = "JSON.stringify(" + in.toJsCmd + ")"
  }

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

case object JQuery13Artifacts extends JQueryArtifacts {
  override def pathRewriter: PartialFunction[List[String], List[String]] = {
    case "jquery.js" :: Nil if Props.devMode => List("jquery-1.3.2.js")
    case "jquery.js" :: Nil => List("jquery-1.3.2-min.js")
  }
}


case object JQuery14Artifacts extends JQueryArtifacts {
  override def pathRewriter: PartialFunction[List[String], List[String]] = {
    case "jquery.js" :: Nil if Props.devMode => List("jquery-1.4.2.js")
    case "jquery.js" :: Nil => List("jquery-1.4.2-min.js")
  }
}


}
}
}
}
