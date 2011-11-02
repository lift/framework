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
package extcore

import scala.xml.{Elem, NodeSeq}

import net.liftweb.http.S
import net.liftweb.http.js.JE
import net.liftweb.http.js.JsCmds
import JE._
import JsCmds._
import util.Helpers._

object ExtCoreArtifacts extends JSArtifacts {
  def toggle(id: String) = new JsExp {
  	def toJsCmd = "Ext.fly(" + id.encJs + ").toggle()"
  }

  def hide(id: String) = new JsExp {
  	def toJsCmd = "Ext.fly(" + id.encJs + ").hide()"
  }

  def show(id: String) = new JsExp {
  	def toJsCmd = "Ext.fly(" + id.encJs + ").show()"
  }

  def showAndFocus(id: String) = new JsExp {
  	def toJsCmd = "Ext.fly(" + id.encJs + ").show().focus(200)"
  }

  def serialize(id: String) = new JsExp {
  	def toJsCmd = "Ext.Ajax.serializeForm(" + id.encJs + ")"
  }

  def replace(id: String, content: NodeSeq): JsCmd = new JsCmd with HtmlFixer {
    override val toJsCmd = {
      val (html, js) = fixHtmlAndJs("inline", content)

      val ret =
        """
	  try {
	  var parent1 = document.getElementById(""" + id.encJs + """);
	  parent1.innerHTML = """ + html + """;
	  for (var i = 0; i < parent1.childNodes.length; i++) {
	    var node = parent1.childNodes[i];
	    parent1.parentNode.insertBefore(node.cloneNode(true), parent1);
	  }
	  parent1.parentNode.removeChild(parent1);
	  } catch (e) {
	    // if the node doesn't exist or something else bad happens
	  }
	"""
      if (js.isEmpty) ret else ret + " "+js.toJsCmd

    }
  }

  def setHtml(id: String, xml: NodeSeq): JsCmd = new JsCmd {
  	def toJsCmd = fixHtmlCmdFunc(id, xml){s => "try { Ext.fly(" + id.encJs + ").dom.innerHTML = " + s + "; } catch (e) {}"}
  }

  def onLoad(cmd: JsCmd): JsCmd = new JsCmd {
  	def toJsCmd = "Ext.onReady(function() {" + cmd.toJsCmd + "})"
	}

  def fadeOut(id: String, duration: TimeSpan, fadeTime: TimeSpan) = Noop

  def ajax(data: AjaxInfo): String = {
    "Ext.Ajax.request(" + toJson(data, S.contextPath,
                            prefix =>
                            JsRaw(S.encodeURL(prefix + "/" +LiftRules.ajaxPath + "/").encJs))+");"
  }

  def comet(data: AjaxInfo): String = {
    "Ext.Ajax.request(" + toJson(data, LiftRules.cometServer(), LiftRules.calcCometPath) + ");"
  }

  def jsonStringify(in: JsExp) : JsExp = new JsExp {
    def toJsCmd = "Ext.encode(" + in.toJsCmd + ")"
  }

  def formToJSON(formId: String):JsExp = new JsExp() {
    def toJsCmd = "Ext.urlDecode(Ext.Ajax.serializeForm(" + formId.encJs + "));"
  }

  private def toJson(info: AjaxInfo, server: String, path: String => JsExp): String =
  (("url : liftAjax.addPageName(" + path(server).toJsCmd + ")" ) ::
   "params : " + info.data.toJsCmd ::
   ("method : " + info.action.encJs) ::
   ("dataType : " + info.dataType.encJs) ::
   "timeout : " + info.timeout ::
   "disableCaching : " + !info.cache ::
   "success: function(response, options) { res = Ext.lift.eval(response.responseText);" + info.successFunc.map(_ + "(res);").openOr("") + "}" ::
   "failure: " + info.failFunc.openOr ("function(arg) {alert('Ajax request failed');}") :: 
   Nil) mkString("{ ", ", ", " }")
}

