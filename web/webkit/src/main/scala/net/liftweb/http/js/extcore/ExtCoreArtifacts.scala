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

import scala.xml.NodeSeq

import net.liftweb.http.S
import net.liftweb.http.js.JE
import net.liftweb.http.js.JsCmds
import JE._
import JsCmds._
import util.Helpers._

/**
 * JavaScript DSL to use the Ext Core JavaScript library
 */
object ExtCoreArtifacts extends JSArtifacts {
  /**
   * Toggles between current JS object and the object denominated by id
   */
  def toggle(id: String) = new JsExp {
  	def toJsCmd = "Ext.fly(" + id.encJs + ").toggle()"
  }

  /**
   * Hides the element denominated by id
   */
  def hide(id: String) = new JsExp {
  	def toJsCmd = "Ext.fly(" + id.encJs + ").hide()"
  }

  /**
   * Shows the element denominated by this id
   */
  def show(id: String) = new JsExp {
  	def toJsCmd = "Ext.fly(" + id.encJs + ").show()"
  }

  /**
   * Shows the element denoinated by id and puts the focus on it
   */
  def showAndFocus(id: String) = new JsExp {
  	def toJsCmd = "Ext.fly(" + id.encJs + ").show().focus(200)"
  }

  /**
   * Serializes a form denominated by the id. It returns a query string
   * containing the fields that are to be submitted
   */
  def serialize(id: String) = new JsExp {
  	def toJsCmd = "Ext.Ajax.serializeForm(" + id.encJs + ")"
  }

  /**
   * Replaces the content of the node with the provided id with the markup given by content
   */
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

  /**
   * Sets the inner HTML of the element denominated by the id
   */
  def setHtml(id: String, xml: NodeSeq): JsCmd = new JsCmd {
  	def toJsCmd = fixHtmlCmdFunc(id, xml){s => "try { Ext.fly(" + id.encJs + ").dom.innerHTML = " + s + "; } catch (e) {}"}
  }

  /**
   * Sets the JavScript that will be executed when document is ready
   * for processing
   */
  def onLoad(cmd: JsCmd): JsCmd = new JsCmd {
  	def toJsCmd = "Ext.onReady(function() {" + cmd.toJsCmd + "})"
	}

  /**
   * Fades out the element having the provided id, by waiting
   * for the given duration and fades out during fadeTime
   */
  def fadeOut(id: String, duration: TimeSpan, fadeTime: TimeSpan) = Noop

  /**
   * Trabsforms a JSON object intoits string representation
   */
  def jsonStringify(in: JsExp) : JsExp = new JsExp {
    def toJsCmd = "Ext.encode(" + in.toJsCmd + ")"
  }

  /**
   * Converts a form denominated by formId into a JSON object
   */
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

