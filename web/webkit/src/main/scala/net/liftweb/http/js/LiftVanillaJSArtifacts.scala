
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
import net.liftweb.util.Helpers
import JsCmds._

import scala.xml.NodeSeq
import util.Helpers._

object LiftVanillaJSArtifacts extends JSArtifacts {

  private val PreviousDisplayValueAttr = "data-liftjs-previous-display-value"

  private case class ById(id: String) extends  JsExp {
    override def toJsCmd: String = s"document.getElementById('${id.encJs}')"
  }

  /**
    * Toggles the visibility of the element denomiated by id
    */
  override def toggle(id: String): JsExp = {
    s"""
       |var el = ${ById(id)};
       |var currentDisplayValue = window.getComputedStyle(el).display;
       |if (currentDisplayValue === 'none') {
       |  var prevDisplayValue = el.getAttribute('$PreviousDisplayValueAttr');
       |  if (prevDisplayValue) {
       |    el.style.display = prevDisplayValue;
       |  } else {
       |    var tempEl = document.body.appendChild(document.createElement(el.tagName));
       |    var defaultDisplayValue = window.getComputedStyle(tempEl).display;
       |    if (!defaultDisplayValue || defaultDisplayValue === 'none') {
       |      defaultDisplayValue = 'block';
       |    }
       |    tempEl.parentNode.removeChild(tempEl);
       |    el.style.display = defaultDisplayValue;
       |  }
       |} else {
       |  el.setAttribute('$PreviousDisplayValueAttr', currentDisplayValue);
       |  el.style.display = 'none';
       |}
     """.stripMargin
  }

  /**
    * Hides the element denominated by id
    */
  override def hide(id: String): JsExp = {
    s"""
       |var el = ${ById(id)};
       |var currentDisplayValue = window.getComputedStyle(el).display;
       |if (currentDisplayValue !== 'none') {
       |  el.setAttribute('$PreviousDisplayValueAttr', currentDisplayValue);
       |  el.style.display = 'none';
       |}
     """.stripMargin
  }

  /**
    * Shows the element denominated by id
    */
  override def show(id: String): JsExp = ???

  /**
    * Shows the element denominated by id and puts the focus on it
    */
  override def showAndFocus(id: String): JsExp = ???

  /**
    * Serializes a form denominated by id. It returns a query string
    * containing the fields that are to be submitted
    */
  override def serialize(id: String): JsExp = ???

  /**
    * Replaces the content of the node denominated by id with the markup given by content
    */
  override def replace(id: String, content: NodeSeq): JsCmd =  new JsCmd {
    override def toJsCmd: String = {
      val (html, js) = fixHtmlAndJs("inline", content)
      s"""
         |var el = ${ById(id)};
         |if (el && el.parentNode) {
         |  var docFrag = document.createDocumentFragment();
         |  var tempDiv = document.createElement('div');
         |  tempDiv.innerHTML('$html');
         |  for (var i = 0; i < tempDiv.children.length; i++) {
         |    docFrag.appendChild(tempDiv.item(i));
         |  }
         |  el.parentNode.replaceChild(docFrag, el);
         |  ${js.toJsCmd}
         |}
     """.stripMargin
    }
  }

  /**
    * Sets the inner HTML of the element denominated by id
    */
  override def setHtml(id: String, content: NodeSeq): JsCmd = {
    ById(id) ~> new JsMember with HtmlFixer {
      override def toJsCmd: String = fixHtmlCmdFunc("inline", content) { html =>
        s"innerHTML('$html')"
      }
    }

  }

  /**
    * Queues the JavaScript in cmd for execution when the document is
    * ready for processing
    */
  override def onLoad(cmd: JsCmd): JsCmd = {
    s"""
       |window.addEventListener("load", function() {
       |  ${cmd.toJsCmd}
       |});
     """.stripMargin
  }

  /**
    * Fades out the element denominated by id, by waiting
    * for duration milliseconds and fading out for fadeTime milliseconds
    */
  override def fadeOut(id: String, duration: Helpers.TimeSpan, fadeTime: Helpers.TimeSpan): JsCmd =

  /**
    * Transforms a JSON object into its string representation
    */
  override def jsonStringify(in: JsExp): JsExp = new JsExp {
    def toJsCmd = s"JSON.stringify(${in.toJsCmd})"
  }

  /**
    * Converts a form denominated by formId into a JSON object
    */
  override def formToJSON(formId: String): JsExp = ???
}