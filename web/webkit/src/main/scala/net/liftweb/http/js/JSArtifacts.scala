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

import _root_.net.liftweb.common.{Box, Full, Empty}
import _root_.net.liftweb.http.NoticeType
import _root_.scala.xml.{Elem, NodeSeq}
import _root_.net.liftweb.util.Helpers._

/**
 * Abstracted JavaScript artifacts used by lift core.
 */
trait JSArtifacts {

  /**
   * Toggles between current JS object and the object denominated by id
   */
  def toggle(id: String): JsExp

  /**
   * Hides the element denominated by id
   */
  def hide(id: String): JsExp

  /**
   * SHows the element denominated by this id
   */
  def show(id: String): JsExp

  /**
   * Shows the element denoinated by id and puts the focus on it
   */
  def showAndFocus(id: String): JsExp

  /**
   * Serializes a form denominated by the id. It returns a query string
   * containing the fields that are to be submitted
   */
  def serialize(id: String): JsExp

  /**
   * Sets the inner HTML of the element denominated by the id
   */
  def setHtml(id: String, xml: NodeSeq): JsCmd

  /**
   * Sets the JavScript that willbe executed when document is ready
   * for processing
   */
  def onLoad(cmd: JsCmd): JsCmd

  /**
   * Fades out the element having the provided id, by waiting
   * for the given duration and fades out during fadeTime
   */
  def fadeOut(id: String, duration: TimeSpan, fadeTime: TimeSpan): JsCmd

  /**
   * Makes an Ajax request using lift's Ajax path and the request
   * attributes described by data parameter
   */
  def ajax(data: AjaxInfo): String

  /**
   * Makes a Ajax comet request using lift's Comet path and the request
   * attributes described by data parameter
   */
  def comet(data: AjaxInfo): String

  /**
   * Trabsforms a JSON object intoits string representation
   */
  def jsonStringify(in: JsExp): JsExp

  /**
   * Converts a form denominated by formId into a JSON object
   */
  def formToJSON(formId: String): JsExp

  /**
   * Rewrites the incomming path with the actual script path
   *
   */
  def pathRewriter: PartialFunction[List[String], List[String]] = 
    new PartialFunction[List[String], List[String]] {
    
      def isDefinedAt(in: List[String]): Boolean = false

      def apply(in: List[String]): List[String] = Nil 
 
    }
}

/**
 * The companion module for AjaxInfo that provides
 * different construction schemes
 */
object AjaxInfo {
  def apply(data: JsExp, post: Boolean) =
    new AjaxInfo(data, if (post) "POST" else "GET", 1000, false, "script", Empty, Empty)

  def apply(data: JsExp,
            dataType: String,
            post: Boolean) =
    new AjaxInfo(data, if (post) "POST" else "GET", 1000, false, dataType, Empty, Empty)

  def apply(data: JsExp) =
    new AjaxInfo(data, "POST", 1000, false, "script", Empty, Empty)

  def apply(data: JsExp,
            dataType: String) =
    new AjaxInfo(data, "POST", 1000, false, dataType, Empty, Empty)

  def apply(data: JsExp,
            post: Boolean,
            timeout: Long,
            successFunc: String,
            failFunc: String) =
    new AjaxInfo(data,
      if (post) "POST" else "GET",
      timeout,
      false,
      "script",
      Full(successFunc),
      Full(failFunc))
}

/**
 * Represents the meta data of an AJax request.
 */
case class AjaxInfo(data: JsExp, action: String, timeout: Long,
                    cache: Boolean, dataType: String,
                    successFunc: Box[String], failFunc: Box[String])

}
}
}
