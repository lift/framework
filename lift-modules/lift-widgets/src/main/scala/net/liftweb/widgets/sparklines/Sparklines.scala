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
package widgets {
package sparklines {

import _root_.scala.xml._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.common.{Box, Full, Empty}
import _root_.net.liftweb.http.S._
import _root_.net.liftweb.http.LiftRules
import _root_.net.liftweb.http.{LiftResponse, JsonResponse}
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.http.js.jquery._
import JsCmds._
import JE._
import JqJsCmds._
import JqJE._

/**
 * Facilitates Sparklines from http://www.willarson.com/code/sparklines/sparklines.html
 * integration into Lift applications.
 *
 */
object Sparklines {

  /**
   * Renders a sparkline graph when the page loads
   *
   * @param id - the id of the canvas HTML element
   * @param graphStyle - the style of the chart (LINE, BAR)
   * @param data - the amplitudes to be rendered
   * @param opts - Sparklines options. See http://www.willarson.com/code/sparklines/sparklines.html
   */
  def onLoad(id: String, graphStyle: SparklineStyle.Value, data: JsArray, opts: JsObj) : NodeSeq = {
    <head>
       <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/sparklines/sparklines.min.js"}/>
    {
      Script(OnLoad(toJsExp(id, graphStyle, data, opts)))
    }
    </head>
  }

  /**
   * Returns a JsExp that draws a sparklines graph. Can be combimed with other JsExp's to render the
   * graph whenever needed.
   *
   * @param id - the id of the canvas HTML element
   * @param graphStyle - the style of the chart (LINE, BAR)
   * @param data - the amplitudes to be rendered
   * @param opts - Sparklines options. See http://www.willarson.com/code/sparklines/sparklines.html
   */
  def toJsExp(id: String, graphStyle: SparklineStyle.Value, data: JsArray, opts: JsObj): JsExp = new JsExp {
    def toJsCmd = "new " + graphStyle + "(" + id.encJs + ", " + data.toJsCmd + ", " + opts.toJsCmd + ").draw()"
  }

  /**
   * Renders a canvas element but makes sure that sparklines JS script dependency will be rendered.
   *
   * @param id - the id of the canvas HTML element
   * @param cssClass - the stylesheet class
   *
   */
  def renderCanvas(id: String, cssClass: String) : NodeSeq = {
    <head>
       <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/sparklines/sparklines.min.js"}/>
    </head> ++
    <canvas id={id} class={cssClass}></canvas>
  }

  /**
   * Call this function typically in boot
   */
  def init() {
    import _root_.net.liftweb.http.ResourceServer

    ResourceServer.allow({
      case "sparklines" :: _ => true
    })

  }

}

}
}
}
