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

package net.liftweb.widgets.tablesorter

import _root_.net.liftweb.http.ResourceServer
import _root_.scala.xml.NodeSeq
import _root_.net.liftweb.http.{LiftRules}
import _root_.net.liftweb.http.js._
import JsCmds._
import JE._
import jquery.JqJE._
import jquery.JQueryRight

object TableSorter {

  def apply(selector: String) = renderOnLoad(selector)

  /**
   * Initializes the widget
   */
  def init() {
    ResourceServer.allow({
      case "tablesorter" :: tail => true
    })
  }

  /**
   * Transforms a regular table into a tablesorter when page is loaded
   */
  def renderOnLoad(selector: String) = {
    val onLoad ="""jQuery(function($){
            $('"""+selector+"""').tablesorter({sortList:[[0,0]], widgets:['zebra']});
            });
            """
    <head>
      <link rel="stylesheet" href={"/" + LiftRules.resourceServerPath + "/tablesorter/themes/blue/style.css"} type="text/css" id="" media="print, projection, screen" />
      <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/tablesorter/jquery.tablesorter.js"}></script>
      <script type="text/javascript" charset="utf-8">{onLoad}</script>
    </head>
  }

  /**
   * Transforms a regular table into a tablesorter
   */
  def jsRender(selector: String) : JsExp = JqId(selector) >> new JsRaw("tablesorter({sortList:[[0,0]], widgets:['zebra']})") with JQueryRight

}
