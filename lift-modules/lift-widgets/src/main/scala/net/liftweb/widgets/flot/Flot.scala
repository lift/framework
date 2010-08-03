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
package flot {

import scala.xml.{NodeSeq, Node, PCData, Text, Unparsed}
import _root_.net.liftweb.http.{LiftRules}
import _root_.net.liftweb.http.js._
import JsCmds._
import JE._
// jQuery specific
import _root_.net.liftweb.http.js.jquery._
import JsCmds._
import JE._
import JqJE._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import Helpers._

/**
 * renders a flot graph using http://code.google.com/p/flot/ jQuery widget
 * <br />
 * See the sites/flotDemo webapp for examples.
 */

object Flot
{
  /**
   * register the resources with lift (typically in boot)
   */

  def init() {
    import net.liftweb.http.ResourceServer

    ResourceServer.allow({
        case "flot" :: "jquery.flot.css" :: Nil => true
        case "flot" :: "jquery.flot.js" :: Nil => true
        case "flot" :: "excanvas.js" :: Nil => true
      })
  }

  def script(xml: NodeSeq): JsCmd =
    (xml \ "script").map(x => JsRaw(x.text).cmd).foldLeft(Noop)(_ & _)

  /**
   * render a flot graph
   * <p>
   * a comet actor should use this version
   */

  def render(idPlaceholder: String,
             datas: List[FlotSerie],
             options: FlotOptions,
             script: JsCmd,
             caps: FlotCapability*
  ): NodeSeq =
  {
    val ieExcanvasPackJs = Unparsed("<!--[if IE]><script language=\"javascript\" type=\"text/javascript\" src=\"" +
                                    urlEncode(net.liftweb.http.S.contextPath) + "/" +
                                    urlEncode(LiftRules.resourceServerPath) + "/flot/excanvas.js\"></script><![endif]-->")

    // 27/06/2009 add style tag )See http://groups.google.com/group/liftweb/browse_thread/thread/5e0335583e2a248b?hl=en&pli=1)


    renderHead() ++ Script(_renderJs(idPlaceholder, datas, options, script, caps :_*))
  }

    def renderHead(): NodeSeq = {
    val ieExcanvasPackJs = Unparsed("<!--[if IE]><script language=\"javascript\" type=\"text/javascript\" src=\"" +
                                    urlEncode(net.liftweb.http.S.contextPath) + "/" +
                                    urlEncode(LiftRules.resourceServerPath) + "/flot/excanvas.js\"></script><![endif]-->")

    // 27/06/2009 add style tag )See http://groups.google.com/group/liftweb/browse_thread/thread/5e0335583e2a248b?hl=en&pli=1)


    <head>
      <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/flot/jquery.flot.js"}/>
    {ieExcanvasPackJs}
      <link rel="stylesheet" href={"/" + LiftRules.resourceServerPath + "/flot/jquery.flot.css"} type="text/css"/>
    </head>
  }


  def renderCss (idPlaceholder : String) = {
      JqId(idPlaceholder) ~> new JsExp with JsMember {
           def toJsCmd = "addClass(\"flot_lww\")"
         }
  }

  /*
   *
   */

  def renderCapability (fRender: FlotCapability => JsCmd, caps: FlotCapability *): JsCmd =
  caps.foldLeft(Noop)((js, cap) => js & fRender(cap))


  /*
   * can be used to generate AJAX response
   */

  def renderJs (
    idPlaceholder : String,
      datas : List [FlotSerie],
      options : FlotOptions,
      script: JsCmd,
      caps : FlotCapability *): JsCmd =
  datas match {
    case Nil => renderFlotHide(idPlaceholder, caps: _*)

    case _ => renderVars(idPlaceholder, datas, options) &
      renderFlotShow(idPlaceholder, datas, options, script, caps :_*)
  }

  //

  def renderFlotHide (idPlaceholder: String, caps: FlotCapability *): JsCmd =
  JsHideId(idPlaceholder) &
  renderCapability (c => c.renderHide(), caps :_*)


  // part that belongs to jQuery "document ready" function

  def renderFlotShow (
    idPlaceholder: String,
      datas: List [FlotSerie],
      options: FlotOptions,
      script: JsCmd,
      caps: FlotCapability *): JsCmd = {

    val main = FlotInfo (idPlaceholder, datas, options)

    renderCss (idPlaceholder) &
    JsShowId(idPlaceholder) &
    renderCapability (c => c.renderShow (), caps :_*) &
    JsRaw(
      "var plot_" + idPlaceholder +
      " = jQuery.plot(jQuery(" + ("#"+idPlaceholder).encJs +
      "), datas_" + idPlaceholder +
      ", options_" + idPlaceholder + ")") &
    renderCapability (c => c.render (main), caps :_*) &
    script
  }

  // generate Javascript inside "document ready" event

  def callPlotFunction(idPlaceholder: String): JsCmd = JsRaw("flot_plot_"+idPlaceholder+"();")

  private def _renderJs (
    idPlaceholder : String,
      datas : List [FlotSerie],
      options : FlotOptions,
      script: JsCmd,
      caps : FlotCapability*): JsCmd = {
    renderVars (idPlaceholder, datas, options) &
    Function("flot_plot_"+idPlaceholder, Nil, (datas match {
          case Nil => renderFlotHide(idPlaceholder, caps : _*)
          case _ => renderFlotShow(idPlaceholder, datas, options, script,
                                   caps : _*)
        })) &
    OnLoad(callPlotFunction(idPlaceholder))


  }

  /*
   private def renderJqueryScript (jqueryScript: Seq[Node]) : JsCmd = {
   jqueryScript.foldLeft ("") ( (sz,node) => {
   sz + (node match {
   case net.liftweb.util.PCData (_s) => _s
   case _ => node.toString
   })
   })
   }

   //

   val initFlot = "jQuery(function () {"
   val endFlot = "});"
   */
  /**
   * render a data value:<br/>
   * [2, 10]
   */
  def renderOneValue (one: (Double, Double)) : JsExp =
  one match {
    case (Math.NaN_DOUBLE, _) => JsNull
    case (_, Math.NaN_DOUBLE) => JsNull
    case (a, b) => JsArray(a, b)
  }


  /**
   * render serie of data:<br/>
   * [2, 10], [5, 12], [11, 2]
   */
  def renderValues(values: List[(Double, Double)]): JsExp =
  JsArray(values.map(renderOneValue) :_*)


  /**
   *
   */

  def renderDataSerie(idPlaceholder: String)(data: (FlotSerie, Int)): JsCmd =
  JsCrVar("data_"+idPlaceholder+"_"+(data._2 + 1), renderValues(data._1.data))

  /*
   * render all variables that can be modified via Javascript after first page load (for example using Ajax or comet)
   */

  def renderVars (idPlaceholder : String,
                  datas: List[FlotSerie],
                  options: FlotOptions): JsCmd =
  datas match {
    case Nil => Noop

    case _ =>
      datas.zipWithIndex.map(renderDataSerie(idPlaceholder)).
      reduceLeft(_ & _) &
      JsCrVar("datas_"+idPlaceholder, renderSeries(datas, idPlaceholder)) &
      JsCrVar("options_"+idPlaceholder, options.asJsObj)
  }



  /**
   * render one serie:<br />
   * <br />
   * <code>
   * (
   *   label: "<name_label>"
   *   lines: { show: true, fill: true }
   *   bars: { show: true }
   *   points: { show: true }
   *   data: data_[ph]_[x] where ph is the placeholder id and {x} serie's the id
   * )
   * </code>
   */

  def renderOneSerie(data: FlotSerie, idPlaceholder: String, idSerie: Int): JsObj = {
    val info: List[Box[(String, JsExp)]] =
    List(data.label.map(v => ("label", v)),
         data.lines.map(v => ("lines", v.asJsObj)),
         data.points.map(v => ("points", v.asJsObj)),
         data.bars.map(v => ("bars", v.asJsObj)),
         data.color.map {
        case Left(c) => ("color", c)
        case Right(c) => ("color", c)
      },
         data.shadowSize.map(s => ("shadowSize", s)),
         Full(("data", JsVar("data_"+idPlaceholder + "_" + idSerie))))

    JsObj(info.flatten(_.toList) :_*)
  }

  /**
   * render all series: <br />
   * <br />
   * ( <br />
   *   label: "<name_label_1>" <br />
   *   lines:  ... <br />
   *   data: [[2, 10], [5, 12], [11, 2]] <br />
   * ), <br />
   * (<br />
   *   label: "<name_label2>"<br />
   *   data: [[2, 14], [6, 4], [11, 17]]<br />
   * )<br />
   *
   */
  def renderSeries(datas: List[FlotSerie], idPlaceholder: String): JsArray =
  JsArray(datas.zipWithIndex.map{
      case (d, idx) => renderOneSerie(d, idPlaceholder, idx + 1)
    } :_*)


  //


  //
  // min: 0, max: 10, tickDecimals: 0
  // mode: "time",
  // minTickSize: [1, "month"],    // TODO
  //
  /*
   def renderAxisOptions (options: FlotAxisOptions): JsObj = {
   val info: List[Box[(String, JsExp)]] =
   List(options.min.map(v => ("min", v)),
   options.max.map(v => ("max", v)),
   options.tickDecimals.map(v => ("tickDecimals", v)),
   options.ticks match {
   case Nil => Empty
   case x :: Nil => Full(("ticks", x))
   case xs => Full(("ticks", JsArray(xs.map(d => Num(d)) :_*)))
   },
   options.mode.map(v => ("mode", v))
   )

   JsObj(info.flatten(_.toList) :_*)
   }*/

  //
  //    xaxis: { tickDecimals: 0 },
  //    yaxis: { min: 0, max: 10 },
  //
  /*
   def renderAxis (axis : String, options : FlotAxisOptions) : String = {
   axis + "axis: {" + renderAxisOptions (options) + "}"
   }
   */

  //
  //
  //


  //
  //
  //




  //
  // {
  //    lines: { show: true},
  //    points: { show: true}
  //    xaxis: { tickDecimals: 0 },
  //    yaxis: { min: 0, max: 10 },
  //    selection: { mode: "x" }
  //    legend: { noColumns: 2 },
  // }
  //
  /*
   def renderOptions(options: FlotOptions): JsExp = {
   var first = true

   def endOfLine () = {
   val ret = if (! first) ",\n      " else "      "
   first = false
   ret
   }

   val set_lines = options.lines match {
   case None => ""
   case Some (_lines) => {first=false; "lines: {" + renderLines (_lines) + "}"}
   }

   val set_points = options.points match {
   case None => ""
   case Some (_points) => {endOfLine + "points: {" + renderPoints (_points) + "}"}
   }

   val set_xaxis = options.xaxis match {
   case None => ""
   case Some (options) => {endOfLine + renderAxis ("x", options)}
   }

   val set_yaxis = options.yaxis match {
   case None => ""
   case Some (options) => {endOfLine + renderAxis ("y", options)}
   }

   val set_selection = options.modeSelection match {
   case None => ""
   case Some (mode) => {endOfLine + "selection: { mode: '" + mode + "'}"}
   }

   val set_legend = options.legend match {
   case None => ""
   case Some (_legend) => {endOfLine + "legend: {" + renderLegend (_legend) +  "}"}
   }

   val set_shadowSize = options.shadowSize match {
   case None => ""
   case Some (_shadowSize) => {endOfLine + "shadowSize: " + _shadowSize}
   }

   val set_grid = options.grid match {
   case None => ""
   case Some (_grid) => {endOfLine + "grid: {" + renderGrid (_grid) + "}"}
   }

   if (! first)
   {
   "{\n" +
   set_lines +
   set_points  +
   set_xaxis +
   set_yaxis +
   set_selection +
   set_legend +
   set_shadowSize +
   set_grid +
   "  }"
   }
   else
   "{}"
   }

   def renderId (id : String) : String = {
   "'#" + id + "'"
   }
   */
}

}
}
}
