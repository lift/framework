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

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._

import _root_.net.liftweb.http.js._
import JsCmds._
import JE._
import _root_.net.liftweb.http.js.jquery._
import JqJE._

/**
 * axis options
 */

trait FlotAxisOptions extends BaseFlotOptions {
  def tickDecimals: Box[Double] = Empty
  def min: Box[Double] = None
  def max: Box[Double] = None
  def mode: Box[String] = Empty
  def ticks: List[Double] = Nil //  null or number or ticks array or (fn: range -> ticks array)

   protected def buildOptions =
   List(c("min", min),
        c("max", max),
        c("tickDecimals", tickDecimals),
        c("mode", mode),
        ticks match {
        case Nil => Empty
        case x :: Nil => Full(("ticks", x))
        case xs => Full(("ticks", JsArray(xs.map(d => Num(d)) :_*)))
      })
  /* TODO
   autoscaleMargin: null or number
   labelWidth: null or number
   labelHeight: null or number
   tickSize: number or array
   minTickSize: number or array
   tickFormatter: (fn: number, object -> string) or string
   */
}

/**
 * legend options
 */

trait BaseFlotOptions {
  def asJsObj: JsObj = JsObj(buildOptions.flatten(_.toList) :_*)

  protected def buildOptions: List[Box[(String, JsExp)]]

  implicit def optionToJsE(in: BaseFlotOptions): JsExp = in.asJsObj

  def c[T](name: String,value: Box[T])(implicit cvt: T => JsExp):
  Box[(String, JsExp)] = value.map(v => (name, cvt(v)))
}

trait FlotLegendOptions extends BaseFlotOptions {
  def show: Box[Boolean] = Empty
  // def labelFormatter: Box[String] = Empty // null or (fn: string -> string)
  def labelBoxBorderColor: Box[String] = Empty // color
  def noColumns: Box[Int] = Empty // number
  def position: Box[String] = Empty // "ne" or "nw" or "se" or "sw"
  def margin: Box[Int] = Empty // number of pixels
  def backgroundColor: Box[String] = Empty //  null or color
  def backgroundOpacity: Box[Double] = Empty // number in 0.0 - 1.0
  def container: Box[String] = Empty // null or jQuery object

  protected def buildOptions =
  List(
    show.map(v => ("show", v)),
      labelBoxBorderColor.map(v => ("labelBoxColor", v)),
      noColumns.map(v => ("noColumns", v)),
      position.map(v => ("position", v)),
      margin.map(v => ("margin", v)),
      backgroundColor.map(v => ("backgroundColor", v)),
      backgroundOpacity.map(v => ("backgroundOpacity", v)),
      container.map(v => ("container", JqId(v)))
  )
}

/**
 * lines options and points/bars options parent
 */

trait FlotLinesOptions extends BaseFlotOptions {
  def show: Box[Boolean] = Empty
  def lineWidth: Box[Int] = Empty
  def fill: Box[Boolean] = Empty // TODO: boolean or number
  def fillColor: Box[String] = Empty

  protected def buildOptions =
  List(
    show.map(v => ("show", v)),
      lineWidth.map(v => ("lineWidth", v)),
      fill.map(v => ("fill", v)),
      fillColor.map(v => ("fillColor", v))
  )
}

trait FlotPointsOptions extends FlotLinesOptions with BaseFlotOptions {
  def radius: Box[Int] = Empty // TODO
  override protected def buildOptions =
  radius.map(v => ("radius", Num(v))) :: super.buildOptions
}

trait FlotBarsOptions extends FlotLinesOptions with BaseFlotOptions {
  def barWidth: Box[Int] = Empty // TODO
  override protected def buildOptions =
  barWidth.map(v => ("barWidth", Num(v))) :: super.buildOptions
}

/**
 * grid options
 */

trait FlotGridOptions extends BaseFlotOptions {
  def color: Box[String] = Empty
  def backgroundColor: Box[String] = Empty
  def tickColor: Box[String] = Empty
  def labelMargin: Box[Int] = Empty
  def coloredAreasColor: Box[String] = Empty
  def borderWidth: Box[Int] = Empty
  def clickable: Box[Boolean] = Empty
  def hoverable: Box[Boolean] = Empty
  def coloredAreas: Box[String] = Empty // only (fn: plot area -> array of areas)

  def buildOptions =
  List(c("color", color),
       backgroundColor.map(v => ("backgroundColor", v)),
       tickColor.map(v => ("tickColor", v)),
       labelMargin.map(v => ("labelMargin", v)),
       coloredAreasColor.map(v => ("coloredAreasColor", v)),
       borderWidth.map(v => ("borderWidth", v)),
       clickable.map(v => ("clickable", v)),
       hoverable.map(v => ("hoverable", v)),
       coloredAreas.map(v => ("coloredAreas", v))
  )

  /* TODO
   coloredAreas: array of areas or (fn: plot area -> array of areas)
   */
}

/**
 * Options
 */

trait FlotOptions extends BaseFlotOptions {
  @deprecated def lines: Box[FlotLinesOptions] = Empty
  @deprecated def points: Box[FlotPointsOptions] = Empty
  def legend: Box[FlotLegendOptions] = Empty
  def xaxis: Box[FlotAxisOptions] = Empty
  def yaxis: Box[FlotAxisOptions] = Empty
  def modeSelection: Box[String] = Empty
  @deprecated def shadowSize: Box[Int] = Empty
  def grid: Box[FlotGridOptions] = Empty
  def series: Box[Map[String, JsExp]] = Empty

  def buildOptions =
  List(
    lines.map(v => ("lines", v.asJsObj)),
    points.map(v => ("points", v.asJsObj)),
    legend.map(v => ("legend", v.asJsObj)),
    xaxis.map(v => ("xaxis", v.asJsObj)),
    yaxis.map(v => ("yaxis", v.asJsObj)),
    modeSelection.map(v => ("selection", JsObj("mode" -> v))),
    c("shadowSize", shadowSize),
    c("grid", grid),
    series.map(v => ("series", JsObj(v.toSeq: _*)))
  )

}

}
}
}
