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

import _root_.net.liftweb.http.js._
import JsCmds._
import JE._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._


/**
 *
 */

case class JsFlot(idPlaceholder: String,datas: List[FlotSerie], options: FlotOptions) extends JsCmd
{
  lazy val jsCmd: JsCmd =  Flot.renderJs(idPlaceholder, datas, options, Noop)
  lazy val toJsCmd: String = jsCmd.toJsCmd
}

/**
 *
 */
case class JsFlotAppendData(idPlaceholder: String,
                            datas: List [FlotSerie],
                            newDatas: List [(Double, Double)], pop: Boolean) extends JsCmd
{
  def toJsCmd: String = {
    if (datas.size != newDatas.size) Noop.toJsCmd
    else {

    val newValuePush: String = newDatas.zipWithIndex.map
    {case (newData, num) => {
        val nameSerie = "data_" + idPlaceholder + "_" + (num + 1)
        val popjs = if (pop) {nameSerie + ".shift () ;\n"} else ""

        popjs + nameSerie + ".push ( [" + newData._1.toString + ", " + newData._2.toString + "]); \n"
      }
    }.reduceLeft (_ + _)

    val flotShow = Flot.renderFlotShow (idPlaceholder, datas, new FlotOptions{}, Noop).toJsCmd

    newValuePush + flotShow
    }
  }
}

/**
 *
 */

case class JsFlotWithOverview(idPlaceholder: String,
                              datas: List [FlotSerie],
                              options: FlotOptions,
                              idOverview: String,
                              optionsOverview: FlotOptions) extends JsCmd
{
  def toJsCmd: String = {
    val jsClearLegend: JsCmd =
    optionsOverview.legend.flatMap(_.container.
                                   map(c => JsRaw("jQuery("+("#" + c).encJs
                                                  + ").html ('')").cmd)).
    openOr(Noop)

    val overview = new FlotOverview (idOverview, optionsOverview)

    jsClearLegend & Flot.renderJs(idPlaceholder, datas, options, Noop, overview)
  }
}

}
}
}
