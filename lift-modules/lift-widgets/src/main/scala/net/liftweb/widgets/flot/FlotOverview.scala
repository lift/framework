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

import scala.xml.{NodeSeq, Node, PCData, Text}

import _root_.net.liftweb.http.js._
import _root_.net.liftweb.util.Helpers._
import JsCmds._
import JE._
import _root_.net.liftweb.http.js.jquery._
import JqJE._

/**
 * create an overview graph to help zooming in the main graph
 */

class FlotOverview (idOverview: String, optionsOverview: FlotOptions)
extends FlotCapability {

  def render (main: FlotInfo): JsCmd = {
    def tv(in: String) = in+idOverview

    /*
     val sz = new StringBuilder (1000)

     sz.append ("  var options_" + idOverview + " = " + Flot.renderOptions (optionsOverview) + " ;\n\n")
     sz.append ("  var plot_" + idOverview +" = jQuery.plot(jQuery('#" + idOverview + "'), datas_" + main.idPlaceholder + ", options_" + idOverview + ") ; \n\n")

     sz.append ("  var internalSelection = false;\n")

     sz.append ("  jQuery('#" + main.idPlaceholder + "').bind('selected', function (event, area) { \n")
     sz.append ("    plot_" + main.idPlaceholder + " = jQuery.plot(jQuery('#" + main.idPlaceholder + "'), datas_" + main.idPlaceholder + ",\n")
     sz.append ("                jQuery.extend(true, {}, options_" + main.idPlaceholder + ", {\n")
     sz.append ("                    xaxis: { min: area.x1, max: area.x2 },\n")
     sz.append ("                    yaxis: { min: area.y1, max: area.y2 }\n")
     sz.append ("                }));\n\n")

     sz.append ("    if (internalSelection)\n")
     sz.append ("        return;\n")
     sz.append ("    internalSelection = true;\n")
     sz.append ("    plot_" + idOverview +".setSelection(area);\n")
     sz.append ("    internalSelection = false;\n")
     sz.append ("  });\n\n")

     sz.append ("  jQuery('#" + idOverview +"').bind('selected', function (event, area) {\n")
     sz.append ("      if (internalSelection)\n")
     sz.append ("          return;\n")
     sz.append ("      internalSelection = true;\n")
     sz.append ("      plot_" + main.idPlaceholder + ".setSelection(area);\n")
     sz.append ("      internalSelection = false;\n")
     sz.append ("  });\n")
     */


     val internalSel: JsCmd =
     JsIf(JsVar("internalSelection"), JsReturn()) &
     (JsVar("internalSelection") === true) &
     JsRaw("plot_" + main.idPlaceholder + ".setSelection(area)") &
     (JsVar("internalSelection") === false)

     val plotArea: JsCmd =
     (JsVar("plot_" + main.idPlaceholder) === JsRaw("""jQuery.plot(jQuery("""+("#" + main.idPlaceholder).encJs +
                                                    """), datas_""" + main.idPlaceholder + """,
                    jQuery.extend(true, {}, options_""" + main.idPlaceholder + """, {
                         xaxis: { min: area.x1, max: area.x2 },
                         yaxis: { min: area.y1, max: area.y2 }
                     }))""")) &
     JsIf(JsVar("internalSelection"), JsReturn()) &
     (JsVar("internalSelection") === true) &
     JsRaw("plot_"+ idOverview + ".setSelection(area)") &
     (JsVar("internalSelection") === false)

     JsCrVar(tv("options_"), optionsOverview.asJsObj) &
     JsCrVar(tv("plot_"), JsRaw("jQuery.plot(jQuery('#" + idOverview +
                                "'), datas_" + main.idPlaceholder +
                                ", options_" + idOverview + ")")) &
     JsCrVar("internalSelection", false) &
     (JqId(main.idPlaceholder) ~> JsFunc("bind", "selected",
                                         AnonFunc("event, area", plotArea))) &
     (JqId(main.idPlaceholder) ~> JsFunc("bind", "selected",
                                         AnonFunc("event, area", internalSel)))

     }

     def renderHide(): JsCmd = JsHideId(idOverview)

     def renderShow(): JsCmd = JsShowId(idOverview)
     }

}
}
}
