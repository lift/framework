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
package calendars {

import _root_.scala.xml._
import _root_.java.util.{Calendar, Locale}
import _root_.java.util.Calendar._
import _root_.java.text.SimpleDateFormat
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.common.{Box, Full, Empty}
import _root_.net.liftweb.http.{LiftRules}
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.http.SHtml._
import JsCmds._
import JE._

object CalendarWeekView {

  /**
   * Call this function typically in boot
   */
  def init() {
    import _root_.net.liftweb.http.ResourceServer
    ResourceServer.allow({
      case "calendars" :: _ => true
      case "common" :: _ => true
    })
  }

  def apply(when: Calendar,
      calendars: List[CalendarItem],
      itemClick: Box[AnonFunc]) = new CalendarWeekView(when).render(calendars, itemClick)

  def apply(when: Calendar,
      meta: WeekViewMeta,
      calendars: List[CalendarItem],
      itemClick: Box[AnonFunc]) = new CalendarWeekView(when, meta) render(calendars, itemClick)

}

class CalendarWeekView(val when: Calendar, val meta: WeekViewMeta) {

  def this(when: Calendar) = this(when, WeekViewMeta(MONDAY, Locale getDefault))

  def makeHead(headCal: Calendar) = <tr><td></td>{
    (0 to 6) map(x => <td width="14%">{
      try{
        meta.weekDaysFormatter format(headCal getTime)
      } finally {
        headCal add(DAY_OF_MONTH, 1)
      }
    }</td>)
  }</tr>


  def render(calendars: List[CalendarItem], itemClick: Box[AnonFunc]): NodeSeq = {

    val cal = when.clone().asInstanceOf[Calendar]
    cal.set(Calendar.MILLISECOND, 0)
    cal.set(Calendar.SECOND, 0)
    cal.set(Calendar.MINUTE, 0)
    cal.set(Calendar.HOUR_OF_DAY, 0)

    val delta = cal.get(DAY_OF_WEEK) - meta.firstDayOfWeek

    cal add(DAY_OF_MONTH, if (delta < 0) -delta-7 else -delta)

    val lastCal = cal.clone().asInstanceOf[Calendar]
    lastCal.add(DAY_OF_MONTH, 7);

    val startIndex = cal.get(DAY_OF_WEEK)

    val f : (Int, Int) => Int = {
      case (day, startIndex) =>
        var st = day + startIndex
        if (st > 7) {
          st = st - 7
        }
        st
    }

    val headCal = cal.clone().asInstanceOf[Calendar]

    <head>
      <link rel="stylesheet" href={"/" + LiftRules.resourceServerPath + "/calendars/weekview/style.css"} type="text/css"/>
      <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/common/jquery.dimensions.js"}/>
      <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/calendars/js/calendarviews.js"}/>
      <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/common/jquery.bgiframe.js"}/>
      <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/common/jquery.tooltip.js"}/>
    {Script(
      JsCrVar("itemClick", itemClick openOr JsRaw("function(param){}")) &
      JsCrVar("calendars", CalendarUtils.toJSON(calendars filter (c => c.start.after(cal) && c.start.before(lastCal)))) &
      JsRaw("""
         jQuery(document).ready(function() {
            CalendarWeekView.buildWeekViewCalendars();
          })
         """)
      )}
    </head>

    <div class="weekView">
    <div class="wkHead">
      <table cellspacing="0" cellpading="0" style="width: 100%;">
       <tr>
         <td  class="wkHour"><div></div></td>
         {
          (for (val day <- 0 to 6) yield
              <td class="wkHeadCell">{
                try{
                  val time = headCal.getTime
                  meta.weekDaysFormatter.format(time) + " " + dateFormatter.format(time)
                } finally {
                  headCal add(DAY_OF_MONTH, 1)
                }
              }</td>
          )
         }
        </tr>
      </table>
    </div>
    <div class="weekViewBody">
      <table cellspacing="0" cellpading="0" style="width: 100%;">
      {
        val cal = Calendar getInstance;
        cal set(HOUR_OF_DAY, 0)
        cal set(MINUTE, 0)
        for (val i <- 0 to 23) yield
        try{
          <tr>
            <td class="wkHour"><div>{(meta.timeFormatter format(cal getTime)).toString}</div></td>
           {
              <td id={("wkhidx_" + startIndex + "_" + (i*2 toString))} class="wkCell borderDashed"></td> ++
                (for (val day <- 1 to 6) yield {
                    <td id={("wkhidx_" + (f(day, startIndex)) + "_" + (i*2 toString))} class="wkCell borderDashed borderLeft"></td>
                 }
                )
            }
          </tr>
          <tr>
            <td class="wkHour borderSolid"></td>
            {
              <td id={("wkhidx_" + startIndex + "_" + ((i*2+1) toString))} class="wkCell borderSolid"></td> ++
                (for (val day <- 1 to 6) yield
                    <td id={("wkhidx_" + (f(day, startIndex)) + "_" + ((i*2+1) toString))} class="wkCell borderSolid borderLeft"></td>
                )
            }
          </tr>
        } finally {
          cal add(HOUR_OF_DAY, 1)
        }
      }</table>
    </div>
    </div>
  }
}

}
}
}
