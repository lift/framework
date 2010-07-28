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
import _root_.net.liftweb.http.js.jquery._
import _root_.net.liftweb.http.SHtml._
import JsCmds._
import JE._
import JqJsCmds._
import JqJE._

object CalendarMonthView {

  /**
   * Call this function typically in boot
   */
  def init() {
    import _root_.net.liftweb.http.ResourceServer
    ResourceServer.allow({
      case "calendars" :: tail => true
      case "common" :: _ => true
    })
  }

  def apply(when: Calendar,
            calendars: Seq[CalendarItem],
            itemClick: Box[AnonFunc],
            dayClick: Box[AnonFunc],
            weekClick: Box[AnonFunc]) = new CalendarMonthView(when).render(calendars, itemClick, dayClick, weekClick)

  def apply(when: Calendar,
            meta: MonthViewMeta,
            calendars: Seq[CalendarItem],
            itemClick: Box[AnonFunc],
            dayClick: Box[AnonFunc],
            weekClick: Box[AnonFunc]) = new CalendarMonthView(when, meta) render(calendars, itemClick, dayClick, weekClick)


}

/**
 * CalendarMonthView renders a month view representation of a collection of CalendarItem
 * <br>
 * Usage example - assume CalendarView is a typical LiftWeb snippet
 * <pre>
 * class CalendarView {
 *
 *  def render(html: Group) : NodeSeq = {
 *    val c = Calendar getInstance;
 *    c.set(MONTH, 4)
 *    bind("cal", html,
 *         "widget" --> CalendarMonthView(c, makeCals, itemClick, dayClick, weekClick)
 *    )
 *  }
 *
 *  import JE._
 *  import JsCmds._
 *
 *  def itemClick = Full(AnonFunc("elem, param", JsRaw("alert(param + ' - ' + elem.nodeName)")))
 *  def dayClick = Full(AnonFunc("elem, param", JsRaw("alert(param + ' - ' + elem.nodeName)")))
 *  def weekClick = Full(AnonFunc("elem, param", JsRaw("alert(param + ' - ' + elem.nodeName)")))
 *
 *
 *  private def makeCals = {
 *    val c1 = Calendar getInstance
 *    val c2 = Calendar getInstance;
 *    val c3 = Calendar getInstance;
 *
 *    c2.set(DAY_OF_MONTH, 3)
 *    c3.set(DAY_OF_MONTH, 1)
 *    c3.set(MONTH, 4)
 *
 *    val item1 = CalendarItem(...)
 *    val item2 = CalendarItem(...)
 *    val item3 = CalendarItem(...)
 *
 *    item1 :: item2 :: item3 ::  Nil
 *  }
 * }
 *
 * </pre>
 *
 * @param when - the Calendar object describing the month that needs tobe rendered
 *
 */
class CalendarMonthView(val when: Calendar, val meta: MonthViewMeta) {

  def this(when: Calendar) = this(when, MonthViewMeta(MONDAY, Locale getDefault))

  /**
   * Returns the markup for rendering the calendar month view
   *
   * @param calendars - the calendar items than need to be rendered
   * @param itemClick - Ajax function to be called when a calendar item was clicked.
   *                    It takes two parameters: elem the node that was clicked and
   *                    param the identifier if this CalendarItem
   * @param dayClick - Ajax function to be called when a day number(cell header) item was clicked
   *                   It takes two parameters: elem the node that was clicked and
   *                   param the date of the clicked day in MM/dd/yyyy format
   * @param weekClick - Ajax function to be called when a day number(cell header) item was clicked
   *                   It takes two parameters: elem the node that was clicked and
   *                   the week number
   * @return NodeSeq - the markup to be rendered
   */
  def render(calendars: Seq[CalendarItem],
             itemClick: Box[AnonFunc],
             dayClick: Box[AnonFunc],
             weekClick: Box[AnonFunc]): NodeSeq = {

     def predicate (current: Calendar, c: CalendarItem) = {
       // Adjust the precision
       current.set(MILLISECOND, c.start.get(MILLISECOND))
       current.set(SECOND, c.start.get(SECOND))
       current.set(MINUTE, c.start.get(MINUTE))
       current.set(HOUR_OF_DAY, c.start.get(HOUR_OF_DAY))

       c end match {
         case Full(end) => {
           val crt = current getTimeInMillis;
           (crt >= c.start.getTimeInMillis) && (crt <= end.getTimeInMillis)
         }
         case _ => current.get(DAY_OF_MONTH) >= c.start.get(DAY_OF_MONTH) && current.get(MONTH) >= c.start.get(MONTH)
       }
     }

    def makeCells(calendar: Calendar): NodeSeq = {

      val thisMonth = when get(MONTH)
      val cal = calendar.clone().asInstanceOf[Calendar]
      val today = Calendar getInstance (meta locale)
      (0 to 5) map (row => <tr><td wk={cal get(WEEK_OF_YEAR) toString} class="cellWeek">
        {cal get(WEEK_OF_YEAR)}</td>{(0 to 6) map (col =>
        try{
         <td class="day">{
            val day = cal.get(DAY_OF_MONTH)
            val month = cal.get(MONTH)
            val isToday = today.get(DAY_OF_MONTH) == cal.get(DAY_OF_MONTH) && (month == today.get(MONTH))
            val div = <div></div> % ("id" -> "month_%s_day_%s".format(month,day))
            val (head, cell) = isToday match {
              case true => ("cellHeadToday", "cellBodyToday")
              case _ => (month != thisMonth) match {
                case true => ("cellHeadOtherMonth", "cellBodyOtherMonth")
                case _ => ("cellHead", "cellBody")
              }
            }
            Group(<div>{day}</div> %
              ("class" -> head) ::
              div % ("class" -> cell) :: Nil)
          }</td> % ("date" -> (dateFormatter format(cal getTime))) 
        } finally {
          cal add(DAY_OF_MONTH, 1)
        }
        )
      }</tr>)
    }

    def makeHead(headCal: Calendar) = <tr><td></td>{
      (0 to 6) map(x => <td width="14%">{
        try{
          meta.weekDaysFormatter format(headCal getTime)
        } finally {
          headCal add(DAY_OF_MONTH, 1)
        }
      }</td>)
    }</tr>

    val cal = when.clone().asInstanceOf[Calendar]
    cal set(DAY_OF_MONTH, 1)
    val delta = cal.get(DAY_OF_WEEK) - meta.firstDayOfWeek
    cal add(DAY_OF_MONTH, if (delta < 0) -delta-7 else -delta)

    val headCal = cal.clone().asInstanceOf[Calendar]

      <head>
        <link rel="stylesheet" href={"/" + LiftRules.resourceServerPath + "/calendars/monthview/style.css"} type="text/css"/>
        <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/common/jquery.dimensions.js"}/>
        <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/calendars/js/calendarviews.js"}/>
        <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/common/jquery.bgiframe.js"}/>
        <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/common/jquery.tooltip.js"}/>
        {Script((JsCrVar("itemClick", itemClick openOr JsRaw("function(param){}")) &
        JsCrVar("dayClick", dayClick openOr JsRaw("function(param){}")) &
        JsCrVar("weekClick", weekClick openOr JsRaw("function(param){}")) &
        JsCrVar("calendars", CalendarUtils.toJSON(calendars.toList)) &
        JsRaw("""
         jQuery(document).ready(function() {
            CalendarMonthView.buildMonthViewCalendars();
            
            jQuery('.day').bind('click',function(event){
              dayClick(this,jQuery(this).attr('date'));
            });
            
            jQuery('.cellWeek').bind('click',function(){
              weekClick(this, jQuery(this).attr('wk'));
            });
            
          })
         """)))}
      </head>
      <div class="monthView">{
        <table width="100%" cellspacing="1" cellpadding="0" style="table-layout: fixed;" class="topHead">
          {makeHead(headCal)}
          {makeCells(cal)}
        </table>
      }</div>
  }
}

}
}
}
