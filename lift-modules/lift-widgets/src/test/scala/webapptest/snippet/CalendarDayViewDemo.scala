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

package webapptest {
package snippet {

import _root_.scala.xml._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.S._
import _root_.net.liftweb.http.SHtml._
import _root_.net.liftweb.http.{RequestVar}
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.common.Box._
import _root_.net.liftweb.http.js._
import _root_.java.util.{Calendar, Locale}
import _root_.java.util.Calendar._

import _root_.net.liftweb.widgets.calendars.{CalendarDayView, DayViewMeta, CalendarItem, CalendarType}

class CalendarDayViewDemo {

  def render(html: Group) : NodeSeq = {

    val c1 = Calendar getInstance;
    c1.set(HOUR_OF_DAY, 0)
    c1.set(MINUTE, 40)
    c1.set(DAY_OF_MONTH, 17)
    c1.set(MONTH, 4)

    val c1End = Calendar getInstance;
    c1End.set(HOUR_OF_DAY, 8)
    c1End.set(MINUTE, 45)
    c1End.set(DAY_OF_MONTH, 17)
    c1End.set(MONTH, 4)

    val c2 = Calendar getInstance;
    c2.set(HOUR_OF_DAY, 0)
    c2.set(MINUTE, 45)
    c2.set(DAY_OF_MONTH, 17)
    c2.set(MONTH, 4)

    val c2End = Calendar getInstance;
    c2End.set(HOUR_OF_DAY, 6)
    c2End.set(MINUTE, 15)
    c2End.set(DAY_OF_MONTH, 17)
    c2End.set(MONTH, 4)

    val c3 = Calendar getInstance;
    c3.set(HOUR_OF_DAY, 5)
    c3.set(MINUTE, 30)
    c3.set(DAY_OF_MONTH, 17)
    c3.set(MONTH, 4)

    val c3End = Calendar getInstance;
    c3End.set(HOUR_OF_DAY, 13)
    c3End.set(MINUTE, 31)
    c3End.set(DAY_OF_MONTH, 17)
    c3End.set(MONTH, 4)

    val c4 = Calendar getInstance;
    c4.set(HOUR_OF_DAY, 11)
    c4.set(MINUTE, 30)
    c4.set(DAY_OF_MONTH, 17)
    c4.set(MONTH, 4)

    val c4End = Calendar getInstance;
    c4End.set(HOUR_OF_DAY, 17)
    c4End.set(MINUTE, 15)
    c4End.set(DAY_OF_MONTH, 17)
    c4End.set(MONTH, 4)

    val c5 = Calendar getInstance;
    c5.set(HOUR_OF_DAY, 11)
    c5.set(MINUTE, 30)
    c5.set(DAY_OF_MONTH, 17)
    c5.set(MONTH, 4)

    val c5End = Calendar getInstance;
    c5End.set(HOUR_OF_DAY, 15)
    c5End.set(MINUTE, 15)
    c5End.set(DAY_OF_MONTH, 17)
    c5End.set(MONTH, 4)


    val item1 = CalendarItem("1234", c1, CalendarType.MEETING) optional (
        _ end (c1End),
        _ subject("Hi there"),
        _ description("We really need to meet. Enough with standard JEE deployments that just do not scale :)")
        )
    val item2 = CalendarItem("2234", c2, CalendarType.MEETING) optional (
        _ end (c2End),
        _ subject("Hmmm... ")
        )

    val item3 = CalendarItem("werw", c3, CalendarType.MEETING) optional (
        _ end (c3End),
        _ subject("Another... ")
        )

    val item4 = CalendarItem("3453", c4, CalendarType.MEETING) optional (
        _ end (c4End),
        _ subject("Important meeting "),
        _ description("We really need to meet"),
        _ baseCSSClassName("greenItem")
        )

    val item5 = CalendarItem("asdff", c5, CalendarType.MEETING) optional (
        _ end (c5End),
        _ subject("Quick chat")
        )

    val list = item1 :: item2 :: item3 :: item4 :: item5 :: Nil;

    val c = Calendar getInstance;
    c.set(DAY_OF_MONTH, 17)
    c.set(MONTH, 4)
    bind("cal", html,
         "widget" -> CalendarDayView(c, DayViewMeta(Locale.getDefault()), list, itemClick)
    )

  }

  import JE._
  import JsCmds._


  def itemClick = Full(AnonFunc("elem, param", JsRaw("alert(CalendarDayView.getItemById(calendars.items, param).subject  + ' - ' + elem.nodeName)")))

}

}
}
