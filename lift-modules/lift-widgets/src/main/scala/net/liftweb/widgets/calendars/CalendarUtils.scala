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

import _root_.net.liftweb.http.js._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http.js.JE._
import _root_.net.liftweb.http.js.JsCmds._
import _root_.java.text.SimpleDateFormat
import _root_.java.util.Calendar
import _root_.java.util.Calendar._


object CalendarUtils {
  private lazy val timeFormatter = new SimpleDateFormat("h:m")

  import JE._
  import JsCmds._

  /**
   * Returns the JSON representation of the list of CalendrItem sorted by start time.
   */
  def toJSON(items: List[CalendarItem]): JsExp = {

    JsObj(("items", JsArray(items.sort((e1, e2) => e1.start before(e2 start)) map(c => {

      val (sh, sm) = (c.start.get(HOUR_OF_DAY), c.start.get(MINUTE));
      val (eh, em) = c.end map (c => (c.get(HOUR_OF_DAY), c.get(MINUTE))) openOr (48, 0)

      val startIndex = sm match {
        case x if sm >= 30 => sh*2 + 1
        case _ => sh*2
      }

      val endIndex = em match {
        case x if em > 30 => eh*2 + 2
        case x if em > 0 => eh*2 + 1
        case _ => eh*2
      }

      var items: List[(String, JsExp)] = ("id", Str(c.id)) ::
        ("start", JsRaw(startIndex toString)) ::
        ("end", c.end map(c => JsRaw(endIndex toString)) openOr JsRaw("48")) ::
        ("weekDay", Str(weekDay(c start) toString)) ::
        ("startTime", Str(timeFormatter.format(c.start getTime))) ::
        ("subject", Str(c.subject openOr "")) :: 
        ("month", JsRaw( c.start.get(MONTH) toString )) :: 
        ("dayOfMonth", JsRaw( dayOfMonth(c start) toString )) :: Nil

      items = c.description map(desc => items ++ (("description", Str(desc)) :: Nil) ) openOr items
      items = c.baseCSSClassName map(name => items ++ (("cssClass", Str(name)) :: Nil) ) openOr items

      JsObj(items:_*)


    }):_*)))
  }

  def dayOfMonth(cal: Calendar) = cal get (DAY_OF_MONTH)
  def weekDay(cal: Calendar) = cal get (DAY_OF_WEEK)

  def sameDay(c1: Calendar, c2: Calendar) = (c1.get(DAY_OF_MONTH) == c2.get(DAY_OF_MONTH)) &&
                                             (c1.get(MONTH) == c2.get(MONTH)) &&
                                             (c1.get(YEAR) == c2.get(YEAR))

}

}
}
}
