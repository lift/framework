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

import _root_.java.util.Calendar
import _root_.java.util.Calendar._
import _root_.net.liftweb.common.{Box, Empty, Full}

object CalendarItem {
  def apply(id: String, start: Calendar, calendarType: CalendarType.Value) = new CalendarItem(id, start, calendarType)
}
case class CalendarItem(id: String,
                        start: Calendar,
                        calendarType: CalendarType.Value,
                        end: Box[Calendar],
                        subject: Box[String],
                        description: Box[String],
                        baseCSSClassName: Box[String]) {

  def this(id: String, start: Calendar, calendarType: CalendarType.Value) = this(id, start, calendarType, Empty, Empty, Empty, Empty)

  def end (end: Calendar) = CalendarItem(id, start, calendarType, Box.legacyNullTest(end), Empty, Empty, Empty)
  def subject (subject: String) = CalendarItem(id, start, calendarType, Empty, Box.legacyNullTest(subject), Empty, Empty)
  def description(description: String) = CalendarItem(id, start, calendarType, Empty, Empty, Box.legacyNullTest(description), Empty)
  /**
   * Defines the base name of the css classes describing the calendar items rendered.
   * For instance if "greenItem" is provided the css needs to have the following classes:
   * greenItem, greenItemHead and greenItemBody. For exemplification please see calendarItem,
   * calendarItemHead and calendarItemBody classes. If this is missing calendarItem base name
   * will be assumed by JS code.
   */
  def baseCSSClassName(name: String) = CalendarItem(id, start, calendarType, Empty, Empty, Empty, Box.legacyNullTest(name))

  private def choose[T](l: Box[T], r: Box[T]): Box[T] = l match {case Empty => r case _ => l}

  def optional(f: (CalendarItem) => CalendarItem*): CalendarItem = {
    f.map(c => c(this)).foldLeft(this)((l, r) => CalendarItem(id, start, calendarType,
        choose(l end, r end),
        choose(l subject, r subject),
        choose(l description, r description),
        choose(l baseCSSClassName, r baseCSSClassName))
    )
  }
}

object CalendarType extends Enumeration {
  val MEETING, ANIVERSARY, EVENT, ALLDAY = Value
}

}
}
}
