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

import _root_.java.util.{Calendar, Locale}
import _root_.java.util.Calendar._
import _root_.java.text.SimpleDateFormat


abstract class ViewMeta(locale : Locale) {
  var timeFormatter = new SimpleDateFormat("h a", locale)
  var weekDaysFormatter = new SimpleDateFormat("EEEE", locale)
  var dateFormatter = new SimpleDateFormat("M/dd", locale)
}

case class WeekViewMeta(firstDayOfWeek: Int, locale: Locale) extends ViewMeta(locale)

case class MonthViewMeta(firstDayOfWeek: Int, locale: Locale) extends ViewMeta(locale)

case class DayViewMeta(locale: Locale) extends ViewMeta(locale)

}
}
}
