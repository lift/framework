/*
 * Copyright 2013 WorldWide Conferencing, LLC
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

package net.liftweb
package util

import common._
import Helpers.{asInt, tryo}

import java.util.Date

import org.joda.time._
import org.joda.time.format._

object JodaHelpers extends JodaHelpers

trait JodaHelpers {
  def dateTimeFormatter: DateTimeFormatter = ISODateTimeFormat.dateTime
  def toDateTime(in: Any): Box[DateTime] = {
    try {
      in match {
        case null => Empty
        case d: Date => Full(new DateTime(d))
        case d: DateTime => Full(d)
        case lng: Long => Full(new DateTime(lng))
        case lng: Number => Full(new DateTime(lng.longValue))
        case Nil | Empty | None | Failure(_, _, _) => Empty
        case Full(v) => toDateTime(v)
        case Some(v) => toDateTime(v)
        case v :: vs => toDateTime(v)
        case s: String => tryo(DateTime.parse(s, dateTimeFormatter))
        case o => toDateTime(o.toString)
      }
    } catch {
      case e: Exception => Failure("Bad date: "+in, Full(e), Empty)
    }
  }
}
