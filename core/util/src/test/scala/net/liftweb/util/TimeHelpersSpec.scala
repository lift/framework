/*
 * Copyright 2006-2011 WorldWide Conferencing, LLC
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

import java.util.{Calendar, Date}

import org.joda.time.DateTime
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.specs2.time.NoTimeConversions
import org.scalacheck.Gen._
import org.scalacheck.Prop._

import common._
import TimeHelpers._



/**
 * Systems under specification for TimeHelpers.
 */
object TimeHelpersSpec extends Specification with ScalaCheck with TimeAmountsGen with NoTimeConversions {
  "TimeHelpers Specification".title

  "the TimeHelpers" should {
    "provide a 'seconds' function transforming a number of seconds into millis" in {
      seconds(3) must_== 3 * 1000
    }
    "provide a 'minutes' function transforming a number of minutes into millis" in {
      minutes(3) must_== 3 * 60 * 1000
    }
    "provide a 'hours' function transforming a number of hours into milliss" in {
      hours(3) must_== 3 * 60 * 60 * 1000
    }
    "provide a 'days' function transforming a number of days into millis" in {
      days(3) must_== 3 * 24 * 60 * 60 * 1000
    }
    "provide a 'weeks' function transforming a number of weeks into millis" in {
      weeks(3) must_== 3 * 7 * 24 * 60 * 60 * 1000
    }
    "provide a noTime function on Date objects to transform a date into a date at the same day but at 00:00" in {
      hourFormat(now.noTime) must_== "00:00:00"
    }

    "make sure noTime does not change the day" in {
      val dateWithoutTime = new DateTime(2015, 1, 1, 0, 0)
      val dateWithTime = dateWithoutTime.withHourOfDay(2).withMinuteOfHour(3)
      dateWithTime.toDate.noTime must_== dateWithoutTime.toDate
    }

    "provide a day function returning the day of month corresponding to a given date (relative to UTC)" in {
      day(today.setTimezone(utc).setDay(3).getTime) must_== 3
    }
    "provide a month function returning the month corresponding to a given date" in {
      month(today.setTimezone(utc).setMonth(4).getTime) must_== 4
    }
    "provide a year function returning the year corresponding to a given date" in {
      year(today.setTimezone(utc).setYear(2008).getTime) must_== 2008
    }
    "provide a millisToDays function returning the number of days since the epoch time" in {
      millisToDays(new Date(0).getTime) must_== 0
      millisToDays(today.setYear(1970).setMonth(0).setDay(1).getTime.getTime) must_== 0 // the epoch time
      // on the 3rd day after the epoch time, 2 days are passed
      millisToDays(today.setTimezone(utc).setYear(1970).setMonth(0).setDay(3).getTime.getTime) must_== 2
    }
    "provide a daysSinceEpoch function returning the number of days since the epoch time" in {
      daysSinceEpoch must_== millisToDays(now.getTime)
    }
    "provide a time function creating a new Date object from a number of millis" in {
      time(1000) must_== new Date(1000)
    }
    "provide a calcTime function returning the time taken to evaluate a block in millis and the block's result" in {
      val (time, result) = calcTime((1 to 10).reduceLeft[Int](_ + _))
      time.toInt must beCloseTo(0, 1000)  // it should take less than 1 second!
      result must_== 55
    }

    "provide a hourFormat function to format the time of a date object" in {
      hourFormat(Calendar.getInstance(utc).noTime.getTime) must_== "00:00:00"
    }

    "provide a formattedDateNow function to format todays date" in {
      formattedDateNow must beMatching("\\d\\d\\d\\d/\\d\\d/\\d\\d")
    }
    "provide a formattedTimeNow function to format now's time with the TimeZone" in {
      val regex = "\\d\\d:\\d\\d (....?|GMT((\\+|\\-)\\d\\d:00)?)"
      "10:00 CEST" must beMatching(regex)
      "10:00 GMT+02:00" must beMatching(regex)
      "10:00 GMT" must beMatching(regex)
      "10:00 XXX" must beMatching(regex)
      formattedTimeNow must beMatching(regex)
    }

    "provide a parseInternetDate function to parse a string formatted using the internet format" in {
      parseInternetDate(internetDateFormatter.format(now)).getTime.toLong must beCloseTo(now.getTime.toLong, 1000L)
    }
    "provide a parseInternetDate function returning new Date(0) if the input date cant be parsed" in {
      parseInternetDate("unparsable") must_== new Date(0)
    }
    "provide a toInternetDate function formatting a date to the internet format" in {
      toInternetDate(now) must beMatching("..., \\d* ... \\d\\d\\d\\d \\d\\d:\\d\\d:\\d\\d .*")
    }
    "provide a toDate returning a Full(date) from many kinds of objects" in {
      val d = now
      List(null, Nil, None, Failure("", Empty, Empty)) forall { toDate(_) must_== Empty }
      List(Full(d), Some(d), List(d)) forall { toDate(_) must_== Full(d) }

      toDate(internetDateFormatter.format(d)) must beLike {
        case Full(converted) =>
          converted.getTime.toLong must beCloseTo(d.getTime.toLong, 1000L)
      }
    }
  }

  "The Calendar class" should {
    "have a setDay method setting the day of month and returning the updated Calendar" in {
      day(today.setTimezone(utc).setDay(1).getTime) must_== 1
    }
    "have a setMonth method setting the month and returning the updated Calendar" in {
      month(today.setTimezone(utc).setMonth(0).getTime) must_== 0
    }
    "have a setYear method setting the year and returning the updated Calendar" in {
      year(today.setTimezone(utc).setYear(2008).getTime) must_== 2008
    }
    "have a setTimezone method to setting the time zone and returning the updated Calendar" in {
      today.setTimezone(utc).getTimeZone must_== utc
    }
    "have a noTime method to setting the time to 00:00:00 and returning the updated Calendar" in {
      hourFormat(today.noTime.getTime) must_== "00:00:00"
    }
  }
}