/*
 * Copyright 2006-2015 WorldWide Conferencing, LLC
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

import java.util.{Calendar, Date, TimeZone}

import net.liftweb.common._
import net.liftweb.util.TimeHelpers._
import org.joda.time.{Period, DateTimeZone, DateTime}
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.specs2.ScalaCheck
import org.specs2.execute.AsResult
import org.specs2.matcher.MatchersImplicits
import org.specs2.mutable.{Around, Specification}

/**
 * Systems under specification for TimeHelpers.
 */
class TimeHelpersSpec extends Specification with ScalaCheck with TimeAmountsGen {
  "TimeHelpers Specification".title

  "A TimeSpan" can {
    "be created from a number of milliseconds" in forAllTimeZones {
      TimeSpan(3000) === TimeSpan(3 * 1000)
    }
    "be created from a number of seconds" in forAllTimeZones {
      3.seconds === TimeSpan(3 * 1000)
    }
    "be created from a number of minutes" in forAllTimeZones {
      3.minutes === TimeSpan(3 * 60 * 1000)
    }
    "be created from a number of hours" in forAllTimeZones {
      3.hours === TimeSpan(3 * 60 * 60 * 1000)
    }
    "be created from a number of days" in forAllTimeZones {
      3.days === TimeSpan(3 * 24 * 60 * 60 * 1000)
    }
    "be created from a number of weeks" in forAllTimeZones {
      3.weeks === TimeSpan(3 * 7 * 24 * 60 * 60 * 1000)
    }
    "be created from a number of months" in forAllTimeZones {
      3.months === Period.months(3)
    }
    "be created from a number of years" in forAllTimeZones {
      3.years === Period.years(3)
    }
    "be converted implicitly to a date starting from the epoch time" in forAllTimeZones {
      3.seconds.after(new Date(0)) must beTrue
    }
    "be converted to a date starting from the epoch time, using the date method" in forAllTimeZones {
      3.seconds.after(new Date(0)) must beTrue
    }
    "be compared to another TimeSpan" in forAllTimeZones {
      3.seconds === 3.seconds
      3.seconds must_!= 2.seconds
    }
    "be compared to another object" in forAllTimeZones {
      3.seconds must_!= "string"
    }
  }

  "A TimeSpan" should {
    "return a new TimeSpan representing the sum of the 2 times when added with another TimeSpan" in forAllTimeZones {
      3.seconds + 3.seconds === 6.seconds
    }
    "return a new TimeSpan representing the difference of the 2 times when substracted with another TimeSpan" in forAllTimeZones {
      3.seconds - 4.seconds === (-1).seconds
    }
    "have a toString method returning the relevant number of weeks, days, hours, minutes, seconds, millis" in forAllTimeZones {
      val conversionIsOk = forAll(timeAmounts)((t: TimeAmounts) => { val (timeSpanToString, timeSpanAmounts) = t
        timeSpanAmounts forall { case (amount, unit) =>
          amount >= 1  &&
          timeSpanToString.contains(amount.toString) || true }
      })
      val timeSpanStringIsPluralized = forAll(timeAmounts)((t: TimeAmounts) => { val (timeSpanToString, timeSpanAmounts) = t
        timeSpanAmounts forall { case (amount, unit) =>
               amount > 1  && timeSpanToString.contains(unit + "s") ||
               amount == 1 && timeSpanToString.contains(unit) ||
               amount == 0 && !timeSpanToString.contains(unit)
        }
      })
      conversionIsOk && timeSpanStringIsPluralized
    }
  }

  "the TimeHelpers" should {
    "provide a 'seconds' function transforming a number of seconds into millis" in forAllTimeZones {
      seconds(3) === 3 * 1000
    }
    "provide a 'minutes' function transforming a number of minutes into millis" in forAllTimeZones {
      minutes(3) === 3 * 60 * 1000
    }
    "provide a 'hours' function transforming a number of hours into milliss" in forAllTimeZones {
      hours(3) === 3 * 60 * 60 * 1000
    }
    "provide a 'days' function transforming a number of days into millis" in forAllTimeZones {
      days(3) === 3 * 24 * 60 * 60 * 1000
    }
    "provide a 'weeks' function transforming a number of weeks into millis" in forAllTimeZones {
      weeks(3) === 3 * 7 * 24 * 60 * 60 * 1000
    }
    "provide a noTime function on Date objects to transform a date into a date at the same day but at 00:00" in forAllTimeZones {
      hourFormat(now.noTime) === "00:00:00"
    }

    "provide a day function returning the day of month corresponding to a given date (relative to UTC)" in forAllTimeZones {
      day(today.setTimezone(utc).setDay(3).getTime) === 3
    }
    "provide a month function returning the month corresponding to a given date" in forAllTimeZones {
      month(today.setTimezone(utc).setMonth(4).getTime) === 4
    }
    "provide a year function returning the year corresponding to a given date" in forAllTimeZones {
      year(today.setTimezone(utc).setYear(2008).getTime) === 2008
    }
    "provide a millisToDays function returning the number of days since the epoch time" in forAllTimeZones {
      millisToDays(new Date(0).getTime) === 0
      millisToDays(today.setYear(1970).setMonth(0).setDay(1).getTime.getTime) === 0 // the epoch time
      // on the 3rd day after the epoch time, 2 days are passed
      millisToDays(today.setTimezone(utc).setYear(1970).setMonth(0).setDay(3).getTime.getTime) === 2
    }
    "provide a daysSinceEpoch function returning the number of days since the epoch time" in forAllTimeZones {
      daysSinceEpoch === millisToDays(now.getTime)
    }
    "provide a time function creating a new Date object from a number of millis" in forAllTimeZones {
      time(1000) === new Date(1000)
    }
    "provide a calcTime function returning the time taken to evaluate a block in millis and the block's result" in forAllTimeZones {
      val (time, result) = calcTime((1 to 10).reduceLeft[Int](_ + _))
      time.toInt must beCloseTo(0, 1000)  // it should take less than 1 second!
      result === 55
    }

    "provide a hourFormat function to format the time of a date object" in forAllTimeZones {
      hourFormat(Calendar.getInstance(utc).noTime.getTime) === "00:00:00"
    }

    "provide a formattedDateNow function to format todays date" in forAllTimeZones {
      formattedDateNow must beMatching("\\d\\d\\d\\d/\\d\\d/\\d\\d")
    }
    "provide a formattedTimeNow function to format now's time with the TimeZone" in forAllTimeZones {
      val regex = "\\d\\d:\\d\\d (....?.?|GMT((\\+|\\-)\\d\\d:\\d\\d)?)"
      "10:00 CEST" must beMatching(regex)
      "10:00 GMT+02:00" must beMatching(regex)
      "10:00 GMT" must beMatching(regex)
      "10:00 XXX" must beMatching(regex)
      formattedTimeNow must beMatching(regex)
    }

    "provide a parseInternetDate function to parse a string formatted using the internet format" in forAllTimeZones {
      parseInternetDate(internetDateFormatter.format(now)).getTime.toLong must beCloseTo(now.getTime.toLong, 1000L)
    }
    "provide a parseInternetDate function returning new Date(0) if the input date cant be parsed" in forAllTimeZones {
      parseInternetDate("unparsable") === new Date(0)
    }
    "provide a toInternetDate function formatting a date to the internet format" in forAllTimeZones {
      toInternetDate(now) must beMatching("..., \\d* ... \\d\\d\\d\\d \\d\\d:\\d\\d:\\d\\d .*")
    }
    "provide a toDate returning a Full(date) from many kinds of objects" in forAllTimeZones {
      val d = now
      List(null, Nil, None, Failure("", Empty, Empty)) forall { toDate(_) must beEqualTo(Empty) }
      List(Full(d), Some(d), List(d)) forall { toDate(_) must beEqualTo(Full(d)) }

      toDate(internetDateFormatter.format(d)) must beLike {
        case Full(converted) =>
          converted.getTime.toLong must beCloseTo(d.getTime.toLong, 1000L)
      }
    }
  }

  "The Calendar class" should {
    "have a setDay method setting the day of month and returning the updated Calendar" in forAllTimeZones {
      day(today.setTimezone(utc).setDay(1).getTime) === 1
    }
    "have a setMonth method setting the month and returning the updated Calendar" in forAllTimeZones {
      month(today.setTimezone(utc).setMonth(0).getTime) === 0
    }
    "have a setYear method setting the year and returning the updated Calendar" in forAllTimeZones {
      year(today.setTimezone(utc).setYear(2008).getTime) === 2008
    }
    "have a setTimezone method to setting the time zone and returning the updated Calendar" in forAllTimeZones {
      today.setTimezone(utc).getTimeZone === utc
    }
    "have a noTime method to setting the time to 00:00:00 and returning the updated Calendar" in forAllTimeZones {
      hourFormat(today.noTime.getTime) === "00:00:00"
    }
  }
}

object forAllTimeZones extends Around {
  import MatchersImplicits._

  override def around[T: AsResult](f: => T) = synchronized {
    import scala.jdk.CollectionConverters._
    // setDefault is on static context so tests should be sequenced
    // some timezones for java (used in formatters) and for Joda (other computations) has other offset
    val commonJavaAndJodaTimeZones = (TimeZone.getAvailableIDs.toSet & DateTimeZone.getAvailableIDs.asScala.toSet).filter { timeZoneId =>
      TimeZone.getTimeZone(timeZoneId).getOffset(millis) == DateTimeZone.forID(timeZoneId).getOffset(millis)
    }
    val tzBefore = TimeZone.getDefault
    val dtzBefore = DateTimeZone.getDefault
    try {
      forall(commonJavaAndJodaTimeZones) { timeZoneId =>
        TimeZone.setDefault(TimeZone.getTimeZone(timeZoneId))
        DateTimeZone.setDefault(DateTimeZone.forID(timeZoneId))
        f
      }
    } finally {
      TimeZone.setDefault(tzBefore)
      DateTimeZone.setDefault(dtzBefore)
    }
  }
}


trait TimeAmountsGen {

  type TimeAmounts = (String, List[(Int, String)])

  val timeAmounts =
    for {
      w <- choose(0, 2)
      d <- choose(0, 6)
      h <- choose(0, 23)
      m <- choose(0, 59)
      s <- choose(0, 59)
      ml <- choose(0, 999)
    }
    yield (
      TimeSpan(weeks(w) + days(d) + hours(h) + minutes(m) + seconds(s) + ml).toString,
      (w, "week") :: (d, "day") :: (h, "hour") :: (m, "minute") :: (s, "second") :: (ml, "milli") :: Nil
    )
}
