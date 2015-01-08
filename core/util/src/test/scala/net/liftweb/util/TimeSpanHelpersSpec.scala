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
package net.liftweb.util

import java.util.Date

import net.liftweb.util.TimeSpanHelpers._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.specs2.time.NoTimeConversions



/**
 * Systems under specification for TimeHelpers.
 */
class TimeSpanHelpersSpec extends Specification with ScalaCheck with TimeAmountsGen with NoTimeConversions {
  "TimeSpanHelpers Specification".title

  "A TimeSpan" can {
    "be converted implicitly to a date starting from the epoch time" in {
      3.seconds.after(new Date(0)) must beTrue
    }
    "be converted to a date starting from the epoch time, using the date method" in {
      3.seconds.after(new Date(0)) must beTrue
    }
    "be implicitly converted to a Long" in {
      (3.seconds == 3000L) must_== true
    }
    "be compared to an int" in {
      (3.seconds == 3000) must_== true
      (3.seconds != 2000) must_== true
    }
    "be compared to a long" in {
      (3.seconds == 3000L) must_== true
      (3.seconds != 2000L) must_== true
    }
    "be compared to another TimeSpan" in {
      3.seconds must_== 3.seconds
      3.seconds must_!= 2.seconds
    }
    "be compared to another object" in {
      3.seconds must_!= "string"
    }
  }

  "A TimeSpan" should {
    "return a new TimeSpan representing the sum of the 2 times when added with another TimeSpan" in {
      3.seconds + 3.seconds must_== 6.seconds
    }
    "return a new TimeSpan representing the difference of the 2 times when substracted with another TimeSpan" in {
      3.seconds - 4.seconds must_== (-1).seconds
    }
    "have a later method returning a date relative to now plus the time span" in {
      val expectedTime = new Date().getTime + 3.seconds.millis

      3.seconds.later.getTime must beCloseTo(expectedTime, 1000L)
    }
    "have an ago method returning a date relative to now minus the time span" in {
      val expectedTime = new Date().getTime - 3.seconds.millis

      3.seconds.ago.getTime must beCloseTo(expectedTime, 1000L)
    }
    "have a toString method returning the relevant number of weeks, days, hours, minutes, seconds, millis" in {
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
      check(conversionIsOk && timeSpanStringIsPluralized)
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