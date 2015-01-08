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
import java.util.concurrent.TimeUnit

import org.joda.time.{DateTime, PeriodType, Period, Duration}

import scala.concurrent.duration.FiniteDuration

/**
 * The TimeSpanHelpers object extends the TimeSpanHelpers. It can be imported to access all of the trait functions.
 *
 * @see net.liftweb.util.TimeSpanHelpers.TimeSpan for deprecation notice
 */
@deprecated("Use scala.concurrent.duration.Duration instead of TimeSpan")
object TimeSpanHelpers extends TimeSpanHelpers with ControlHelpers with ClassHelpers

/**
 * The TimeSpanHelpers trait provide functions to create TimeSpans (an object representing an amount of time), to manage date formats
 * or general utility functions (get the date for today, get year/month/day number,...)
 *
 * @see net.liftweb.util.TimeSpanHelpers.TimeSpan for deprecation notice
 */
@deprecated("Use scala.concurrent.duration.Duration instead of TimeSpan")
trait TimeSpanHelpers extends TimeHelpers { self: ControlHelpers =>

  /** private variable allowing the access to all TimeSpanHelpers functions from inside the TimeSpan class */
  private val outer = this

  /** transforms a long to a TimeSpanBuilder object. Usage: 3L.seconds returns a TimeSpan of 3000L millis  */
  implicit def longToTimeSpanBuilder(in: Long): TimeSpanBuilder = TimeSpanBuilder(in)

  /** transforms an int to a TimeSpanBuilder object. Usage: 3.seconds returns a TimeSpan of 3000L millis  */
  implicit def intToTimeSpanBuilder(in: Int): TimeSpanBuilder = TimeSpanBuilder(in)

  /** transforms a long to a TimeSpan object. Usage: 3000L returns a TimeSpan of 3000L millis  */
  implicit def longToTimeSpan(in: Long): TimeSpan = TimeSpan(in)

  /** transforms an int to a TimeSpan object. Usage: 3000 returns a TimeSpan of 3000L millis  */
  implicit def intToTimeSpan(in: Int): TimeSpan = TimeSpan(in)

  private implicit def durToPeriod(dur: Duration): Period = dur.toPeriod(PeriodType.standard())

  /** class building TimeSpans given an amount (len) and a method specify the time unit  */
  case class TimeSpanBuilder(val len: Long) {
    def seconds = new TimeSpan(Right((new Period).plusSeconds(len.toInt)))
    def second = seconds
    def minutes = new TimeSpan(Right((new Period).plusMinutes(len.toInt)))
    def minute = minutes
    def hours = new TimeSpan(Right(Duration.standardHours(len): Period))
    def hour = hours
    def days = new TimeSpan(Right(Duration.standardDays(len): Period))
    def day = days
    def weeks = new TimeSpan(Right(Duration.standardDays(len * 7L): Period))
    def week = weeks
    def months = new TimeSpan(Right((new Period().plusMonths(len.toInt))))
    def month = months
    def years = new TimeSpan(Right((new Period().plusYears(len.toInt))))
    def year = years
  }

  /**
   * The TimeSpan class represents an amount of time.
   * It can be translated to a date with the date method. In that case, the number of millis seconds will be used to create a Date
   * object starting from the Epoch time (see the documentation for java.util.Date)
   *
   * @deprecated In most cases in concurrent programming the intend of developer is to use duration represented as milliseconds.
   *            However TimeSpan hold period representation as a changes in each date field. It could cause ambiguous situation
   *            because it could be used in plus/minus/ago/later/equals operation context where it take care about time switch etc.
   *            or millis could be taken. So please use scala.concurrent.duration.Duration instead of this class.
   */
  @deprecated("Use scala.concurrent.duration.Duration instead of TimeSpan")
  class TimeSpan(private val dt: Either[DateTime, Period]) extends ConvertableToDate {
    /** @return a Date as the amount of time represented by the TimeSpan after the Epoch date */

    def this(ms: Long) =
      this(if (ms < 52L * 7L * 24L * 60L * 60L * 1000L) Right(new Period(ms))
      else Left(new DateTime(ms)))

    def date: Date = dt match {
      case Left(datetime) => new Date(datetime.getMillis())
      case _ => new Date(millis)
    }

    /**
     * Convert to a Date
     */
    def toDate: Date = date

    /**
     * Convert to a JodaTime DateTime
     */
    def toDateTime = dt match {
      case Left(datetime) => datetime
      case _ => new DateTime(millis)
    }

    def toMillis = millis

    def millis = dt match {
      case Left(datetime) => datetime.getMillis()
      case Right(duration) => duration.toStandardDuration.getMillis()
    }


    /** @return a Date as the amount of time represented by the TimeSpan after now */
    def later: TimeSpan = dt match {
      case Right(duration) => new TimeSpan(Left(new DateTime(outer.millis).plus(duration)))
      case _ => TimeSpan(millis + outer.millis)
    }

    /** @return a Date as the amount of time represented by the TimeSpan before now */
    def ago: TimeSpan = dt match {
      case Right(duration) => new TimeSpan(Left(new DateTime(outer.millis).minus(duration)))
      case _ => TimeSpan(outer.millis - millis)
    }

    def noTime: Date = new DateExtension(this).noTime

    /** @return a TimeSpan representing the addition of 2 TimeSpans */
    def +[B](in: B)(implicit f: B => TimeSpan): TimeSpan =
      (this.dt, f(in).dt) match {
        case (Right(p1), Right(p2)) => p1.plus(p2)
        case (Left(date), Right(duration)) => date.plus(duration)
        case (Right(duration), Left(date)) => date.plus(duration)
        case _ => TimeSpan(this.millis + f(in).millis)
      }

    /** @return a TimeSpan representing the addition of 2 TimeSpans */
    def plus[B](in: B)(implicit f: B => TimeSpan): TimeSpan = this.+(in)(f)

    /** @return a TimeSpan representing the substraction of 2 TimeSpans */
    def -[B](in: B)(implicit f: B => TimeSpan): TimeSpan =
      (this.dt, f(in).dt) match {
        case (Right(p1), Right(p2)) => p1.minus(p2)
        case (Left(date), Right(duration)) => date.minus(duration)
        case (Right(duration), Left(date)) => date.minus(duration)
        case _ => TimeSpan(this.millis - f(in).millis)
      }

    /** override the equals method so that TimeSpans can be compared to long, int and TimeSpan */
    override def equals(cmp: Any) = {
      cmp match {
        case lo: Long => lo == this.millis
        case i: Int => i == this.millis
        case ti: TimeSpan => ti.dt == this.dt
        case d: Date => d.getTime() == this.millis
        case dt: DateTime => Left(dt) == this.dt
        case dur: Duration => Right(dur: Period) == this.dt
        case dur: Period => Right(dur) == this.dt
        case _ => false
      }
    }

    /** override the toString method to display a readable amount of time */
    override def toString = dt match {
      case Left(date) => date.toString
      case Right(dur) => TimeSpan.format(millis)
    }

    def toScalaDuration: concurrent.duration.Duration = {
      FiniteDuration(millis, TimeUnit.MILLISECONDS)
    }
  }

  /**
   * The TimeSpan object provides class represents an amount of time.
   * It can be translated to a date with the date method. In that case, the number of millis seconds will be used to create a Date
   * object starting from the Epoch time (see the documentation for java.util.Date)
   */
  object TimeSpan {
    /** time units and values used when converting a total number of millis to those units (see the format function)  */
    val scales = List((1000L, "milli"), (60L, "second"), (60L, "minute"), (24L, "hour"), (7L, "day"), (10000L, "week"))

    /** explicit constructor for a TimeSpan  */
    def apply(in: Long) = new TimeSpan(in)

    /**
     * Formats a number of millis to a string representing the number of weeks, days, hours, minutes, seconds, millis
     */
    def format(millis: Long): String = {
      def divideInUnits(millis: Long) = scales.foldLeft[(Long, List[(Long, String)])]((millis, Nil)){ (total, div) =>
        (total._1 / div._1, (total._1 % div._1, div._2) :: total._2)
      }._2
      def formatAmount(amountUnit: (Long, String)) = amountUnit match {
        case (amount, unit) if (amount == 1) => amount + " " + unit
        case (amount, unit) => amount + " " + unit + "s"
      }
      divideInUnits(millis).filter(_._1 > 0).map(formatAmount(_)).mkString(", ")
    }

    /**
     * Convert a Date to a TimeSpan
     */
    implicit def dateToTS(in: Date): TimeSpan =
      new TimeSpan(Left(new DateTime(in.getTime)))

    /**
     * Convert a DateTime to a TimeSpan
     */
    implicit def dateTimeToTS(in: DateTime): TimeSpan =
      new TimeSpan(Left(in))

    /**
     * Convert a Duration to a TimeSpan
     */
    implicit def durationToTS(in: Duration): TimeSpan =
      new TimeSpan(Right(in: Period))

    /**
     * Convert a Period to a TimeSpan
     */
    implicit def periodToTS(in: Period): TimeSpan =
      new TimeSpan(Right(in))
  }

}

trait ConvertableToDate {
  def toDate: Date
  def toDateTime: DateTime
  def millis: Long
}

object ConvertableToDate {
  implicit def toDate(in: ConvertableToDate): Date = in.toDate
  implicit def toDateTime(in: ConvertableToDate): DateTime = in.toDateTime
  implicit def toMillis(in: ConvertableToDate): Long = in.millis

}