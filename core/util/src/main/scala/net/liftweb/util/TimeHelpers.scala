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

import java.text.SimpleDateFormat
import java.util.{TimeZone, Calendar, Date, Locale}

import scala.language.implicitConversions

import org.joda.time._

import common._

/**
 * The TimeHelpers object extends the TimeHelpers. It can be imported to access all of the trait functions.
 */
object TimeHelpers extends TimeHelpers with ControlHelpers with ClassHelpers

/**
 * The TimeHelpers trait provide functions to create TimeSpans (an object representing duration in milliseconds),
 * to manage date formats or general utility functions (get the date for today, get year/month/day number,...)
 */
trait TimeHelpers { self: ControlHelpers =>
  // Logger must be lazy, since we cannot instantiate until after boot is complete
  private  lazy val logger = Logger(classOf[TimeHelpers])

  /** transforms a long to a TimeSpanBuilder object. Usage: 3L.seconds returns a TimeSpan of 3000L millis  */
  implicit def longToTimeSpanBuilder(in: Long): TimeSpanBuilder = TimeSpanBuilder(in)

  /** transforms an int to a TimeSpanBuilder object. Usage: 3.seconds returns a TimeSpan of 3000L millis  */
  implicit def intToTimeSpanBuilder(in: Int): TimeSpanBuilder = TimeSpanBuilder(in)

  /** transforms a long to a TimeSpan object. Usage: 3000L returns a TimeSpan of 3000L millis  */
  @deprecated("Long to TimeSpan conversion will be removed for possibility of ambiguous behaviours, use TimeSpan(in) instead if you are using in.millis", "3.0.0")
  implicit def longToTimeSpan(in: Long): TimeSpan = TimeSpan(in)

  /** transforms an int to a TimeSpan object. Usage: 3000 returns a TimeSpan of 3000L millis  */
  @deprecated("Int to TimeSpan conversion will be removed for possibility of ambiguous behaviours, use TimeSpan(in) instead if you are using in.millis", "3.0.0")
  implicit def intToTimeSpan(in: Int): TimeSpan = TimeSpan(in)

  /** class building TimeSpans given an amount (len) and a method specify the time unit  */
  case class TimeSpanBuilder(len: Long) {
    def seconds = new TimeSpan(Left(Duration.standardSeconds(len)))
    def second = seconds
    def minutes = new TimeSpan(Left(Duration.standardMinutes(len)))
    def minute = minutes
    def hours = new TimeSpan(Left(Duration.standardHours(len)))
    def hour = hours
    def days = new TimeSpan(Left(Duration.standardDays(len)))
    def day = days
    def weeks = new TimeSpan(Left(Duration.standardDays(len * 7L)))
    def week = weeks
    @deprecated("This builder will be removed due to its unclear behavior; use Joda-Time `Period.months` and convert to `TimeSpan` manually instead.", "3.0.0")
    def months = new TimeSpan(Right(new Period().plusMonths(len.toInt)))
    @deprecated("This builder will be removed due to its unclear behavior; use Joda-Time `Period.months` and convert to `TimeSpan` manually instead.", "3.0.0")
    def month = months
    @deprecated("This builder will be removed due to its unclear behavior; use Joda-Time `Period.years` and convert to `TimeSpan` manually instead.", "3.0.0")
    def years = new TimeSpan(Right(new Period().plusYears(len.toInt)))
    @deprecated("This builder will be removed due to its unclear behavior; use Joda-Time `Period.years` and convert to `TimeSpan` manually instead.", "3.0.0")
    def year = years
  }

  /**
   * The `TimeSpan` class represents a duration of time in milliseconds. In this
   * way, it is similar to the `[[scala.concurrent.Duration]]` class. It is
   * mostly used in Lift APIs in similar positions as the Scala `Duration`
   * class (for example, in event scheduling).
   *
   * Unlike in the Lift 2.x series, building a `TimeSpan` with a `Long` will not
   * have different behavior depending on the value passed. Any passed `Long`
   * will be used as a duration.
   *
   * Prior to Lift 3.0, `TimeSpan` was an amalgam of duration and joda
   * `DateTime`, and allowed conversions between the two. As a result,
   * operational semantics were poorly defined and it was easy to call a method
   * that seemed like it should have simple duration semantics but run into
   * `DateTime` semantics that made things more complicated instead.
   *
   * Lift 3.0 mostly maintains API compatibility with the Lift 2.x series, but
   * introduces a series of deprecations to indicate places where dangerous
   * and potentially unclear behavior may occur. Lift 3.1 will maintain API
   * compatibility with all non-deprecated parts of the `TimeSpan` API, but will
   * remove the deprecated aspects.
   *
   * For deprecated years and month builders it handle an operations on duration
   * field values. Then it could be used only in to-period implicit conversion.
   */
  class TimeSpan(private val dt: Either[Duration, Period]) extends ConvertableToDate {

    def this(ms: Long) =
      this(Left(new Duration(ms)))

    /**
     * Convert to a Java `Date`. The number of milliseconds in the `Duration`
     * will be added to the UNIX epoch to create a `Date` object.
     */
    @deprecated("This method will be removed due to its unclear behavior; use new Date(timeSpan.millis) instead.", "3.0.0")
    def date: Date = new Date(millis)

    /**
     * Convert to a Java `Date`. Synonym of `[[date]]`.
     */
    @deprecated("This method will be removed due to its unclear behavior; use new Date(timeSpan.millis) instead.", "3.0.0")
    def toDate: Date = date

    /**
     * Convert to a Joda-Time `DateTime`. The number of milliseconds in the `Duration`
     * will be added to the UNIX epoch to create a `DateTime` object.
     */
    @deprecated("This method will be removed due to its unclear behavior; use new DateTime(timeSpan.millis) instead.", "3.0.0")
    def toDateTime = new DateTime(millis)

    @deprecated("TimeSpan will not support operations on Joda-Time `Period`s in the future; use new Period(timeSpan.millis) instead.", "3.0.0")
    private[util] def toPeriod: Period = dt match { // package protected because of view bound usage in tsToPeriod
      case Left(duration) => duration.toPeriod
      case Right(period) => period
    }

    /**
     * @return The amount of milliseconds this `TimeSpan` represents.
     * @throws UnsupportedOperationException When created by the deprecated
     *         months/years builder (month and year lengths in milliseconds
     *         are only defined with respect to a reference point, since the
     *         length of a month or year can vary).
     */
    def toMillis = millis

    /**
     * @return The amount of milliseconds this `TimeSpan` represents.
     * @throws UnsupportedOperationException When created by the deprecated months/years builder (
     *     month and year lengths in milliseconds are only defined with respect to a reference point,
     *     since the length of a month or year can vary).
     */
    def millis = dt match {
      case Left(duration) => duration.getMillis
      case Right(period) => period.toStandardDuration.getMillis // will throw exception because it holds month or year
    }

    // TODO If we choose to move away from TimeSpan, we'll need to take into
    // TODO account the fact that this method can take anything convertible to
    // TODO a TimeSpan, so we'll need at least one release where TimeSpan is
    // TODO around for the purposes of these implicit conversions in case client
    // TODO code defines one.
    /**
     * Sums this `TimeSpan` with an object that can be converted to a
     * `TimeSpan`. If either `TimeSpan` represents a `Duration`, add the
     * `Duration`s directly. If both `TimeSpan`s represents a `Period` (which is
     * deprecated behavior), adds them using `Period` addition.
     *
     * @note Adding two `TimeSpan`s where one of the two was constructed using the
     *       deprecated `months` or `years` builders will throw an exception.
     * @note Adding two `TimeSpan`s where both were constructed using the
     *       deprecated `months` or `years` builders will result in a `TimeSpan`
     *       representing a `Period`. These `TimeSpan`s can behave in unexpected
     *       ways, including throwing exceptions when their millisecond duration
     *       is required.
     *
     * @return A `TimeSpan` representing the sum of this span and `in`'s
     *         `TimeSpan` representation.
     * @throws UnsupportedOperationException If only one of the two `TimeSpan`s
     *         represents a `Period` and that `Period` has a year or month
     *          component (this only occurs if the deprecated `months` or
     *          `years` builders are used, as month and year lengths in
     *          milliseconds are only defined with respect to a reference point,
     *          since the length of a month or year can vary)
     */
    def +[B](in: B)(implicit f: B => TimeSpan): TimeSpan =
      (this.dt, f(in).dt) match {
        case (Right(p1), Right(p2)) => new TimeSpan(Right(p1.plus(p2)))
        case (Left(duration), Right(period)) => new TimeSpan(Left(duration.plus(period.toStandardDuration)))
        case (Right(period), Left(duration)) => new TimeSpan(Left(period.toStandardDuration.plus(duration)))
        case (Left(d1), Left(d2)) => new TimeSpan(Left(d1.plus(d2)))
      }

    /**
     * Alias for `[[+]]`.
     */
    def plus[B](in: B)(implicit f: B => TimeSpan): TimeSpan = this.+(in)(f)

    // TODO If we choose to move away from TimeSpan, we'll need to take into
    // TODO account the fact that this method can take anything convertible to
    // TODO a TimeSpan, so we'll need at least one release where TimeSpan is
    // TODO around for the purposes of these implicit conversions in case client
    // TODO code defines one.
    /**
     * Subtracts an object that can be converted to a `TimeSpan` from this
     * `TimeSpan`. If either `TimeSpan` represents a `Duration`, subtracts the
     * `Duration`s directly. If both `TimeSpan`s represents a `Period` (which is
     * deprecated behavior), subtracts them using `Period` subtraction.
     *
     * @note Subtracting two `TimeSpan`s where one of the two was constructed
     *       using the deprecated `months` or `years` builders will throw an
     *       exception.
     * @note Subtracting two `TimeSpan`s where both were constructed using the
     *       deprecated `months` or `years` builders will result in a `TimeSpan`
     *       representing a `Period`. These `TimeSpan`s can behave in unexpected
     *       ways, including throwing exceptions when their millisecond duration
     *       is required.
     *
     * @return A `TimeSpan` representing the sum of this span and `in`'s
     *         `TimeSpan` representation.
     * @throws UnsupportedOperationException If only one of the two `TimeSpan`s
     *         represents a `Period` and that `Period` has a year or month
     *          component (this only occurs if the deprecated `months` or
     *          `years` builders are used, as month and year lengths in
     *          milliseconds are only defined with respect to a reference point,
     *          since the length of a month or year can vary)
     */
    def -[B](in: B)(implicit f: B => TimeSpan): TimeSpan =
      (this.dt, f(in).dt) match {
        case (Right(p1), Right(p2)) => new TimeSpan(Right(p1.minus(p2)))
        case (Left(duration), Right(period)) => new TimeSpan(Left(duration.minus(period.toStandardDuration)))
        case (Right(period), Left(duration)) => new TimeSpan(Left(period.toStandardDuration.minus(duration)))
        case (Left(d1), Left(d2)) => new TimeSpan(Left(d1.minus(d2)))
      }

    /**
     * Override the equals method so that `TimeSpan`s can be compared to long, int,
     * Joda-Time `Duration`, and `TimeSpan`.
     *
     * @note Comparing to a Joda-Time `Period` is also done correctly, but is
     *       deprecated.
     */
    override def equals(cmp: Any) = {
      cmp match {
        case lo: Long => lo == this.millis
        case i: Int => i == this.millis
        case ti: TimeSpan => ti.dt == this.dt
        case dur: Duration => Left(dur) == this.dt
        case period: Period => Right(period) == this.dt
        case _ => false
      }
    }

    /**
     * Override the toString method to display a readable amount of time using
     * `[[TimeSpan.format]]``
     */
    override def toString = TimeSpan.format(millis)
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
        case (amount, unit) if (amount == 1) => s"${amount} ${unit}"
        case (amount, unit) => s"${amount} ${unit}s"
      }
      divideInUnits(millis).filter(_._1 > 0).map(formatAmount(_)).mkString(", ")
    }

    /**
     * Convert a Duration to a TimeSpan
     */
    implicit def durationToTS(in: Duration): TimeSpan =
      new TimeSpan(Left(in))
  }

  /** @return the current System.nanoTime() */
  def nano = System.nanoTime()

  /** @return the current number of millis: System.currentTimeMillis  */
  def millis = System.currentTimeMillis

  /** @return the number of millis corresponding to 'in' seconds */
  def seconds(in: Long): Long = in * 1000L

  /** @return the number of millis corresponding to 'in' minutes */
  def minutes(in: Long): Long = seconds(in) * 60L

  /** @return the number of millis corresponding to 'in' hours */
  def hours(in: Long): Long = minutes(in) * 60L

  /** @return the number of millis corresponding to 'in' days */
  def days(in: Long): Long = hours(in) * 24L

  /** @return the number of millis corresponding to 'in' weeks */
  def weeks(in: Long): Long = days(in) * 7L

  /** implicit def used to add the noTime method to the Date class */
  implicit def toDateExtension(d: Date): DateExtension = new DateExtension(d)

  /** This class adds a noTime method the Date class, in order to get at Date object starting at 00:00 */
  class DateExtension(date: Date) {
    /** @return a Date object starting at 00:00 from date */
    def noTime = {
      val calendar = Calendar.getInstance
      calendar.setTime(date)
      calendar.set(Calendar.HOUR_OF_DAY, 0)
      calendar.set(Calendar.MINUTE, 0)
      calendar.set(Calendar.SECOND, 0)
      calendar.set(Calendar.MILLISECOND, 0)
      calendar.getTime
    }
  }


  implicit class DateTimeExtension(dateTime: DateTime) {
    def noTime = dateTime.withTimeAtStartOfDay()
  }

  /** implicit def used to add the setXXX methods to the Calendar class */
  implicit def toCalendarExtension(c: Calendar): CalendarExtension = new CalendarExtension(c)

  /** This class adds the setXXX methods to the Calendar class. Each setter returns the updated Calendar */
  class CalendarExtension(c: Calendar) {
    /** set the day of the month (1 based) and return the calendar */
    def setDay(d: Int) = { c.set(Calendar.DAY_OF_MONTH, d); c }

    /** set the month (0 based) and return the calendar */
    def setMonth(m: Int) = { c.set(Calendar.MONTH, m); c }

    /** set the year and return the calendar */
    def setYear(y: Int) = { c.set(Calendar.YEAR, y); c }

    /** set the TimeZone and return the calendar */
    def setTimezone(tz: TimeZone) = { c.setTimeZone(tz); c }

    /** set the time to 00:00:00.000 and return the calendar */
    def noTime = { c.setTime(c.getTime.noTime); c }
  }

  /** @return the date object for now */
  def now  = new Date

  /** @return the Calendar object for today (the TimeZone is the local TimeZone). Its time is 00:00:00.000 */
  def today  = Calendar.getInstance.noTime

  /** @return the current year */
  def currentYear: Int = Calendar.getInstance.get(Calendar.YEAR)

  /** alias for new Date(millis) */
  def time(when: Long) = new Date(when)

  /** @return the month corresponding to today (0 based, relative to UTC) */
  def month(in: Date): Int = {
    val cal = Calendar.getInstance(utc)
    cal.setTimeInMillis(in.getTime)
    cal.get(Calendar.MONTH)
  }

  /** @return the year corresponding to today (relative to UTC) */
  def year(in: Date): Int =  {
    val cal = Calendar.getInstance(utc)
    cal.setTimeInMillis(in.getTime)
    cal.get(Calendar.YEAR)
  }

  /** @return the day of month corresponding to the input date (1 based) */
  def day(in: Date): Int =  {
    val cal = Calendar.getInstance(utc)
    cal.setTimeInMillis(in.getTime)
    cal.get(Calendar.DAY_OF_MONTH)
  }

  /** The UTC TimeZone */
  val utc = TimeZone.getTimeZone("UTC")

  /** @return the number of days since epoch converted from millis */
  def millisToDays(millis: Long): Long = millis / (1000L * 60L * 60L * 24L)

  /** @return the number of days since epoch */
  def daysSinceEpoch: Long = millisToDays(millis)

  /** @return the time taken to evaluate f in millis and the result */
  def calcTime[T](f: => T): (Long, T) = {
    val start = millis
    val result = f
    (millis - start, result)
  }

  /**
   * Log a message with the time taken in millis to do something and return the result
   * @return the result
   */
  def logTime[T](msg: String)(f: => T): T = {
    val (time, ret) = calcTime(f)
    logger.info(msg + " took " + time + " Milliseconds")
    ret
  }

 /**
   * Call f and log the string returned together with the time taken in millis.
   * @return the second result from f
   */
  def logTime[T](f: => (String,T)): T = {
    val (time, fret) = calcTime(f)
    val (msg, ret) = fret
    logger.info(msg + " took " + time + " Milliseconds")
    ret
  }

  /**
   * @return a standard format HH:mm:ss
   */
  def hourFormat = new SimpleDateFormat("HH:mm:ss")

  /**
   * @return the formatted time for a given Date
   */
  def hourFormat(in: Date): String = hourFormat.format(in)

  /** @return a standard format for the date yyyy/MM/dd */
  def dateFormatter = new SimpleDateFormat("yyyy/MM/dd")

  /** @return a format for the time which includes the TimeZone: HH:mm zzz*/
  def timeFormatter = new SimpleDateFormat("HH:mm zzz")

  /** @return today's date formatted as yyyy/MM/dd */
  def formattedDateNow = dateFormatter.format(now)

  /** @return now's time formatted as HH:mm zzz */
  def formattedTimeNow = timeFormatter.format(now)

  /** @return a formatter for internet dates (RFC822/1123) including:
   *  the day of week, the month, day of month, time and time zone */
  def internetDateFormatter = {
    val ret = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss 'GMT'", Locale.US)
    ret.setTimeZone(utc)
    ret
  }

  /** @return a Box[date] from a string using the internet format. */
  def boxParseInternetDate(dateString: String): Box[Date] = tryo {
    internetDateFormatter.parse(dateString)
  }

  /** @return a date from a string using the internet format. Return the Epoch date if the parse is unsuccesful */
  def parseInternetDate(dateString: String): Date = tryo {
    internetDateFormatter.parse(dateString)
  } openOr new Date(0L)

  /** @return a date formatted with the internet format */
  def toInternetDate(in: Date): String = internetDateFormatter.format(in)

  /** @return a date formatted with the internet format (from a number of millis) */
  def toInternetDate(in: Long): String = internetDateFormatter.format(new Date(in))

  /** @return the current time as an internet date */
  def nowAsInternetDate: String = toInternetDate(millis)

  /** @return a Full(date) or a failure if the input couldn't be translated to date (or Empty if the input is null)*/
  def toDate(in: Any): Box[Date] = {
    try {
      in match {
        case null => Empty
        case d: Date => Full(d)
        case lng: Long => Full(new Date(lng))
        case lng: Number => Full(new Date(lng.longValue))
        case Nil | Empty | None | Failure(_, _, _) => Empty
        case Full(v) => toDate(v)
        case Some(v) => toDate(v)
        case v :: vs => toDate(v)
        case s : String => tryo(internetDateFormatter.parse(s)) or tryo(dateFormatter.parse(s))
        case o => toDate(o.toString)
      }
    } catch {
      case e: Exception => logger.debug("Error parsing date "+in, e); Failure("Bad date: "+in, Full(e), Empty)
    }
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
