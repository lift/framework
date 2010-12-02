/*
 * Copyright 2006-2010 WorldWide Conferencing, LLC
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
package util {

import _root_.java.text.SimpleDateFormat
import _root_.java.util.{TimeZone, Calendar, Date, Locale}
import common._
import _root_.org.joda.time.{DateTime, Duration}

/**
 * The TimeHelpers object extends the TimeHelpers. It can be imported to access all of the trait functions.
 */
object TimeHelpers extends TimeHelpers with ControlHelpers with ClassHelpers

/**
 * The TimeHelpers trait provide functions to create TimeSpans (an object representing an amount of time), to manage date formats
 * or general utility functions (get the date for today, get year/month/day number,...)
 */
trait TimeHelpers { self: ControlHelpers =>
  // Logger must be lazy, since we cannot instantiate until after boot is complete
  private  lazy val logger = Logger(classOf[TimeHelpers])

  /** private variable allowing the access to all TimeHelpers functions from inside the TimeSpan class */
  private val outer = this

  /** transforms a long to a TimeSpanBuilder object. Usage: 3L.seconds returns a TimeSpan of 3000L millis  */
  implicit def longToTimeSpanBuilder(in: Long): TimeSpanBuilder = TimeSpanBuilder(in)

  /** transforms an int to a TimeSpanBuilder object. Usage: 3.seconds returns a TimeSpan of 3000L millis  */
  implicit def intToTimeSpanBuilder(in: Int): TimeSpanBuilder = TimeSpanBuilder(in)

  /** transforms a long to a TimeSpan object. Usage: 3000L returns a TimeSpan of 3000L millis  */
  implicit def longToTimeSpan(in: Long): TimeSpan = TimeSpan(in)

  /** transforms an int to a TimeSpan object. Usage: 3000 returns a TimeSpan of 3000L millis  */
  implicit def intToTimeSpan(in: Int): TimeSpan = TimeSpan(in)

  /** class building TimeSpans given an amount (len) and a method specify the time unit  */
  case class TimeSpanBuilder(val len: Long) {
    def seconds = new TimeSpan(Right(Duration.standardSeconds(len)))
    def second = seconds
    def minutes = new TimeSpan(Right(Duration.standardMinutes(len)))
    def minute = minutes
    def hours = new TimeSpan(Right(Duration.standardHours(len)))
    def hour = hours
    def days = new TimeSpan(Right(Duration.standardDays(len)))
    def day = days
    def weeks = new TimeSpan(Right(Duration.standardDays(len * 7L)))
    def week = weeks
  }

  /*
  /**
   * transforms a TimeSpan to a date by converting the TimeSpan expressed as millis and creating
   * a Date lasting that number of millis from the Epoch time (see the documentation for java.util.Date)
   */
  implicit def timeSpanToDate(in: TimeSpan): Date = in.date

  /** transforms a TimeSpan to its long value as millis */
  implicit def timeSpanToLong(in: TimeSpan): Long = in.millis
  */

  /**
   * The TimeSpan class represents an amount of time.
   * It can be translated to a date with the date method. In that case, the number of millis seconds will be used to create a Date
   * object starting from the Epoch time (see the documentation for java.util.Date)
   */
  class TimeSpan(private val dt: Either[DateTime, Duration]) extends ConvertableToDate {
    /** @return a Date as the amount of time represented by the TimeSpan after the Epoch date */

    def this(ms: Long) = 
      this(if (ms < 52L * 7L * 24L * 60L * 60L * 1000L) Right(new Duration(ms))
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
      case Right(duration) => duration.getMillis()
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
        case (Left(date), Right(duration)) => date.plus(duration)
        case (Right(duration), Left(date)) => date.plus(duration)
        case _ => TimeSpan(this.millis + f(in).millis)
      }

    /** @return a TimeSpan representing the addition of 2 TimeSpans */
    def plus[B](in: B)(implicit f: B => TimeSpan): TimeSpan = this.+(in)(f)

    /** @return a TimeSpan representing the substraction of 2 TimeSpans */
    def -[B](in: B)(implicit f: B => TimeSpan): TimeSpan =
      (this.dt, f(in).dt) match {
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
        case dur: Duration => Right(dur) == this.dt
        case _ => false
      }
    }

    /** override the toString method to display a readable amount of time */
    override def toString = dt match {
      case Left(date) => date.toString
      case Right(dur) => TimeSpan.format(dur.getMillis())
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
      new TimeSpan(Right(in))
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
  implicit def toDateExtension(d: Date) = new DateExtension(d)

  /** This class adds a noTime method the Date class, in order to get at Date object starting at 00:00 */
  class DateExtension(date: Date) {
    /** @returns a Date object starting at 00:00 from date */
    def noTime = {
      val calendar = Calendar.getInstance
      calendar.set(Calendar.HOUR_OF_DAY, 0)
      calendar.set(Calendar.MINUTE, 0)
      calendar.set(Calendar.SECOND, 0)
      calendar.set(Calendar.MILLISECOND, 0)
      calendar.getTime
    }
  }

  /** implicit def used to add the setXXX methods to the Calendar class */
  implicit def toCalendarExtension(c: Calendar) = new CalendarExtension(c)

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

  /**
   * @deprecated use now instead
   * @return the current time as a Date object
   */
  def timeNow = new Date

  /**
   * @deprecated use today instead
   * @return the current Day as a Date object
   */
  def dayNow: Date = 0.seconds.later.noTime

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
   * Log a message with the time taken in millis to do something and retrun the result
   * @return the result
   */
  def logTime[T](msg: String)(f: => T): T = {
    val (time, ret) = calcTime(f)
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

  /** @return a formatter for internet dates including: the day of week, the month, day of month, time and time zone */
  def internetDateFormatter = {
    val ret = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss z", Locale.US)
    ret.setTimeZone(utc)
    ret
  }

  /** @return a date from a string using the internet format. Return the Epoch date if the parse is unsuccesfull */
  def boxParseInternetDate(dateString: String): Box[Date] = tryo {
    internetDateFormatter.parse(dateString)
  }

  /** @return a date from a string using the internet format. Return the Epoch date if the parse is unsuccesfull */
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
      case e => logger.debug("Error parsing date "+in, e); Failure("Bad date: "+in, Full(e), Empty)
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

}
}
