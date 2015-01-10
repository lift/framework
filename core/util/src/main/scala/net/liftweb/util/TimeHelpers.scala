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

import common._

/**
 * The TimeHelpers object extends the TimeHelpers. It can be imported to access all of the trait functions.
 */
object TimeHelpers extends TimeHelpers with ControlHelpers with ClassHelpers

/**
 * The TimeHelpers trait provide functions to manage date formats or general utility functions
 * (get the date for today, get year/month/day number,...)
 */
trait TimeHelpers { self: ControlHelpers =>
  // Logger must be lazy, since we cannot instantiate until after boot is complete
  private  lazy val logger = Logger(classOf[TimeHelpers])

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

