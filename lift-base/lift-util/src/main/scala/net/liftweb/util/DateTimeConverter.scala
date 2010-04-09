/*
 * Copyright 2007-2009 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */


package net.liftweb {
package util {


import Helpers.{tryo, internetDateFormatter=>internetDateFormat, dateFormatter=>dateFormat, hourFormat, timeFormatter=>timeFormat}
import net.liftweb.common._
import java.util.Date

/**
 * Implement this trait to specify a set of rules to parse and format dates
 * @author nafg 
*/
trait DateTimeConverter {  
  /**
   * A function to format a Date as a date and time
   */
  def formatDateTime(d: Date): String
  
  /**
   * A function to format a Date as a date only
  */
  def formatDate(d: Date): String
  
  /**
   * A function to format a Date as a time.
  */
  def formatTime(d: Date): String

  /**
   * A function that parses a String representing a date and time into a Date
   */
   def parseDateTime(s: String): Box[Date]
   
  /**
   * A function that parses a String representing a date into a Date.
   */
   def parseDate(s: String): Box[Date]
  
  /**
   * A function that parses a String representing a time into a Date.
   */
   def parseTime(s: String): Box[Date]
}

/**
 * A default implementation of DateTimeConverter that uses (Time)Helpers
*/
object DefaultDateTimeConverter extends DateTimeConverter {
  def formatDateTime(d: Date) = internetDateFormat.format(d)
  def formatDate(d: Date) = dateFormat.format(d)
  /**  Uses Helpers.hourFormat which includes seconds but not time zone */
  def formatTime(d: Date) = hourFormat.format(d)
  
  def parseDateTime(s: String) = tryo { internetDateFormat.parse(s) }
  def parseDate(s: String) = tryo { dateFormat.parse(s) }
  /** Tries Helpers.hourFormat and Helpers.timeFormat */
  def parseTime(s: String) = tryo{hourFormat.parse(s)} or tryo{timeFormat.parse(s)}
}


}
}
