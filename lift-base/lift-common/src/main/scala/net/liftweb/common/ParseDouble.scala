/*
 * Copyright 2011 WorldWide Conferencing, LLC
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
package common

/**
 * Safely parse a String into a Double, avoiding the JVM bug
 * that causes the thread to hang if the String is
 * 2.2250738585072012e-308
 */
object ParseDouble {
  /**
   * Parse a String to a Double avoiding the
   * JVM parsing bug.  May throw NumberFormatException
   * if the String is not properly formatted
   */
  def apply(str: String): Double = str.trim match {
    case "2.2250738585072012e-308" => throw new NumberFormatException("Error parsing 2.2250738585072012e-308")
    case str => java.lang.Double.parseDouble(str)
  }

  /**
   * A handy dandy extractor
   */
  def unapply(in: String): Option[Double] = try {
    Some(apply(in))
  } catch {
    case e: Exception => None
  }
}
