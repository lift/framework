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
 *
 * This wonkaround is not recommended anymore. Instead consider using a
 * newer version of JVM which has the necessary fix.
 *
 * @see http://blogs.oracle.com/security/entry/security_alert_for_cve-2010-44
 */
@deprecated("Use a newer or patched JVM instead.", "2.5")
object ParseDouble {
  private val BrokenDouble = BigDecimal("2.2250738585072012e-308")

  /**
   * Parse a String to a Double avoiding the
   * JVM parsing bug.  May throw NumberFormatException
   * if the String is not properly formatted
   */
  def apply(str: String): Double = {
    val d = BigDecimal(str)
    if (d == BrokenDouble) error("Error parsing 2.2250738585072012e-308")
    else d.doubleValue
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
