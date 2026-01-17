/*
 * Copyright 2009-2011 WorldWide Conferencing, LLC
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
package http
package provider
package encoder

import java.util._
import net.liftweb.http.provider.{HTTPCookie, SameSite}
import net.liftweb.common.{Full}

/**
  * Converts an HTTPCookie into a string to used as header cookie value.
  * 
  * The string representation follows the <a href="https://tools.ietf.org/html/rfc6265">RFC6265</a>
  * standard with the added field of SameSite to support secure browsers as explained at
  * <a href="https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie/SameSite">MDN SameSite Cookies</a>
  *
  * This code is based on the Netty's HTTP cookie encoder.
  *
  * Multiple cookies are supported just sending separate "Set-Cookie" headers for each cookie.
  *
  */
object CookieEncoder {

  private val VALID_COOKIE_NAME_OCTETS = validCookieNameOctets();

  private val VALID_COOKIE_VALUE_OCTETS = validCookieValueOctets();

  private val VALID_COOKIE_ATTRIBUTE_VALUE_OCTETS = validCookieAttributeValueOctets();

  private val PATH = "Path"

  private val EXPIRES = "Expires"

  private val MAX_AGE = "Max-Age"

  private val DOMAIN = "Domain"

  private val SECURE = "Secure"

  private val HTTPONLY = "HTTPOnly"

  private val SAMESITE = "SameSite"

  private val DAY_OF_WEEK_TO_SHORT_NAME = Array("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")

  private val CALENDAR_MONTH_TO_SHORT_NAME = Array("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                                                   "Sep", "Oct", "Nov", "Dec")

  def encode(cookie: HTTPCookie): String = {
    val name = cookie.name
    val value = cookie.value.getOrElse("")
    val skipValidation = isOldVersionCookie(cookie)
    if (!skipValidation) {
      validateCookie(name, value)
    }
    val buf = new StringBuilder()
    add(buf, name, value);
    cookie.maxAge foreach { maxAge =>
      add(buf, MAX_AGE, maxAge);
      val expires = new Date(maxAge * 1000 + System.currentTimeMillis());
      buf.append(EXPIRES);
      buf.append('=');
      appendDate(expires, buf);
      buf.append(';');
      buf.append(' ');
    }
    cookie.path foreach { path =>
      add(buf, PATH, path);
    }
    cookie.domain foreach { domain =>
      add(buf, DOMAIN, domain);
    }
    cookie.secure_? foreach { isSecure =>
      if (isSecure) add(buf, SECURE);
    }
    cookie.httpOnly foreach { isHttpOnly =>
      if (isHttpOnly) add(buf, HTTPONLY)
    }
    cookie.sameSite foreach {
      case SameSite.LAX =>
        add(buf, SAMESITE, "Lax")
      case SameSite.STRICT =>
        add(buf, SAMESITE, "Strict")
      case SameSite.NONE =>
        add(buf, SAMESITE, "None")
    }
    stripTrailingSeparator(buf)
  }

  private def validateCookie(name: String, value: String): Unit = {
    val posFirstInvalidCookieNameOctet = firstInvalidCookieNameOctet(name)
    if (posFirstInvalidCookieNameOctet >= 0) {
      throw new IllegalArgumentException("Cookie name contains an invalid char: " +
                                          name.charAt(posFirstInvalidCookieNameOctet))
    }
    val unwrappedValue = unwrapValue(value);
    if (unwrappedValue == null) {
      throw new IllegalArgumentException("Cookie value wrapping quotes are not balanced: " +
                                          value);
    }
    val postFirstInvalidCookieValueOctet = firstInvalidCookieValueOctet(unwrappedValue)
    if (postFirstInvalidCookieValueOctet >= 0) {
      throw new IllegalArgumentException("Cookie value contains an invalid char: " +
                                          unwrappedValue.charAt(postFirstInvalidCookieValueOctet));
    }
  }

  /**
    * Checks if the cookie is set with an old version 0.
    * 
    * More info about the cookie version at https://javadoc.io/static/jakarta.servlet/jakarta.servlet-api/5.0.0/jakarta/servlet/http/Cookie.html#setVersion-int-
    *
    * @param cookie
    * @return true if the cookie version is 0, false if it has no value or a different value than 0
    */
  private def isOldVersionCookie(cookie: HTTPCookie): Boolean = {
    cookie.version map (_ == 0) getOrElse false
  }

  private def appendDate(date: Date, sb: StringBuilder): StringBuilder = {
    val cal = new GregorianCalendar(TimeZone.getTimeZone("UTC"))
    cal.setTime(date)
    sb.append(DAY_OF_WEEK_TO_SHORT_NAME(cal.get(Calendar.DAY_OF_WEEK) - 1)).append(", ")
    appendZeroLeftPadded(cal.get(Calendar.DAY_OF_MONTH), sb).append(' ')
    sb.append(CALENDAR_MONTH_TO_SHORT_NAME(cal.get(Calendar.MONTH))).append(' ')
    sb.append(cal.get(Calendar.YEAR)).append(' ')
    appendZeroLeftPadded(cal.get(Calendar.HOUR_OF_DAY), sb).append(':')
    appendZeroLeftPadded(cal.get(Calendar.MINUTE), sb).append(':')
    appendZeroLeftPadded(cal.get(Calendar.SECOND), sb).append(" GMT")
  }

  private def appendZeroLeftPadded(value: Int, sb: StringBuilder): StringBuilder = {
    if (value < 10) {
      sb.append('0');
    }
    return sb.append(value);
  }

  private def validCookieNameOctets() = {
    val bits = new BitSet()
    (32 until 127) foreach bits.set
    val separators = Array('(', ')', '<', '>', '@', ',', ';', ':', '\\', '"', '/', '[', ']', '?', '=', '{',
                           '}', ' ', '\t' )
    separators.foreach(separator => bits.set(separator, false))
    bits
  }

  private def validCookieValueOctets() = {
    val bits = new BitSet()
    bits.set(0x21);
    (0x23 to 0x2B) foreach bits.set
    (0x2D to 0x3A) foreach bits.set
    (0x3C to 0x5B) foreach bits.set
    (0x5D to 0x7E) foreach bits.set
    bits
  }

  private def validCookieAttributeValueOctets() = {
    val bits = new BitSet()
    (32 until 127) foreach bits.set
    bits.set(';', false)
    bits
  }

  private def stripTrailingSeparator(buf: StringBuilder) = {
    if (buf.nonEmpty) {
      buf.setLength(buf.length() - 2);
    }
    buf.toString()
  }

  private def add(sb: StringBuilder, name: String, value: Long) = {
    sb.append(name);
    sb.append('=');
    sb.append(value);
    sb.append(';');
    sb.append(' ');
  }

  private def add(sb: StringBuilder, name: String, value: String) = {
    sb.append(name);
    sb.append('=');
    sb.append(value);
    sb.append(';');
    sb.append(' ');
  }

  private def add(sb: StringBuilder, name: String) = {
    sb.append(name);
    sb.append(';');
    sb.append(' ');
  }

  private def firstInvalidCookieNameOctet(cs: CharSequence): Int = {
    return firstInvalidOctet(cs, VALID_COOKIE_NAME_OCTETS);
  }

  private def firstInvalidCookieValueOctet(cs: CharSequence): Int = {
    return firstInvalidOctet(cs, VALID_COOKIE_VALUE_OCTETS);
  }

  private def firstInvalidOctet(cs: CharSequence, bits: BitSet): Int = {
    (0 until cs.length()).foreach { i =>
      val c = cs.charAt(i)
      if (!bits.get(c)) {
      return i;
      }
    }
    -1;
  }

  private def unwrapValue(cs: CharSequence): CharSequence = {
    val len = cs.length()
    if (len > 0 && cs.charAt(0) == '"') {
      if (len >= 2 && cs.charAt(len - 1) == '"') {
        if (len == 2) "" else cs.subSequence(1, len - 1)
      } else {
        null
      }
    } else {
      cs
    }
  }

}
