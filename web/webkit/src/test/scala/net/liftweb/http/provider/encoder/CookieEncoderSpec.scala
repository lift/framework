/*
 * Copyright 2010-2011 WorldWide Conferencing, LLC
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

package net.liftweb.http.provider.encoder

import net.liftweb.http.provider._
import net.liftweb.common._
import org.specs2.mutable.Specification

object CookieEncoderSpec extends Specification {

  "CookieEncoder" should {
    "convert a simple cookie" in {
      val cookie = HTTPCookie("test-name", "test-value")
      CookieEncoder.encode(cookie) === "test-name=test-value"
    }

    "convert a secure cookie" in {
      val cookie = HTTPCookie(name = "test-name",
                              value = Full("test-value"),
                              domain = Empty,
                              path = Empty,
                              maxAge = Empty,
                              version = Empty,
                              secure_? = Full(true),
                              httpOnly = Empty,
                              sameSite = Empty)
      CookieEncoder.encode(cookie) === "test-name=test-value; Secure"
    }

    "convert a cookie with a domain" in {
      val cookie = HTTPCookie(name = "test-name",
                              value = Full("test-value"),
                              domain = Full("test-domain.com"),
                              path = Empty,
                              maxAge = Empty,
                              version = Empty,
                              secure_? = Empty,
                              httpOnly = Empty,
                              sameSite = Empty)
      CookieEncoder.encode(cookie) === "test-name=test-value; Domain=test-domain.com"
    }

    "convert a cookie with a path" in {
      val cookie = HTTPCookie(name = "test-name",
                              value = Full("test-value"),
                              domain = Empty,
                              path = Full("/test-path"),
                              maxAge = Empty,
                              version = Empty,
                              secure_? = Empty,
                              httpOnly = Empty,
                              sameSite = Empty)
      CookieEncoder.encode(cookie) === "test-name=test-value; Path=/test-path"
    }

    "convert a cookie with a max age" in {
      val cookie = HTTPCookie(name = "test-name",
                              value = Full("test-value"),
                              domain = Empty,
                              path = Empty,
                              maxAge = Full(10),
                              version = Empty,
                              secure_? = Empty,
                              httpOnly = Empty,
                              sameSite = Empty)
      val encodedCookie = CookieEncoder.encode(cookie)
      encodedCookie.startsWith("test-name=test-value; ") === true
      encodedCookie.contains("Max-Age=10; Expires=") === true
    }

    "convert an HTTP only cookie" in {
      val cookie = HTTPCookie(name = "test-name",
                              value = Full("test-value"),
                              domain = Empty,
                              path = Empty,
                              maxAge = Empty,
                              version = Empty,
                              secure_? = Empty,
                              httpOnly = Full(true),
                              sameSite = Empty)
      CookieEncoder.encode(cookie) === "test-name=test-value; HTTPOnly"
    }

    "convert a same site LAX cookie" in {
      val cookie = HTTPCookie(name = "test-name",
                              value = Full("test-value"),
                              domain = Empty,
                              path = Empty,
                              maxAge = Empty,
                              version = Empty,
                              secure_? = Empty,
                              httpOnly = Empty,
                              sameSite = Full(SameSite.LAX))
      CookieEncoder.encode(cookie) === "test-name=test-value; SameSite=Lax"
    }

    "convert a same site NONE cookie" in {
      val cookie = HTTPCookie(name = "test-name",
                              value = Full("test-value"),
                              domain = Empty,
                              path = Empty,
                              maxAge = Empty,
                              version = Empty,
                              secure_? = Empty,
                              httpOnly = Empty,
                              sameSite = Full(SameSite.NONE))
      CookieEncoder.encode(cookie) === "test-name=test-value; SameSite=None"
    }

    "convert a same site STRICT cookie" in {
      val cookie = HTTPCookie(name = "test-name",
                              value = Full("test-value"),
                              domain = Empty,
                              path = Empty,
                              maxAge = Empty,
                              version = Empty,
                              secure_? = Empty,
                              httpOnly = Empty,
                              sameSite = Full(SameSite.STRICT))
      CookieEncoder.encode(cookie) === "test-name=test-value; SameSite=Strict"
    }

    "convert a secure same site none cookie" in {
      val cookie = HTTPCookie(name = "test-name",
                              value = Full("test-value"),
                              domain = Empty,
                              path = Empty,
                              maxAge = Empty,
                              version = Empty,
                              secure_? = Full(true),
                              httpOnly = Empty,
                              sameSite = Full(SameSite.NONE))
      CookieEncoder.encode(cookie) === "test-name=test-value; Secure; SameSite=None"
    }

    "convert a secure same site strict cookie with max age" in {
      val cookie = HTTPCookie(name = "test-name",
                              value = Full("test-value"),
                              domain = Empty,
                              path = Empty,
                              maxAge = Full(10),
                              version = Empty,
                              secure_? = Full(true),
                              httpOnly = Empty,
                              sameSite = Full(SameSite.NONE))
      val encodedCookie = CookieEncoder.encode(cookie)
      encodedCookie.startsWith("test-name=test-value; Max-Age=10; Expires=") === true
      encodedCookie.endsWith("; Secure; SameSite=None") === true
    }

    "convert a secure same site lax cookie with max age, domain and path" in {
      val cookie = HTTPCookie(name = "test-name",
                              value = Full("test-value"),
                              domain = Full("test-domain.com"),
                              path = Full("/test-path"),
                              maxAge = Full(10),
                              version = Empty,
                              secure_? = Full(true),
                              httpOnly = Full(false),
                              sameSite = Full(SameSite.LAX))
      val encodedCookie = CookieEncoder.encode(cookie)
      encodedCookie.startsWith("test-name=test-value; Max-Age=10; Expires=") === true
      encodedCookie.endsWith("; Path=/test-path; Domain=test-domain.com; Secure; SameSite=Lax") === true
    }

    "convert a secure HTTP only cookie" in {
      val cookie = HTTPCookie(name = "test-name",
                              value = Full("test-value"),
                              domain = Empty,
                              path = Empty,
                              maxAge = Empty,
                              version = Empty,
                              secure_? = Full(true),
                              httpOnly = Full(true),
                              sameSite = Empty)
      CookieEncoder.encode(cookie) === "test-name=test-value; Secure; HTTPOnly"
    }

    "convert a cookie with only the name" in {
      val cookie = HTTPCookie(name = "test-name",
                              value = Empty,
                              domain = Empty,
                              path = Empty,
                              maxAge = Empty,
                              version = Empty,
                              secure_? = Empty,
                              httpOnly = Empty,
                              sameSite = Empty)
      CookieEncoder.encode(cookie) === "test-name="
    }

    "must fail trying to convert an invalid name cookie" in {
      val cookie = HTTPCookie("invalid-name=", "test-value")
      CookieEncoder.encode(cookie) must throwA[IllegalArgumentException](
          "Cookie name contains an invalid char: =")
    }

    "must fail trying to convert an invalid value cookie" in {
      val cookie = HTTPCookie("test-name", "invalid-value\t")
      CookieEncoder.encode(cookie) must throwA[IllegalArgumentException](
          "Cookie value contains an invalid char: \t")
    }

    "must skip validation for old version cookies" in {
      val cookie = HTTPCookie(name = "invalid-name=",
                              value = Full("invalid-value\t"),
                              domain = Empty,
                              path = Empty,
                              maxAge = Empty,
                              version = Full(0),
                              secure_? = Empty,
                              httpOnly = Empty,
                              sameSite = Empty)
      CookieEncoder.encode(cookie) === "invalid-name==invalid-value\t"
    }

    "must validate new version cookies" in {
      val cookie = HTTPCookie(name = "invalid-name=",
                              value = Full("invalid-value\t"),
                              domain = Empty,
                              path = Empty,
                              maxAge = Empty,
                              version = Full(1),
                              secure_? = Empty,
                              httpOnly = Empty,
                              sameSite = Empty)
      CookieEncoder.encode(cookie) must throwA[IllegalArgumentException](
        "Cookie name contains an invalid char: ="
      )
    }
  }

}
