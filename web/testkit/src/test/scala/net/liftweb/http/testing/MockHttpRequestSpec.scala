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
package mocks

import org.specs2.mutable.Specification

import json.JsonDSL._


/**
 * System under specification for MockHttpRequest.
 */
object MockHttpRequestSpec extends Specification  {
  "MockHttpRequest Specification".title

  val IF_MODIFIED_HEADER = "If-Modified-Since"
  val TEST_URL = "https://foo.com/test/this/page?a=b&b=a&a=c"
  val TEST_URL_BLANK_PARAMETER = "https://foo.com/test/this/page?a=b&b=a&c=&d"
  val TEST_URL_BLANK_PARAMETER_SERIALIZED = "https://foo.com/test/this/page?a=b&b=a&c=&d="

  "MockHttpRequest" should {

    "properly deconstruct from a URL" in {
      val testRequest = new MockHttpServletRequest(TEST_URL, "/test")

      testRequest.getScheme must_== "https"
      testRequest.isSecure must_== true
      testRequest.getServerName must_== "foo.com"
      testRequest.getContextPath must_== "/test"
      testRequest.getRequestURI must_== "/test/this/page"
      testRequest.getRequestURL.toString must_== TEST_URL
      testRequest.getQueryString must_== "a=b&b=a&a=c"
      testRequest.getParameterValues("a").toList must_== List("b","c")
      testRequest.getParameter("b") must_== "a"
    }

    "parse parameters with empty values" in {
      val testRequest = new MockHttpServletRequest(TEST_URL_BLANK_PARAMETER, "/test")

      testRequest.getScheme must_== "https"
      testRequest.isSecure must_== true
      testRequest.getServerName must_== "foo.com"
      testRequest.getContextPath must_== "/test"
      testRequest.getRequestURI must_== "/test/this/page"
      testRequest.getRequestURL.toString must_== TEST_URL_BLANK_PARAMETER_SERIALIZED
      testRequest.getQueryString must_== "a=b&b=a&c=&d="
      testRequest.getParameter("c") must_== ""
      testRequest.getParameter("d") must_== ""
    }

    "correctly add and parse a date header" in {
      val testRequest = new MockHttpServletRequest(TEST_URL, "/test")

      val epoch = 241000 // (milliseconds not included in RFC 1123)

      testRequest.setDateHeader(IF_MODIFIED_HEADER, epoch)

      testRequest.getDateHeader(IF_MODIFIED_HEADER) must_== epoch
    }

    "throw an IllegalArgumentException for an invalid date header" in {
      val testRequest = new MockHttpServletRequest(TEST_URL, "/test")

      testRequest.headers += IF_MODIFIED_HEADER -> List("this is not a valid date")

      testRequest.getDateHeader(IF_MODIFIED_HEADER) must throwA[IllegalArgumentException]
    }

    "throw an IllegalArgumentException for an invalid context path" in {
      (new MockHttpServletRequest(TEST_URL, "foo")) must throwA[IllegalArgumentException]
      (new MockHttpServletRequest(TEST_URL, "/foo/")) must throwA[IllegalArgumentException]
     }

    "throw an IllegalArgumentException for an invalid query string" in {
      val testRequest = new MockHttpServletRequest(TEST_URL, "/test")
      
      (testRequest.queryString ="this=a&&that=b") must throwA[IllegalArgumentException]
     }


    "properly set a default content type for JSON" in {
      val testRequest = new MockHttpServletRequest(TEST_URL, "/test")

      testRequest.body = ("name" -> "joe")

      testRequest.contentType must_== "application/json"
    }

    "properly set a user-specificed content type for JSON" in {
      val testRequest = new MockHttpServletRequest(TEST_URL, "/test")

      testRequest.body_=(("name" -> "joe"), "text/json")

      testRequest.contentType must_== "text/json"
    }

    "properly set a default content type for XML" in {
      val testRequest = new MockHttpServletRequest(TEST_URL, "/test")

      testRequest.body = <test/>

      testRequest.contentType must_== "text/xml"
    }

    "properly set a user-specificed content type for XML" in {
      val testRequest = new MockHttpServletRequest(TEST_URL, "/test")

      testRequest.body_=(<test/>, "application/xml")

      testRequest.contentType must_== "application/xml"  
    }

    "properly set a default content type for a String" in {
      val testRequest = new MockHttpServletRequest(TEST_URL, "/test")

      testRequest.body = "test"

      testRequest.contentType must_== "text/plain"
    }

    "properly set a user-specificed content type for a String" in {
      val testRequest = new MockHttpServletRequest(TEST_URL, "/test")

      testRequest.body_=("test", "text/csv")

      testRequest.contentType must_== "text/csv"
    }
    
  }
}
