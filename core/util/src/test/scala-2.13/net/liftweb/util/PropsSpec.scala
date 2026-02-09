/*
 * Copyright 2006-2026 Lift Committers and Contributors
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

import java.io.ByteArrayInputStream

import org.specs2.mutable.Specification
import org.specs2.mutable.After

import common._
import Props.RunModes._

class PropsSpec extends Specification {
  "Props Specification".title

  case class TestProps() extends Props

  "Props" should {
    "Detect test mode correctly" in {
      TestProps().testMode must_== true
    }

    "Allow modification of whereToLook before run-mode is set" in {
      val testProps = TestProps()
      val originalWtl = testProps.whereToLook

      var wasCalled = false
      testProps.whereToLook = () => {
        wasCalled = true

        List(
          ("test propsters", () => Full(new ByteArrayInputStream("test.prop=value".getBytes("UTF-8"))))
        )
      }

      testProps.getInt("jetty.port") must_== Empty
      testProps.get("test.prop") must_== Full("value")
      wasCalled must_== true
    }

    "Allow modification of run-mode properties before the run-mode is set" in {
      val testProps = TestProps()

      val before = testProps.autoDetectRunModeFn.get
      try {
        testProps.runModeInitialised = false
        testProps.autoDetectRunModeFn.allowModification must_== true
        testProps.autoDetectRunModeFn.set(() => Test) must_== true
        testProps.autoDetectRunModeFn.get must_!= before
      } finally {
        testProps.autoDetectRunModeFn.set(before)
        testProps.runModeInitialised = true
      }
    }

    "Prohibit modification of run-mode properties when the run-mode is set" in {
      val testProps = TestProps()

      val before = testProps.autoDetectRunModeFn.get
      testProps.mode // initialize run mode
      testProps.autoDetectRunModeFn.allowModification must_== false
      testProps.autoDetectRunModeFn.set(() => Test) must_== false
      testProps.autoDetectRunModeFn.get must_== before
    }

    "Parse and cast to int" in {
      TestProps().getInt("an.int") must_== Full(42)
    }

    "Parse and cast to long" in {
      TestProps().getLong("a.long") must_== Full(9223372036854775807L)
    }

    "Parse and cast to boolean" in {
      TestProps().getBool("a.boolean") must_== Full(true)
    }

    "Prefer prepended properties to the test.default.props" in {
      val testProps = TestProps()

      testProps.prependProvider(Map("jetty.port" -> "8080"))
      val port = testProps.getInt("jetty.port")

      port must_== Full(8080)
    }

    "Prefer prepended System.properties to the test.default.props" in {
      val testProps = TestProps()

      System.setProperty("omniauth.baseurl1", "http://google.com")

      testProps.prependProvider(sys.props)
      val baseurl = testProps.get("omniauth.baseurl1")

      baseurl must_== Full("http://google.com")
    }

    "Read through to System.properties, correctly handling mutation" in {
      val testProps = TestProps()

      System.setProperty("omniauth.baseurl2", "http://google.com")
      testProps.prependProvider(sys.props)
      System.setProperty("omniauth.baseurl2", "http://ebay.com")
      val baseurl = testProps.get("omniauth.baseurl2")

      baseurl must_== Full("http://ebay.com")
    }

    "Find properties in appended maps when not defined in test.default.props" in {
      val testProps = TestProps()

      testProps.appendProvider(Map("new.prop" -> "new.value"))
      val prop = testProps.get("new.prop")

      prop must_== Full("new.value")
    }

    "Not interpolate values when no interpolator is given" in {
      val port = TestProps().get("jetty.port")

      port must_== Full("${PORT}")
    }

    "Interpolate values from the given interpolator" in {
      val testProps = TestProps()

      testProps.appendInterpolationValues(Map("PORT" -> "8080"))
      val port = testProps.getInt("jetty.port")

      port must_== Full(8080)
    }

    "Interpolate multiple values in a string from the given interpolator" in {
      val testProps = TestProps()

      testProps.appendInterpolationValues(Map("DB_HOST" -> "localhost", "DB_PORT" -> "3306"))
      val url = testProps.get("db.url")

      url must_== Full("jdbc:mysql://localhost:3306/MYDB")
    }

    "Find properties in append for require()" in {
      val testProps = TestProps()

      testProps.appendProvider(Map("new.prop" -> "new.value"))
      testProps.require("new.prop") must_== Nil
    }

    "Find properties in prepend for require()" in {
      val testProps = TestProps()

      testProps.prependProvider(Map("new.prop" -> "new.value"))
      testProps.require("new.prop") must_== Nil
    }
  }
}
