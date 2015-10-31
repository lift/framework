/*
 * Copyright 2006-2012 WorldWide Conferencing, LLC
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

import net.liftweb.common.Full
import org.specs2.mutable.Specification
import org.specs2.specification.AfterEach
import Props.RunModes._

object PropsSpec extends Specification with AfterEach {
  "Props Specification".title
  sequential

  // TODO: Why doesn't this work??
  def after = Props.testReset()

  "Props" should {
    "Detect test mode correctly" in {
      Props.testMode must_== true
    }

    "Allow modification of run-mode properties before the run-mode is set" in {
      val before = Props.autoDetectRunModeFn.get
      try {
        Props.runModeInitialised = false
        Props.autoDetectRunModeFn.allowModification must_== true
        Props.autoDetectRunModeFn.set(() => Test) must_== true
        Props.autoDetectRunModeFn.get must_!= before
      } finally {
        Props.autoDetectRunModeFn.set(before)
        Props.runModeInitialised = true
      }
    }

    "Prohibit modification of run-mode properties when the run-mode is set" in {
      val before = Props.autoDetectRunModeFn.get
      Props.autoDetectRunModeFn.allowModification must_== false
      Props.autoDetectRunModeFn.set(() => Test) must_== false
      Props.autoDetectRunModeFn.get must_== before
    }

    "Parse and cast to int" in {
      Props.testReset()
      Props.getInt("an.int") must_== Full(42)
    }

    "Parse and cast to long" in {
      Props.testReset()
      Props.getLong("a.long") must_== Full(9223372036854775807L)
    }

    "Parse and cast to boolean" in {
      Props.testReset()
      Props.getBool("a.boolean") must_== Full(true)
    }

    "Prefer prepended properties to the test.default.props" in {
      Props.testReset()
      Props.prependSource(Map("jetty.port" -> "8080"))
      val port = Props.getInt("jetty.port")

      port must_== Full(8080)
    }

    "Prefer prepended System.properties to the test.default.props" in {
      Props.testReset()
      System.setProperty("omniauth.baseurl", "http://google.com")
      Props.prependSource(sys.props)
      val baseurl = Props.get("omniauth.baseurl")

      baseurl must_== Full("http://google.com")
    }

    "Read through to System.properties, correctly handling mutation" in {
      Props.testReset()
      System.setProperty("omniauth.baseurl", "http://google.com")
      Props.prependSource(sys.props)
      System.setProperty("omniauth.baseurl", "http://ebay.com")
      val baseurl = Props.get("omniauth.baseurl")

      baseurl must_== Full("http://ebay.com")
    }

    "Find properties in appended maps when not defined in test.default.props" in {
      Props.testReset()
      Props.appendSource(Map("new.prop" -> "new.value"))
      val prop = Props.get("new.prop")

      prop must_== Full("new.value")
    }

    "Not interpolate values when no interpolator is given" in {
      Props.testReset()
      val port = Props.get("jetty.port")

      port must_== Full("${PORT}")
    }

    "Interpolate values from the given interpolator" in {
      Props.testReset()
      Props.appendInterpolator(Map("PORT" -> "8080"))
      val port = Props.getInt("jetty.port")

      port must_== Full(8080)
    }

    "Interpolate multiple values in a string from the given interpolator" in {
      Props.testReset()
      Props.appendInterpolator(Map("DB_HOST" -> "localhost", "DB_PORT" -> "3306"))
      val url = Props.get("db.url")

      url must_== Full("jdbc:mysql://localhost:3306/MYDB")
    }

    "Find properties in append for require()" in {
      Props.testReset()
      Props.appendSource(Map("new.prop" -> "new.value"))
      Props.require("new.prop") must_== Nil
    }

    "Find properties in prepend for require()" in {
      Props.testReset()
      Props.prependSource(Map("new.prop" -> "new.value"))
      Props.require("new.prop") must_== Nil
    }
  }
}
