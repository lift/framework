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

import org.specs2.mutable.Specification
import Props.RunModes._

/**
 * Systems under specification for Lift Mailer.
 */
object PropsSpec extends Specification {
  "Props Specification".title
  sequential

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
  }
}
