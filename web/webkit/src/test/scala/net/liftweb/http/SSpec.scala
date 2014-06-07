/*
 * Copyright 2013 WorldWide Conferencing, LLC
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

import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification
import util.Helpers
import util.Props.RunModes
import LiftRules.defaultFuncNameGenerator

object SSpec extends Specification with XmlMatchers {
  "S Specification".title

  "formFuncName" should {
    "generate random names when not in Test mode" in {
      for (mode <- RunModes.values if mode != RunModes.Test) {
        val a,b = defaultFuncNameGenerator(mode)()
        a must startWith("F")
        a.length must_== Helpers.nextFuncName.length
        a must_!= b
      }

      success
    }

    "generate predictable names in Test mode" in {
      val a,b = S.formFuncName
      a must startWith("f")
      a.length must_!= Helpers.nextFuncName.length
      a must_== b
      a must_!= S.formFuncName
    }

    "generate resort back to random names when test func-names disabled" in {
      S.disableTestFuncNames {
        val a,b = S.formFuncName
        a must startWith("F")
        a.length must_== Helpers.nextFuncName.length
        a must_!= b
      }
    }

  }
}
