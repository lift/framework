/*
 * Copyright 2016 WorldWide Conferencing, LLC
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

import org.specs2.mutable.Specification
import org.specs2.specification.Fragment

import common._
import util.Props
  import Props.RunModes._

object WebJarLocatorSpec extends Specification  {
  "WebJarLocator Specification".title

  def passBasicTests(example: String, minified: String): Fragment = {
    "return example as is in Development mode" in {
      WebJarLocator.calcPath(example, Development) must_== example
    }

    "return example minified in Production mode" in {
      WebJarLocator.calcPath(example, Production) must_== minified
    }

    "return minified in any mode if minified is passed in" in {
      WebJarLocator.calcPath(minified, Development) must_== minified
      WebJarLocator.calcPath(minified, Production) must_== minified
    }

    success
  }

  "WebJarLocator.calcPath" should {

    "jquery" in {
      passBasicTests("jquery.js", "jquery.min.js")
    }

    "jquery.plugin" in {
      passBasicTests("jquery.plugin.js", "jquery.plugin.min.js")
    }

    "jquery.plugin.test" in {
      passBasicTests("jquery.plugin.test.js", "jquery.plugin.test.min.js")
    }
  }
}
