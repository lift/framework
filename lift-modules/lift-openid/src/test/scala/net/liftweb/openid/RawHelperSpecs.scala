/*
 * Copyright 2010 WorldWide Conferencing, LLC
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

package net.liftweb {
package openid {

import _root_.org.specs._
import _root_.net.liftweb.common.Box._
import _root_.org.specs.runner._
import _root_.org.specs.Sugar._
import _root_.org.specs.ScalaCheck
import _root_.org.scalacheck.Gen._
import _root_.org.scalacheck._
import _root_.org.scalacheck.Arbitrary._
import _root_.org.scalacheck.Prop.{forAll}


class RawHelperAsTest extends JUnit3(RawHelperSpecs)
object RawHelperRunner extends ConsoleRunner(RawHelperSpecs)

object RawHelperSpecs extends Specification {
  "RawUtils" should {
    "Convert a java.util.List" in {
      val org: java.util.List[Object] = new java.util.ArrayList[Object]()
      
      org.add("Hello")
      org.add("Woof")

      RawHelper.rawJUL2List[String](org) must_== List("Hello", "Woof")
    }
  }
}

}
}
