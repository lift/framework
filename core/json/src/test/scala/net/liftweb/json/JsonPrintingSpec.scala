/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
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
package json

import scala.util.Random

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll


/**
 * System under specification for JSON Printing.
 */
class JsonPrintingSpec extends Specification  with JValueGen with ScalaCheck {
  "JSON Printing Specification".title

  "rendering does not change semantics" in {
    val rendering = (json: JValue) => parse(JsonAST.prettyRender(json)) == parse(JsonAST.compactRender(json))
    forAll(rendering)
  }

  "rendering special double values by default" should {
    "render a standard double as is" in {
      val double = Random.nextDouble()
      JsonAST.compactRender(JDouble(double)) must_== double.toString
    }

    "render positive infinity as null" in {
      JsonAST.compactRender(JDouble(Double.PositiveInfinity)) must_== "null"
    }

    "render negative infinity as null" in {
      JsonAST.compactRender(JDouble(Double.NegativeInfinity)) must_== "null"
    }

    "render NaN as null" in {
      JsonAST.compactRender(JDouble(Double.NaN)) must_== "null"
    }
  }

  "rendering special double values with as-is handling" should {
    def render(json: JValue) = {
      JsonAST.render(
        json,
        JsonAST.RenderSettings(0, doubleRenderer = JsonAST.RenderSpecialDoubleValuesAsIs)
      )
    }

    "render a standard double as is" in {
      val double = Random.nextDouble()
      render(JDouble(double)) must_== double.toString
    }

    "render positive infinity as null" in {
      render(JDouble(Double.PositiveInfinity)) must_== "Infinity"
    }

    "render negative infinity as null" in {
      render(JDouble(Double.NegativeInfinity)) must_== "-Infinity"
    }

    "render NaN as null" in {
      render(JDouble(Double.NaN)) must_== "NaN"
    }
  }

  "rendering special double values with special value exceptions enabled" should {
    def render(json: JValue) = {
      JsonAST.render(
        json,
        JsonAST.RenderSettings(0, doubleRenderer = JsonAST.FailToRenderSpecialDoubleValues)
      )
    }

    "render a standard double as is" in {
      val double = Random.nextDouble()
      render(JDouble(double)) must_== double.toString
    }

    "throw an exception when attempting to render positive infinity" in {
      render(JDouble(Double.PositiveInfinity)) must throwAn[IllegalArgumentException]
    }

    "throw an exception when attempting to render negative infinity" in {
      render(JDouble(Double.NegativeInfinity)) must throwAn[IllegalArgumentException]
    }

    "throw an exception when attempting to render NaN" in {
      render(JDouble(Double.NaN)) must throwAn[IllegalArgumentException]
    }
  }

  private def parse(json: String) =
    spray.json.JsonParser(json)

  implicit def arbDoc: Arbitrary[JValue] = Arbitrary(genJValue)
}
