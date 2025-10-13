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

package net.liftweb
package http
package js

import org.specs2.mutable.Specification

import common._
import util.Helpers._

import org.json4s._
import org.json4s.JsonDSL._

/**
 * System under specification for JsExp.
 */
class JsExpSpec extends Specification  {
  "JsExp Specification".title

  "JsExp" should {
    "Deal with lift-json" in {
      val json = ("a" -> 4) ~ ("b" -> 44)

      JE.JsArray(json, "dog").toJsCmd === (
        """[{"a":4,"b":44}, "dog"]""" + "\n")

      
    }

    "Implicitly convert from Numeric types" in {
      import JsExp._

      (42:JsExp) === JE.Num(42)
      (42L:JsExp) === JE.Num(42L)
      (42.0:JsExp) === JE.Num(42.0)
      (42.0f:JsExp) === JE.Num(42.0f)
      success
    }

    "Correctly infer type" in {
      val l:List[Option[Double]] = List(Some(1), None)

      import JsExp._

      // Can't get this to work:  JE.JsArray(l map {d => (d.getOrElse(0.0)):JsExp}) === JE.JsArray(1.0, 0.0)
      JE.JsArray(l map {d => (d.getOrElse(0.0):Double):JsExp}) === JE.JsArray(1.0, 0.0)
      success
    }
  
  }

  "JsExp ~>" should {
    import JE._

    val js = JsVar("myVar")

    "handle Full parameters" in {
      val expected = (js ~> JsFunc("x") ~> JsFunc("y")).toJsCmd
      (js ~> Full(JsFunc("x")) ~> JsFunc("y")).toJsCmd === expected
      (js ~> Some(JsFunc("x")) ~> JsFunc("y")).toJsCmd === expected
    }

    "ignore Empty parameters" in {
      val expected = (js ~> JsFunc("x")).toJsCmd
      (js ~> Empty ~> JsFunc("x")).toJsCmd === expected
      (js ~> None ~> JsFunc("x")).toJsCmd === expected
    }
  }

  "JsArray" should {
    "work with varags" in {
      JE.JsArray(2, "x", 42.0).toJsCmd === "[2, \"x\", 42.0]\n"
    }
    
    "work with lists" in {
      val l:List[JsExp] = List(2, "x", 42.0)
      JE.JsArray(l).toJsCmd === "[2, \"x\", 42.0]\n"
    }
  }
}
