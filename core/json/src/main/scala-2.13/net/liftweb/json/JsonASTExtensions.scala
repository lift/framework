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

/** Scala 2.13-specific extensions for JsonAST - supports path-dependent types */
trait JsonASTExtensions {
  self: JsonAST.JValue =>
  
  /**
   * Find immediate children of this `[[JValue]]` that match a specific `JValue` subclass.
   *
   * This methid will search a `[[JObject]]` or `[[JArray]]` for values of a specific type and
   * return a `List` of those values if they any are found.
   *
   * So given some JSON like so:
   *
   * {{{
   *  [
   *    {
   *      "thinga":1,
   *      "thingb":"bacon"
   *    },{
   *      "thingc":3,
   *      "thingd":"Wakka"
   *    },{
   *      "thinge":{
   *        "thingf":4
   *      },
   *      "thingg":true
   *    }
   *  ]
   * }}}
   *
   * You would use this method like so:
   *
   * {{{
   * scala> json \ classOf[JInt]
   * res0: List[net.liftweb.json.JInt#Values] = List(1, 3)
   * }}}
   *
   * This method does require that whatever type you're searching for is subtype of `JValue`.
   */
  def \[A <: JsonAST.JValue](clazz: Class[A]): List[A#Values] =
    findDirect(children, typePredicate(clazz) _).asInstanceOf[List[A]] map { _.values }

  /**
   * Find all descendants of this `JValue` that match a specific `JValue` subclass.
   *
   * Unlike its cousin `\`, this method will recurse down into all children looking for
   * type matches searching a `[[JObject]]` or `[[JArray]]` for values of a specific type and
   * return a `List` of those values if they are found.
   *
   * So given some JSON like so:
   *
   * {{{
   *  [
   *    {
   *      "thinga":1,
   *      "thingb":"bacon"
   *    },{
   *      "thingc":3,
   *      "thingd":"Wakka"
   *    },{
   *      "thinge":{
   *        "thingf":4
   *      },
   *      "thingg":true
   *    }
   *  ]
   * }}}
   *
   * You would use this method like so:
   *
   * {{{
   * scala> json \\ classOf[JInt]
   * res0: List[net.liftweb.json.JInt#Values] = List(1, 3, 4)
   * }}}
   */
  def \\[A <: JsonAST.JValue](clazz: Class[A]): List[A#Values] =
    (this filter typePredicate(clazz) _).asInstanceOf[List[A]] map { _.values }

  private def findDirect(xs: List[JsonAST.JValue], p: JsonAST.JValue => Boolean): List[JsonAST.JValue] = xs.flatMap {
    case JsonAST.JObject(l) =>
      l.collect {
        case JsonAST.JField(n, x) if p(x) => x
      }
    case JsonAST.JArray(l) => findDirect(l, p)
    case x if p(x) => x :: Nil
    case _ => Nil
  }

  private def typePredicate[A <: JsonAST.JValue](clazz: Class[A])(json: JsonAST.JValue) = json match {
    case x if x.getClass == clazz => true
    case _ => false
  }
}
