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

/** Scala 3-specific extensions for JsonAST - avoids path-dependent type issues */
trait JsonASTExtensions {
  self: JsonAST.JValue =>
  
  /**
   * Find immediate children of this `[[JValue]]` that match a specific `JValue` subclass.
   *
   * This method will search a `[[JObject]]` or `[[JArray]]` for values of a specific type and
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
   * res0: List[Any] = List(1, 3)
   * }}}
   *
   * This method does require that whatever type you're searching for is subtype of `JValue`.
   * Note: In Scala 3, returns List[Any] instead of List[A#Values] for type safety.
   */
  def \[A <: JsonAST.JValue](clazz: Class[A]): List[Any] = {
    val results = findDirect(children, typePredicate(clazz))
    results.asInstanceOf[List[A]].map { jvalue =>
      jvalue match {
        case JsonAST.JInt(value) => value
        case JsonAST.JDouble(value) => value
        case JsonAST.JString(value) => value
        case JsonAST.JBool(value) => value
        case JsonAST.JNull => null
        case other => other.values
      }
    }
  }

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
   * res0: List[Any] = List(1, 3, 4)
   * }}}
   * 
   * Note: In Scala 3, returns List[Any] instead of List[A#Values] for type safety.
   */
  def \\[A <: JsonAST.JValue](clazz: Class[A]): List[Any] = {
    val filtered = this.filter(typePredicate(clazz))
    filtered.asInstanceOf[List[A]].map { jvalue =>
      jvalue match {
        case JsonAST.JInt(value) => value
        case JsonAST.JDouble(value) => value
        case JsonAST.JString(value) => value
        case JsonAST.JBool(value) => value
        case JsonAST.JNull => null
        case other => other.values
      }
    }
  }

  private def findDirect(xs: List[JsonAST.JValue], p: JsonAST.JValue => Boolean): List[JsonAST.JValue] = xs.flatMap {
    case JsonAST.JObject(l) =>
      l.collect {
        case JsonAST.JField(n, x) if p(x) => x
      }
    case JsonAST.JArray(l) => findDirect(l, p)
    case x if p(x) => x :: Nil
    case _ => Nil
  }

  private def typePredicate[A <: JsonAST.JValue](clazz: Class[A]): JsonAST.JValue => Boolean = { json =>
    json.getClass == clazz
  }
}