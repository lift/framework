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

package net.liftweb {
package json {

/** Function to merge two JSONs.
 */
object Merge {
  import JsonAST._

  /** Return merged JSON.
   * <p>
   * Example:<pre>
   * val m = ("name", "joe") ~ ("age", 10) merge ("name", "joe") ~ ("iq", 105)
   * m: JObject(List(JField(name,JString(joe)), JField(age,JInt(10)), JField(iq,JInt(105))))
   * </pre>
   */
  def merge(val1: JValue, val2: JValue): JValue = (val1, val2) match {
    case (JObject(xs), JObject(ys)) => JObject(mergeFields(xs, ys))
    case (JArray(xs), JArray(ys)) => JArray(mergeVals(xs, ys))
    case (JField(n1, v1), JField(n2, v2)) if n1 == n2 => JField(n1, merge(v1, v2))
    case (f1: JField, f2: JField) => f2
    case (JNothing, x) => x
    case (x, JNothing) => x
    case (_, y) => y
  }

  private def mergeFields(vs1: List[JField], vs2: List[JField]): List[JField] = {
    def mergeRec(xleft: List[JField], yleft: List[JField]): List[JField] = xleft match {
      case Nil => yleft
      case JField(xn, xv) :: xs => yleft find (_.name == xn) match {
        case Some(y @ JField(yn, yv)) => JField(xn, merge(xv, yv)) :: mergeRec(xs, yleft-y)
        case None => JField(xn, xv) :: mergeRec(xs, yleft)
      }
    }

    mergeRec(vs1, vs2)
  }

  private def mergeVals(vs1: List[JValue], vs2: List[JValue]): List[JValue] = {
    def mergeRec(xleft: List[JValue], yleft: List[JValue]): List[JValue] = xleft match {
      case Nil => yleft
      case x :: xs => yleft find (_ == x) match {
        case Some(y) => merge(x, y) :: mergeRec(xs, yleft-y)
        case None => x :: mergeRec(xs, yleft)
      }
    }

    mergeRec(vs1, vs2)
  }

  private[json] trait Mergeable { this: JValue =>
    /** Return merged JSON.
     * @see net.liftweb.json.Merge#merge
     */
    def merge(other: JValue): JValue = Merge.merge(this, other)
  }
}

}
}
