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
package oauth {

final class OAuthProblem(val problem: (String, Int),val params: List[(String, String)]) {
  def httpCode = problem._2
  override def toString = {
    problem._1.toUpperCase + (if (params.isEmpty) "" else " ("+params.map(p => p._1+": "+p._2).mkString(", ")+")")
  }
}

object OAuthProblem {
  def apply(problem: (String, Int)) = new OAuthProblem(problem, Nil)
  def apply(problem: (String, Int), msg: String) = new OAuthProblem(problem, List(msg -> msg))
  def apply(problem: (String, Int), param: (String, String)) = new OAuthProblem(problem, param :: Nil)
  def apply(problem: (String, Int), params: List[(String, String)]) = new OAuthProblem(problem, params)
}

}
}
