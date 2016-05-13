/*
 * Copyright 2007-2011 WorldWide Conferencing, LLC
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
package builtin
package snippet

import scala.xml._
import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.common._

import Helpers._

object Surround extends DispatchSnippet {

  def dispatch : DispatchIt = {
    case _ => render _
  }

  def render(kids: NodeSeq) : NodeSeq =
  (for {
    ctx <- S.session ?~ ("FIX"+"ME: Invalid session")
  } yield {
    def eatDiv(in: NodeSeq): NodeSeq = 
      if (S.attr("eat").isDefined) in.flatMap {
        case e: Elem => e.child
        case n => n
      } else in

    WithParamVar.doWith(Map()) {
      lazy val mainParam = (S.attr("at") openOr "main",
        eatDiv(ctx.processSurroundAndInclude(PageName.get, kids)))
      lazy val paramsMap = {
        val q = mainParam // perform the side-effecting thing here
        WithParamVar.get + q // WithParamVar is the side effects of processing the template
      }
      ctx.findAndMerge(S.attr("with"), paramsMap)
    }
  }) match {
    case Full(x) => x
    case Empty => Comment("FIX"+ "ME: session or request are invalid")
    case Failure(msg, _, _) => Comment(msg)
  }
}

