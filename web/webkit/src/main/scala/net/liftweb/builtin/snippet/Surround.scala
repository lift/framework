/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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
package builtin {
package snippet {

import _root_.scala.xml._
import _root_.net.liftweb.http._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

import Helpers._

object Surround extends DispatchSnippet {

  def dispatch : DispatchIt = {
    case _ => render _
  }

  def render(kids: NodeSeq) : NodeSeq =
  (for {ctx <- S.session ?~ ("FIX"+"ME: Invalid session")
        req <- S.request ?~ ("FIX"+"ME: Invalid request")
    } yield {
      val mainParam = (S.attr.~("at").map(_.text).
                       getOrElse("main"), ctx.processSurroundAndInclude(PageName.get, kids))
      val paramsMap = WithParamVar.get + mainParam
      ctx.findAndMerge(S.attr.~("with"), paramsMap)
    }) match {
    case Full(x) => x
    case Empty => Comment("FIX"+ "ME: session or request are invalid")
    case Failure(msg, _, _) => Comment(msg)
  }
}

}
}
}
