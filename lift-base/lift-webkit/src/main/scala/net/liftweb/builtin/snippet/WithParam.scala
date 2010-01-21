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
package builtin {
package snippet {

import _root_.scala.xml._
import _root_.scala.collection.immutable.{Map}
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._

object WithParamVar extends RequestVar[Map[String, NodeSeq]](Map.empty)

object WithParam extends DispatchSnippet {

  def dispatch : DispatchIt = {
    case _ => render _
  }

  def render(kids: NodeSeq) : NodeSeq = {
    (for {ctx <- S.session ?~ ("FIX"+"ME: Invalid session")
          req <- S.request ?~ ("FIX"+"ME: Invalid request")
    } yield {
       val name = S.attr.~("name").map(_.text).getOrElse("main")
       WithParamVar(WithParamVar.get + (name -> ctx.processSurroundAndInclude(PageName.get, kids)))
       NodeSeq.Empty
    }) match {
      case Full(x) => x
      case Empty => Comment("FIX"+ "ME: session or request are invalid")
      case Failure(msg, _, _) => Comment(msg)
    }
  }

}

}
}
}
