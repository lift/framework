/*
 * Copyright 2009-2011 WorldWide Conferencing, LLC
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
import scala.collection.immutable.{Map}
import net.liftweb.http._
import net.liftweb.common._

object WithParamVar extends RequestVar[Map[String, NodeSeq]](Map.empty)

/**
 *  Evaluates the body and stores it in the WithParam RequestVar map.
 *  This map is used in builtin.snippet.Surround to bind content to named sections.
 *  Note that the WithParam snippet is also mapped to "bind-at"
 */
object WithParam extends DispatchSnippet {

  def dispatch : DispatchIt = {
    case _ => render _
  }

  /**
   *  Evaluates the body and stores it in the WithParam RequestVar map.
   *  This map is used in builtin.snippet.Surround to bind content to named sections.
   *  Note that the WithParam snippet is also mapped to "bind-at"
   */
  def render(kids: NodeSeq) : NodeSeq = {
    (for {
      ctx <- S.session ?~ ("FIX"+"ME: Invalid session")
      req <- S.request ?~ ("FIX"+"ME: Invalid request")
    } yield {
      val name: String = S.attr("name") openOr "main"
      val body = ctx.processSurroundAndInclude(PageName.get, kids)
       WithParamVar.atomicUpdate(_ + (name -> body))
       NodeSeq.Empty
    }) match {
      case Full(x) => x
      case Empty => Comment("FIX"+ "ME: session or request are invalid")
      case Failure(msg, _, _) => Comment(msg)
    }
  }

}

