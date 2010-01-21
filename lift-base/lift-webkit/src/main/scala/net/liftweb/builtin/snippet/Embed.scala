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

import Box._

object Embed extends DispatchSnippet {

  def dispatch : DispatchIt = {
    case _ => render _
  }

  def render(kids: NodeSeq) : NodeSeq =
  (for {
      ctx <- S.session ?~ ("FIX"+"ME: session is invalid")
      what <- S.attr ~ ("what") ?~ ("FIX" + "ME The 'what' attribute not defined")
      template <- ctx.findTemplate(what.text) ?~ ("FIX"+"ME the "+what+" template not found")
    } yield template) match {
    case Full(t) => t
    case Failure(msg, _, _) => Comment(msg)
    case _ => Comment("FIXME: session is invalid")
  }

}

}
}
}
