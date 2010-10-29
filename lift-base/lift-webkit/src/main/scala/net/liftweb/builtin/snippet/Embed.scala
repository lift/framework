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

/**
 * This object implements the logic for the &lt;lift:embed&gt; tag. It
 * supports retrieving a template based on the "what" attribute, and
 * any &lt;lift:bind-at&gt; tags contained in the embed tag will be used
 * to replace &lt;lift:bind&gt; tags within the embedded template.
 */
object Embed extends DispatchSnippet {
  private lazy val logger = Logger(this.getClass)

  def dispatch : DispatchIt = {
    case _ => render _
  }

  def render(kids: NodeSeq) : NodeSeq =
  (for {
      ctx <- S.session ?~ ("FIX"+"ME: session is invalid")
      what <- S.attr ~ ("what") ?~ ("FIX" + "ME The 'what' attribute not defined")
      templateOpt <- ctx.findTemplate(what.text) ?~ ("FIX"+"ME the "+what+" template not found")
    } yield (what,LiftSession.checkForContentId(templateOpt))) match {
    case Full((what,template)) => {
      val bindingMap : Map[String,NodeSeq] = Map(kids.flatMap({
        case p : _root_.scala.xml.PCData => None // Discard whitespace and other non-tag junk
        case t : _root_.scala.xml.Text => None // Discard whitespace and other non-tag junk
        case e : Elem if e.prefix == "lift" && e.label == "bind-at" => {
          e.attribute("name") match {
            /* DCB: I was getting a type error if I just tried to use e.child
             * here. I didn't feel like digging to find out why Seq[Node]
             * wouldn't convert to NodeSeq, so I just do it with fromSeq. */
            case Some(name) => Some(name.text -> NodeSeq.fromSeq(e.child))
            case None => logger.warn("Found <lift:bind-at> tag without name while embedding \"%s\"".format(what.text)); None
          }
        }
        case invalidTag => 
          logger.warn("Found invalid embed tag content \"%s\" while embedding \"%s\"".format(invalidTag, what.text)) ; None
      }): _*)

      BindHelpers.bind(bindingMap, template)
    }
    case Failure(msg, _, _) => Comment(msg)
    case _ => Comment("FIXME: session is invalid")
  }

}

}
}
}
