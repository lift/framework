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
import _root_.net.liftweb.actor._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import JsCmds._
import JE._
import S._
import Helpers._

/**
 * Buitin snippet for rendering page fragments asynchronously
 *
 */
object LazyLoad extends DispatchSnippet {

  object lazyLoadCount extends RequestVar(0)

  def dispatch : DispatchIt = {
    case _ => render _
  }

  def render(xhtml: NodeSeq): NodeSeq = {

    (for (session <- S.session ?~ ("FIXME: Invalid session")) yield {
      val id = "lazy_"+ Helpers.nextFuncName;

      lazyLoadCount(lazyLoadCount.get + 1);

      val attrs = S.attrs
      val req = (S.request openOr Req.nil) snapshot

      val func: () => JsCmd = () => S.lightInit(req, session, attrs){
        Replace(id, xhtml)
      }
    
      val res = <div id={id}></div> ++ (
        if (lazyLoadCount.get == 1) {
          // Add the comet only once per page
          session.addAndInitCometActor(new AsyncRenderComet(),
                                       Full("AsyncRenderComet"),
                                       Full(id),
                                       NodeSeq.Empty, Map.empty)

          <tail><lift:comet type="AsyncRenderComet" name={id}></lift:comet></tail>
        } else {
          NodeSeq.Empty
        }
      )
    
      for (comet <- session.findComet("AsyncRenderComet");
           name <- comet.name if (name == id)) {
        comet ! Ready(func)
      }

      res
    }) match {
      case Full(x) => x
      case Empty => Comment("FIX"+ "ME: session or request are invalid")
      case Failure(msg, _, _) => Comment(msg)
    }

  }

}


private case class Ready(js: () => JsCmd)
private case class StopClient()


/**
 * The Comet Actor for sending down the computed page fragments
 *
 */
class AsyncRenderComet extends CometActor {

  var content = NodeSeq.Empty

  var stopClient = false

  override def lifespan: Box[TimeSpan] = Full(90 seconds)

  def render: RenderOut = {
    content
  }

  override protected def localSetup() {
    ActorPing.schedule(this, StopClient, 1 minute)
  }


  override def lowPriority : PartialFunction[Any, Unit] = {
    case Ready(js) => partialUpdate(js())
    case StopClient => unWatch
  }
}

}
}
}
