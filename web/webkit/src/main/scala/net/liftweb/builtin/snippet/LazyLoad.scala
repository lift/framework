/*
 * Copyright 2007-2012 WorldWide Conferencing, LLC
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

import xml._
import http._
import common._
import actor._
import util._
import http.js._
import JsCmds._
import JE._
import S._
import Helpers._


/**
 * Enclose your snippet tags on your template with LazyLoad and the snippet will execute
 * on a different thread, which avoids blocking the page render.
 */
object LazyLoad extends DispatchSnippet {
  private object myFuncName extends TransientRequestVar(Helpers.nextFuncName)
  private object myActor extends TransientRequestVar[Box[CometActor]](Empty)

  def dispatch: DispatchIt = {
    case _ => render _
  }

  /**
   * Enclose your snippet like this:
   *
   * <pre name="code" class="xml">
   *   &lt;div class="lift:LazyLoad">
   *     &lt;div class="lift:MyLongRunningSnippet">&lt;/div>
   *   &lt;/div>
   * </pre>
   *
   * You can add the template attribute to the LazyLoad tag and instead of
   * showing the spinning circle, it will render your template.
   *
   *
   * <pre name="code" class="xml">
   *   &lt;div class="lift:LazyLoad?template=&#39;my-nice-wait-message-template&#39;">
   *     &lt;div class="lift:MyLongRunningSnippet">&lt;/div>
   *   &lt;/div>
   * </pre>
   *
   *
   */
  def render(xhtml: NodeSeq): NodeSeq = {
    (for {
      session <- S.session ?~ ("FIXME: Invalid session")
    } yield {
      
      // if we haven't created the actor yet, register on this
      // thread to create the AsyncRenderComet actor
      if (myActor.isEmpty) {
        LiftRules.cometCreationFactory.request.set(
          (ccinfo: CometCreationInfo) =>
            ccinfo match {
              case CometCreationInfo(theType @ "AsyncRenderComet",
                                     name,
                                     defaultXml,
                                     attributes,
                                     session) => {
                val ret = new AsyncRenderComet()
                ret.initCometActor(ccinfo)
                ret ! PerformSetupComet2(if (ret.sendInitialReq_?) 
       S.request.map(_.snapshot) else Empty)
                
                // and save it in the request var
                myActor.set(Full(ret))
                
                Full(ret)
              }
              
              case _ => Empty
            })
      }

      val id = Helpers.nextFuncName

      val func: () => JsCmd = 
        session.buildDeferredFunction(() => Replace(id, xhtml))

      <div id={id}>
      {
        S.attr("template") match {
          case Full(template) => <lift:embed what={template}/>
          case _ => <img src="/images/ajax-loader.gif" alt="Loading"/>
        }
      }
      </div>++ (myActor.is match {
        case Full(actor) => actor ! Ready(func); NodeSeq.Empty
        case _ => session.setupComet("AsyncRenderComet", Full(myFuncName.is), Ready(func))
        <tail><lift:comet type="AsyncRenderComet" name={myFuncName.is}/></tail>
      })
    }) match {
      case Full(x) => x
      case Empty => Comment("FIX"+ "ME: session or request are invalid")
      case Failure(msg, _, _) => Comment(msg)
    }

  }

}


private case class Ready(js: () => JsCmd)
private case class Render(js: JsCmd)


/**
 * The Comet Actor for sending down the computed page fragments
 *
 */
class AsyncRenderComet extends CometActor {

  override def lifespan: Box[TimeSpan] = Full(90.seconds)

  def render = NodeSeq.Empty

  // make this method visible so that we can initialize the actor
  override def initCometActor(creationInfo: CometCreationInfo) {
    super.initCometActor(creationInfo)
  }


  override def lowPriority : PartialFunction[Any, Unit] = {
    // farm the request off to another thread
    case Ready(js) => 
      Schedule.schedule(() => this ! Render(js()), 0.seconds)

    // render it
    case Render(js) => 
      partialUpdate(js)
  }
}
