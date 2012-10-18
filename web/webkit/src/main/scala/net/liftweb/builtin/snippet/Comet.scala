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

import scala.xml._
import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.common._

import Helpers._

object Comet extends DispatchSnippet with LazyLoggable {
  def dispatch : DispatchIt = {
    case _ => render _
  }

  /**
   *
   * A typical comet tag could look like:
   *
   * <pre name="code" class="xml">
   *   &lt;lift:comet type=&lt;Your comet class> name=&lt;Optional, the name of this comet instance>>{xhtml}</lift:comet>
   * </pre>
   *
   * For the name, you have three options
   * <ul>
   *   <li>You can set a fix name using <pre><code>name="MyComet"</code></pre>
   *   <li>You can use a query parameter, for a url like foo?=id=122, your comet could take the
   *   name "122" if you use: <pre><code>metaname=id</code></pre>
   *   <li>You could assign a random name by using <pre><code>randomname=true</code></pre>
   * </ul>
   *
   *
   * @param kids The NodeSeq that is enclosed by the comet tags
   * @return
   */
  def render(kids: NodeSeq) : NodeSeq = {

    Props.inGAE match {
      case true => Text("Comet Disabled in Google App Engine")
      case _ =>  buildComet(kids)
    }
  }

  private def buildSpan(timeb: Box[Long], xml: NodeSeq, cometActor: LiftCometActor, spanId: String): NodeSeq =
  Elem(cometActor.parentTag.prefix, cometActor.parentTag.label, cometActor.parentTag.attributes,
       cometActor.parentTag.scope, Group(xml)) %
  (new UnprefixedAttribute("id", Text(spanId), Null)) %
  (timeb.filter(_ > 0L).map(time => (new PrefixedAttribute("lift", "when", Text(time.toString), Null))) openOr Null)

  private def buildComet(kids: NodeSeq) : NodeSeq = {
    val theType: Box[String] = S.attr.~("type").map(_.text)
    val name: Box[String] = S.currentAttr("name") or
    S.currentAttr("metaname").flatMap(S.param) or
    S.currentAttr("randomname").map(ignore => Helpers.nextFuncName)


    (for {ctx <- S.session} yield {
      if (!ctx.stateful_?) 
        throw new StateInStatelessException(
          "Lift does not support Comet for stateless requests")

       try {
         ctx.findComet(theType, name, kids, S.attrsFlattenToMap).map {
           c => {
             // Update the view on each page load in dev mode
             // this make development easier
             if (Props.devMode) {
               c ! UpdateDefaultXml(kids)
             }
             
             (c.!?(c.cometRenderTimeout, AskRender)) match {
               case Full(AnswerRender(response, _, when, _)) if c.hasOuter =>
                 buildSpan(Empty, c.buildSpan(when, response.inSpan) ++ response.outSpan, c, c.uniqueId+"_outer")
               
               case Full(AnswerRender(response, _, when, _)) =>
                 c.buildSpan(when, response.inSpan)
               
               case e =>
                 c.cometRenderTimeoutHandler().openOr{
                   throw new CometTimeoutException("type: "+theType+" name: "+name)
                 }
             }}} openOr {
               throw new CometNotFoundException("type: "+theType+" name: "+name)
             }
         
       } catch {
         case e: SnippetFailureException => throw e
         case e: Exception => logger.error("Failed to find a comet actor", e); kids
       }
    }) openOr {
      throw new CometNotFoundException("Session not found. type: "+theType+" name: "+name)
    }
  }
}

abstract class CometFailureException(msg: String) extends SnippetFailureException(msg) {
   override def buildStackTrace: NodeSeq = <div>{msg}</div> ++ super.buildStackTrace
}
class CometTimeoutException(msg: String) extends CometFailureException(msg) {
  def snippetFailure: LiftRules.SnippetFailures.Value = 
    LiftRules.SnippetFailures.CometTimeout


}

class CometNotFoundException(msg: String) extends CometFailureException(msg) {
  def snippetFailure: LiftRules.SnippetFailures.Value = 
    LiftRules.SnippetFailures.CometNotFound
}

