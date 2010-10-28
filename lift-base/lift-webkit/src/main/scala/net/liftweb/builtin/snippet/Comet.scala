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

object Comet extends DispatchSnippet with LazyLoggable {
  def dispatch : DispatchIt = {
    case _ => render _
  }

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
    val name: Box[String] = S.attr.~("name").map(_.text)


    (for {ctx <- S.session} yield {
      if (!ctx.stateful_?) 
        throw new StateInStatelessException(
          "Lift does not support Comet for stateless requests")

       try {
         ctx.findComet(theType, name, kids, S.attrsFlattenToMap).map(c =>

            (c.!?(26600L, AskRender)) match {
              case Full(AnswerRender(response, _, when, _)) if c.hasOuter =>
                buildSpan(Empty, c.buildSpan(when, response.inSpan) ++ response.outSpan, c, c.uniqueId+"_outer")

              case Full(AnswerRender(response, _, when, _)) =>
                c.buildSpan(when, response.inSpan)

              case _ => 
                throw new CometTimeoutException("type: "+theType+" name: "+name)
            }) openOr {
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

class CometTimeoutException(msg: String) extends SnippetFailureException(msg) {
  def snippetFailure: LiftRules.SnippetFailures.Value = 
    LiftRules.SnippetFailures.CometTimeout
}

class CometNotFoundException(msg: String) extends SnippetFailureException(msg) {
  def snippetFailure: LiftRules.SnippetFailures.Value = 
    LiftRules.SnippetFailures.CometNotFound
}



}
}
}
