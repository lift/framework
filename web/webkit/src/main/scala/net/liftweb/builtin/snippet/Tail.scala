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

import http._
import util._
import scala.xml._



object Tail extends DispatchSnippet {
   def dispatch: DispatchIt = {
     case _ => render _
   }

   def render(xhtml: NodeSeq) : NodeSeq = <tail>{xhtml}</tail>
}

/**
 * The 'head' snippet.  Use this snippet to move
 * a chunk of 
 */
object Head extends DispatchSnippet {
  lazy val valid = Set("title", "base",
                       "link", "meta", "style",
                       "script")

   def dispatch: DispatchIt = {
     case _ => render _
   }

   def render(_xhtml: NodeSeq) : NodeSeq = {
     def validHeadTagsOnly(in: NodeSeq): NodeSeq = 
       in  flatMap {
         case Group(ns) => validHeadTagsOnly(ns)
         case e: Elem if (null eq e.prefix) && valid.contains(e.label) => {
           new Elem(e.prefix,
                    e.label,
                    e.attributes,
                    e.scope,
                    e.minimizeEmpty,
                    validHeadTagsOnly(e.child) :_*)
         }
         case e: Elem if (null eq e.prefix) => NodeSeq.Empty
         case x => x
       }
     
       val xhtml = validHeadTagsOnly(_xhtml)

     <head>{
       if ((S.attr("withResourceId") or S.attr("withresourceid")).filter(Helpers.toBoolean).isDefined) {
         WithResourceId.render(xhtml)
       } else {
         xhtml
       }
     }</head>
   }
}
