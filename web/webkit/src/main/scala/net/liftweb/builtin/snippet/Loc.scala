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
import net.liftweb.http._
import net.liftweb.common._

/**
 * The Loc snippet is used to render localized content.
 *
 * Lookup resource with the id specified with the locid attribute. Will also try to
 * use the render method or the snippet body as locid if not specified.
 *
 * So these are equivalent:
 * 
 *   <lift:Loc locid="myid"/>
 *   <lift.Loc.myid/>
 *   <lift:Loc>myid</lift:Loc>
 *
 * There's a special case with the "i" method. It will use the text content as the locid, and will
 * replace the child node with the localized content instead of the current element.
 *
 * This is especially useful together with designer friendly snippet markup:
 *
 * <h2 class="lift:Loc.i">Some header</h2>
 *
 * If the locid "Some header" for the current locale is e.g "En overskrift", this will render
 *
 * <h2>En overskrift</h2>
 *
 * If the locid is not found, it will just render
 * 
 * <h2>Some header</h2>
 * 
 */
object Loc extends DispatchSnippet {
  def dispatch : DispatchIt = {
    case "i" => ns => i(ns)
    case s => ns => render(s, ns)
  }

  def i(ns: NodeSeq): NodeSeq = {
    ns match {
      case e: Elem => e.copy(child = S.loc(ns.text, Text(ns.text)))
      case _ => render("i", ns)
    }
  }
     
  def render(locId: String, kids: NodeSeq) : NodeSeq = {
    S.loc(locId) openOr 
    (S.attr("locid") match {
      case Full(id) => S.loc(id, kids)
      case _ => S.loc(kids.text, kids)
    })
  }

}
