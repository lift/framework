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
import net.liftweb.http.js._
import net.liftweb.util._

@deprecated("Use any of the ajax methods in SHtml instead.")
object A extends DispatchSnippet {

  def dispatch : DispatchIt = {
    case _ => render _
  }

  /**
   * Usage:
   *
   *   <pre name="code" class="xml"
   *   def a(func: () => JsCmd, body: NodeSeq, attrs: ElemAttr*): Elem = {
   *     val key = formFuncName
   *     addFunctionMap(key, ((a: List[String]) => func()))
   *     attrs.foldLeft(&lt;lift:a key={key}>{body}&lt;/lift:a>)(_ % _)
   *   }
   *   </pre>
   */
  def render(kids: NodeSeq) : NodeSeq = Elem(null, "a", addAjaxHREF(), TopScope, kids :_*)

  private def addAjaxHREF(): MetaData = {
    val ajax: JsExp = SHtml.makeAjaxCall(JE.Str(S.attr.~("key").map(_.text + "=true").getOrElse("")))

    new UnprefixedAttribute("onclick", Text(ajax.toJsCmd + "; return false;"),
                            new UnprefixedAttribute("href", Text("javascript://"),
                                                    S.currentAttrsToMetaData(name => name != "onclick" && name != "href" && name != "key")
                                                   ))
 }
}

