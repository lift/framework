/*
 * Copyright 2007-2011 WorldWide Conferencing, LLC
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


import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util._
import scala.xml._
import Helpers._

/**
 * Adds a resource id entity for each URI in order to control browser caching.
 * The rules of creating "unique" URI's are defined in LiftRules.attachResourceId function.
 * 
 * <pre>
 * &lt;lift:with-resource-id>
 *   &lt;link ... />
 *   &lt;script ... />
 * &lt;/lift:with-resource-id>
 * </pre>
 */
object WithResourceId extends DispatchSnippet {
  def dispatch: DispatchIt = {
    case _ =>  render
  }


  import Helpers._

  def render(xhtml: NodeSeq): NodeSeq = {
    xhtml flatMap (_ match {
     case e: Elem if e.label == "link" => 
        attrStr(e.attributes, "href").map { href =>
          e.copy(attributes =
            MetaData.update(e.attributes, 
                            e.scope,
                            new UnprefixedAttribute("href", LiftRules.attachResourceId(href), Null))
          )
        } openOr e
     case e: Elem if e.label == "script" => 
        attrStr(e.attributes, "src") map { src =>
          e.copy(attributes =
             MetaData.update(e.attributes, 
                             e.scope, 
                             new UnprefixedAttribute("src", LiftRules.attachResourceId(src), Null))
          )
        } openOr e
     case e => e
    })
  }


  private def attrStr(attrs: MetaData, attr: String): Box[String] = (attrs.get(attr) match {
    case None => Empty
    case Some(Nil) => Empty 
    case Some(x) => Full(x.toString)
  }) or (attrs.get(attr.toLowerCase) match {
    case None => Empty
    case Some(Nil) => Empty 
    case Some(x) => Full(x.toString)
  })
}

