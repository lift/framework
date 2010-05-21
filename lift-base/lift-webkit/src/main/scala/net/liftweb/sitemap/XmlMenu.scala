/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
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
package sitemap {

import scala.xml.NodeSeq
import Loc._

/**
 * The beginning of an experiment to provide a capability to define
 * the sitemap menu in xml. Currently pretty limited.
 * menu elements have a name attribute, and contain text and link
 * elements, and optionally multiple menu elemnts.
 * The contents of the text element is the menu display x(ht)ml,
 * and the contents of the link element is an array of
 * path components in JSON array syntax.
 *
 * @author nafg
 */
object XmlMenu {
  def apply(xml: scala.xml.NodeSeq): Seq[Menu] = for(node<-xml) yield node match {
    case m @ <menu>{ children @ _* }</menu> =>
      val name = m \ "@name" text
      val text = scala.xml.NodeSeq.fromSeq((m \ "text" elements) flatMap {_.child.elements} toSeq)
      val link = net.liftweb.util.JSONParser.parse(m \ "link" text).get.asInstanceOf[List[Any]].map(_.asInstanceOf[String])
      Menu(Loc(name, link, text), apply(m \ "menu") : _*)
  }
}

}
}
