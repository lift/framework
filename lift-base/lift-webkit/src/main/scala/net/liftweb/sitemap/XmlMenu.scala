package net.liftweb.sitemap

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
      val text = scala.xml.NodeSeq.fromSeq((m \ "text" elements) flatMap {_.child.elements} collect)
      val link = net.liftweb.util.JSONParser.parse(m \ "link" text).get.asInstanceOf[List[Any]].map(_.asInstanceOf[String])
      Menu(Loc(name, link, text), apply(m \ "menu") : _*)
  }
}

