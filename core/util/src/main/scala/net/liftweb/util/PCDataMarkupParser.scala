/*
 * Copyright 2006-2026 Lift Committers and Contributors
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
package util

import common._

import scala.xml._
import java.io.{ InputStream, StringReader }
import javax.xml.parsers.SAXParserFactory
import org.xml.sax.{ Attributes, InputSource, SAXParseException }
import org.xml.sax.ext.LexicalHandler
import org.xml.sax.helpers.DefaultHandler

/**
 * Utilities for simplifying use of named HTML symbols.
 */
object HtmlEntities {
  /**
   * A list of tuples which match named HTML symbols to their character codes.
   */
  val entList = List(("quot",34), ("amp",38), ("lt",60), ("gt",62), ("nbsp",160), ("iexcl",161), ("cent",162), ("pound",163), ("curren",164), ("yen",165),
                     ("euro",8364), ("brvbar",166), ("sect",167), ("uml",168), ("copy",169), ("ordf",170), ("laquo",171), ("shy",173), ("reg",174), ("trade",8482),
                     ("macr",175), ("deg",176), ("plusmn",177), ("sup2",178), ("sup3",179), ("acute",180), ("micro",181), ("para",182), ("middot",183), ("cedil",184),
                     ("sup1",185), ("ordm",186), ("raquo",187), ("frac14",188), ("frac12",189), ("frac34",190), ("iquest",191), ("times",215), ("divide",247),
                     ("Agrave",192), ("Aacute",193), ("Acirc",194), ("Atilde",195), ("Auml",196), ("Aring",197), ("AElig",198), ("Ccedil",199), ("Egrave",200),
                     ("Eacute",201), ("Ecirc",202), ("Euml",203), ("Igrave",204), ("Iacute",205), ("Icirc",206), ("Iuml",207), ("ETH",208), ("Ntilde",209),
                     ("Ograve",210), ("Oacute",211), ("Ocirc",212), ("Otilde",213), ("Ouml",214), ("Oslash",216), ("Ugrave",217), ("Uacute",218), ("Ucirc",219),
                     ("Uuml",220), ("Yacute",221), ("THORN",222), ("szlig",223), ("agrave",224), ("aacute",225), ("acirc",226), ("atilde",227), ("auml",228),
                     ("aring",229), ("aelig",230), ("ccedil",231), ("egrave",232), ("eacute",233), ("ecirc",234), ("euml",235), ("igrave",236), ("iacute",237),
                     ("icirc",238), ("iuml",239), ("eth",240), ("ntilde",241), ("ograve",242), ("oacute",243), ("ocirc",244), ("otilde",245), ("ouml",246),
                     ("oslash",248), ("ugrave",249), ("uacute",250), ("ucirc",251), ("uuml",252), ("yacute",253), ("thorn",254), ("yuml",255), ("OElig",338),
                     ("oelig",339), ("Scaron",352), ("scaron",353), ("Yuml",376), ("circ",710), ("ensp",8194), ("emsp",8195), ("zwnj",204), ("zwj",8205), ("lrm",8206),
                     ("rlm",8207), ("ndash",8211), ("mdash",8212), ("lsquo",8216), ("rsquo",8217), ("sbquo",8218), ("ldquo",8220), ("rdquo",8221), ("bdquo",8222),
                     ("dagger",8224), ("Dagger",8225), ("permil",8240), ("lsaquo",8249), ("rsaquo",8250), ("fnof",402), ("bull",8226), ("hellip",8230), ("prime",8242),
                     ("Prime",8243), ("oline",8254), ("frasl",8260), ("weierp",8472), ("image",8465), ("real",8476), ("alefsym",8501), ("larr",8592), ("uarr",8593),
                     ("rarr",8594), ("darr",8495), ("harr",8596), ("crarr",8629), ("lArr",8656), ("uArr",8657), ("rArr",8658), ("dArr",8659), ("hArr",8660),
                     ("forall",8704), ("part",8706), ("exist",8707), ("empty",8709), ("nabla",8711), ("isin",8712), ("notin",8713), ("ni",8715), ("prod",8719),
                     ("sum",8721), ("minus",8722), ("lowast",8727), ("radic",8730), ("prop",8733), ("infin",8734), ("ang",8736), ("and",8743), ("or",8744),
                     ("cap",8745), ("cup",8746), ("int",8747), ("there4",8756), ("sim",8764), ("cong",8773), ("asymp",8776), ("ne",8800), ("equiv",8801), ("le",8804),
                     ("ge",8805), ("sub",8834), ("sup",8835), ("nsub",8836), ("sube",8838), ("supe",8839), ("oplus",8853), ("otimes",8855), ("perp",8869), ("sdot",8901),
                     ("lceil",8968), ("rceil",8969), ("lfloor",8970), ("rfloor",8971), ("lang",9001), ("rang",9002), ("loz",9674), ("spades",9824), ("clubs",9827),
                     ("hearts",9829), ("diams",9830), ("Alpha",913), ("Beta",914), ("Gamma",915), ("Delta",916), ("Epsilon",917), ("Zeta",918), ("Eta",919),
                     ("Theta",920), ("Iota",921), ("Kappa",922), ("Lambda",923), ("Mu",924), ("Nu",925), ("Xi",926), ("Omicron",927), ("Pi",928), ("Rho",929),
                     ("Sigma",931), ("Tau",932), ("Upsilon",933), ("Phi",934), ("Chi",935), ("Psi",936), ("Omega",937), ("alpha",945), ("beta",946), ("gamma",947),
                     ("delta",948), ("epsilon",949), ("zeta",950), ("eta",951), ("theta",952), ("iota",953), ("kappa",954), ("lambda",955), ("mu",956), ("nu",957),
                     ("xi",958), ("omicron",959), ("pi",960), ("rho",961), ("sigmaf",962), ("sigma",963), ("tau",964), ("upsilon",965), ("phi",966), ("chi",967),
                     ("psi",968), ("omega",969), ("thetasym",977), ("upsih",978), ("piv",982))

  val entMap: Map[String, Char] = Map.empty ++ entList.map{ case (name, value) => (name, value.toChar)}

  val revMap: Map[Char, String] = Map(entList.map{ case (name, value) => (value.toChar, name)} :_*)

}

object PCDataXmlParser {
  import Helpers._

  def apply(in: InputStream): Box[NodeSeq] =
    for {
      ba  <- tryo(Helpers.readWholeStream(in))
      ret <- apply(new String(ba, "UTF-8"))
    } yield ret

  def apply(in: String): Box[NodeSeq] = {
    var pos = 0
    val len = in.length
    def moveToLT(): Unit =
      while (pos < len && in.charAt(pos) != '<') pos += 1

    moveToLT()

    // scan past <? ... ?>
    if (pos + 1 < len && in.charAt(pos + 1) == '?') {
      pos += 1
      moveToLT()
    }

    // scan past <!DOCTYPE ....>
    if (pos + 1 < len && in.charAt(pos + 1) == '!') {
      pos += 1
      moveToLT()
    }

    parseXml(preprocessEntities(in.substring(pos)))
  }

  private def parseXml(content: String): Box[NodeSeq] =
    tryo {
      // namespace-aware=false: undeclared prefixes (e.g. lift:) are allowed; xmlns
      // attributes are surfaced as regular attributes so we can build NamespaceBinding
      // manually, matching the behaviour of the previous MarkupParser-based impl.
      val spf = SAXParserFactory.newInstance()
      spf.setNamespaceAware(false)
      val saxParser = spf.newSAXParser()
      val xmlReader = saxParser.getXMLReader
      val builder   = new LiftSaxTreeBuilder
      xmlReader.setContentHandler(builder)
      xmlReader.setErrorHandler(builder)
      xmlReader.setProperty("http://xml.org/sax/properties/lexical-handler", builder)
      xmlReader.parse(new InputSource(new StringReader(content)))
      NodeSeq.fromSeq(Seq(builder.result))
    }

  /**
   * Replace named HTML entity references with numeric character references so
   * that standard XML parsers can handle them without a DTD.  CDATA sections
   * are passed through verbatim since `&` inside them is not an entity ref.
   */
  private def preprocessEntities(s: String): String = {
    val sb  = new java.lang.StringBuilder(s.length)
    var i   = 0
    val len = s.length
    while (i < len) {
      val c = s.charAt(i)
      if (c == '<' && i + 8 < len &&
          s.charAt(i+1) == '!' && s.charAt(i+2) == '[' &&
          s.charAt(i+3) == 'C' && s.charAt(i+4) == 'D' &&
          s.charAt(i+5) == 'A' && s.charAt(i+6) == 'T' &&
          s.charAt(i+7) == 'A' && s.charAt(i+8) == '[') {
        // copy CDATA section verbatim
        val end = s.indexOf("]]>", i + 9)
        if (end >= 0) { sb.append(s, i, end + 3); i = end + 3 }
        else          { sb.append(c); i += 1 }
      } else if (c == '&' && i + 1 < len && s.charAt(i + 1) != '#') {
        val semi = s.indexOf(';', i + 1)
        if (semi > i && semi - i <= 12) {
          val name = s.substring(i + 1, semi)
          if (name.nonEmpty && name.forall(c => c.isLetter || c.isDigit)) {
            HtmlEntities.entMap.get(name) match {
              case Some(chr) =>
                sb.append("&#"); sb.append(chr.toInt); sb.append(';')
                i = semi + 1
              case None =>
                sb.append(c); i += 1
            }
          } else { sb.append(c); i += 1 }
        } else { sb.append(c); i += 1 }
      } else { sb.append(c); i += 1 }
    }
    sb.toString
  }

  /**
   * SAX ContentHandler + LexicalHandler that builds a scala-xml node tree.
   * CDATA sections become Lift [[PCData]] nodes instead of text nodes.
   *
   * Requires namespace-aware=false on the factory so that undeclared namespace
   * prefixes (e.g. `lift:`) are accepted and `xmlns:*` attributes are surfaced
   * in the attribute list for manual NamespaceBinding construction.
   */
  private class LiftSaxTreeBuilder extends DefaultHandler with LexicalHandler {
    // (prefix, label, attributes, scope, children-accumulated-in-reverse)
    private var stack: List[(String, String, MetaData, NamespaceBinding, List[Node])] = Nil
    private val textBuf  = new java.lang.StringBuilder
    private var inCDATA  = false
    private val cdataBuf = new java.lang.StringBuilder

    var result: Node = _

    private def flushText(): Unit =
      if (textBuf.length > 0) {
        val t = Text(textBuf.toString)
        textBuf.setLength(0)
        pushChild(t)
      }

    private def pushChild(n: Node): Unit =
      stack match {
        case (pre, lbl, attrs, scope, children) :: rest =>
          stack = (pre, lbl, attrs, scope, n :: children) :: rest
        case Nil =>
      }

    override def startElement(uri: String, localName: String, qName: String,
                              attrs: Attributes): Unit = {
      flushText()

      // Start from the parent's scope and extend with this element's xmlns declarations.
      var scope: NamespaceBinding = stack.headOption.map(_._4).getOrElse(TopScope)
      var metadata: MetaData      = Null

      for (i <- (attrs.getLength - 1) to 0 by -1) {
        val aqn = attrs.getQName(i)
        val v   = attrs.getValue(i)
        if (aqn == "xmlns") {
          scope = new NamespaceBinding(null, v, scope)
        } else if (aqn.startsWith("xmlns:")) {
          scope = new NamespaceBinding(aqn.substring(6), v, scope)
        } else {
          val ai = aqn.indexOf(':')
          metadata =
            if (ai > 0)
              new PrefixedAttribute(aqn.substring(0, ai), aqn.substring(ai + 1), Text(v), metadata)
            else
              new UnprefixedAttribute(aqn, Text(v), metadata)
        }
      }

      val ci = qName.indexOf(':')
      val (pre, label) =
        if (ci > 0) (qName.substring(0, ci), qName.substring(ci + 1))
        else        (null, qName)

      stack = (pre, label, metadata, scope, Nil) :: stack
    }

    override def endElement(uri: String, localName: String, qName: String): Unit = {
      flushText()
      stack match {
        case (pre, label, metadata, scope, children) :: rest =>
          stack = rest
          val elem = Elem(pre, label, metadata, scope, minimizeEmpty = true, children.reverse: _*)
          rest match {
            case Nil =>
              result = elem
            case (pp, pl, pm, ps, pc) :: rr =>
              stack = (pp, pl, pm, ps, elem :: pc) :: rr
          }
        case Nil =>
      }
    }

    override def characters(ch: Array[Char], start: Int, length: Int): Unit =
      if (inCDATA) cdataBuf.append(ch, start, length)
      else         textBuf.append(ch, start, length)

    override def startCDATA(): Unit = {
      flushText()
      inCDATA = true
      cdataBuf.setLength(0)
    }

    override def endCDATA(): Unit = {
      inCDATA = false
      pushChild(PCData(cdataBuf.toString))
      cdataBuf.setLength(0)
    }

    override def fatalError(e: SAXParseException): Unit = throw e

    // LexicalHandler no-ops
    override def startDTD(name: String, publicId: String, systemId: String): Unit = {}
    override def endDTD(): Unit = {}
    override def startEntity(name: String): Unit = {}
    override def endEntity(name: String): Unit = {}
    override def comment(ch: Array[Char], start: Int, length: Int): Unit = {}
  }
}

case class PCData(_data: String) extends Atom[String](_data) {
  /* The following code is a derivative work of scala.xml.Text */
  if (null == data)
  throw new java.lang.NullPointerException("tried to construct Text with null")

  final override def equals(x: Any) = x match {
    case s:String  => s.equals(data.toString())
    case s:Atom[_] => data == s.data
    case _ => false
  }

  /** Returns text, with some characters escaped according to the XML
   *  specification.
   *
   *  @param  sb ...
   *  @return ...
   */
  override def buildString(sb: StringBuilder) = {
    sb.append("<![CDATA[")
    sb.append(data)
    sb.append("]]>")
  }
}

object AltXML {
  val ieBadTags: Set[String] = Set("br", "hr")

  val inlineTags: Set[String] = Set("base", "meta", "link", "hr", "br",
                                    "param", "img", "area", "input", "col" )

  def toXML(n: Node, stripComment: Boolean, convertAmp: Boolean,
            legacyIeCompatibilityMode: Boolean): String = {
    val sb = new StringBuilder(50000)
    toXML(n, TopScope, sb, stripComment, convertAmp, legacyIeCompatibilityMode)
    sb.toString()
  }

  def toXML(n: Node, stripComment: Boolean, convertAmp: Boolean): String = {
    val sb = new StringBuilder(50000)
    toXML(n, TopScope, sb, stripComment, convertAmp)
    sb.toString()
  }

  /**
   * Appends a tree to the given stringbuffer within given namespace scope.
   *
   * @param n            the node
   * @param pscope       the parent scope
   * @param sb           stringbuffer to append to
   * @param stripComment if true, strip comments
   */
  def toXML(x: Node, pscope: NamespaceBinding, sb: StringBuilder,
            stripComment: Boolean, convertAmp: Boolean): Unit =
  x match {
    case Text(str) => escape(str, sb, !convertAmp)

    case c: PCData => c.buildString(sb)

    case c: scala.xml.PCData => c.buildString(sb)

    case up: Unparsed => up.buildString(sb)

    case a: Atom[_] if a.getClass eq classOf[Atom[_]] =>
      escape(a.data.toString, sb, !convertAmp)

    case c: Comment if !stripComment =>
      c.buildString(sb)

    case er: EntityRef if convertAmp =>
      HtmlEntities.entMap.get(er.entityName) match {
        case Some(chr) if chr.toInt >= 128 => sb.append(chr)
        case _ => er.buildString(sb)
      }

    case x: SpecialNode =>
      x.buildString(sb)

    case g: Group =>
      for (c <- g.nodes)
      toXML(c, x.scope, sb, stripComment, convertAmp)

    case e: Elem if ((e.child eq null) || e.child.isEmpty) =>
      sb.append('<')
      e.nameToString(sb)
      if (e.attributes ne null) e.attributes.buildString(sb)
      e.scope.buildString(sb, pscope)
      sb.append(" />")

    case e: Elem =>
      // print tag with namespace declarations
      sb.append('<')
      e.nameToString(sb)
      if (e.attributes ne null) e.attributes.buildString(sb)
      e.scope.buildString(sb, pscope)
      sb.append('>')
      sequenceToXML(e.child, e.scope, sb, stripComment, convertAmp)
      sb.append("</")
      e.nameToString(sb)
      sb.append('>')

    case _ => // dunno what it is, but ignore it
  }

  private def escape(str: String, sb: StringBuilder, reverse: Boolean): Unit =  {
    val len = str.length
    var pos = 0
    while (pos < len) {
      str.charAt(pos) match {
        case '<' => sb.append("&lt;")
        case '>' => sb.append("&gt;")
        case '&' => sb.append("&amp;")
        case '"' => sb.append("&quot;")
        case '\n' => sb.append('\n')
        case '\r' => sb.append('\r')
        case '\t' => sb.append('\t')
        case c   =>
          if (reverse) {
            HtmlEntities.revMap.get(c) match {
              case Some(str) =>
                sb.append('&')
                sb.append(str)
                sb.append(';')
              case _ =>
                if (c >= ' ' && c != '\u0085' && !(c >= '\u007f' && c <= '\u0095')) sb.append(c)
            }
          } else
          if (c >= ' ' && c != '\u0085' && !(c >= '\u007f' && c <= '\u0095')) sb.append(c)
      }

      pos += 1
    }
  }

  /**
   * Appends a tree to the given stringbuffer within given namespace scope.
   *
   * @param n            the node
   * @param pscope       the parent scope
   * @param sb           stringbuffer to append to
   * @param stripComment if true, strip comments
   */
  def toXML(x: Node, pscope: NamespaceBinding, sb: StringBuilder,
            stripComment: Boolean, convertAmp: Boolean,
            legacyIeCompatibilityMode: Boolean): Unit =
  x match {
    case Text(str) => escape(str, sb, !convertAmp)

    case c: PCData => c.buildString(sb)

    case c: scala.xml.PCData => c.buildString(sb)

    case up: Unparsed => up.buildString(sb)

    case a: Atom[_] if a.getClass eq classOf[Atom[_]] =>
      escape(a.data.toString, sb, !convertAmp)

    case c: Comment if !stripComment =>
      c.buildString(sb)

    case er: EntityRef if convertAmp =>
      HtmlEntities.entMap.get(er.entityName) match {
        case Some(chr) if chr.toInt >= 128 => sb.append(chr)
        case _ => er.buildString(sb)
      }

    case x: SpecialNode =>
      x.buildString(sb)

    case g: Group =>
      for (c <- g.nodes)
      toXML(c, x.scope, sb, stripComment, convertAmp, legacyIeCompatibilityMode)

    case e: Elem if !legacyIeCompatibilityMode && ((e.child eq null) || e.child.isEmpty)
      && inlineTags.contains(e.label) =>
      sb.append('<')
      e.nameToString(sb)
      if (e.attributes ne null) e.attributes.buildString(sb)
      e.scope.buildString(sb, pscope)
      sb.append(" />")

    case e: Elem if legacyIeCompatibilityMode && ((e.child eq null) || e.child.isEmpty) &&
      ieBadTags.contains(e.label) =>
      sb.append('<')
      e.nameToString(sb)
      if (e.attributes ne null) e.attributes.buildString(sb)
      e.scope.buildString(sb, pscope)
      sb.append("/>")

    case e: Elem =>
      // print tag with namespace declarations
      sb.append('<')
      e.nameToString(sb)
      if (e.attributes ne null) e.attributes.buildString(sb)
      e.scope.buildString(sb, pscope)
      sb.append('>')
      sequenceToXML(e.child, e.scope, sb, stripComment, convertAmp, legacyIeCompatibilityMode)
      sb.append("</")
      e.nameToString(sb)
      sb.append('>')

    case _ => // dunno what it is, but ignore it
  }


  /**
   * @param children     ...
   * @param pscope       ...
   * @param sb           ...
   * @param stripComment ...
   */
  def sequenceToXML(children: Seq[Node], pscope: NamespaceBinding,
                    sb: StringBuilder, stripComment: Boolean,
                    convertAmp: Boolean, legacyIeCompatibilityMode: Boolean): Unit = {
    val it = children.iterator
    while (it.hasNext) {
      toXML(it.next(), pscope, sb, stripComment, convertAmp, legacyIeCompatibilityMode)
    }
  }

  /**
   * @param children     ...
   * @param pscope       ...
   * @param sb           ...
   * @param stripComment ...
   */
  def sequenceToXML(children: Seq[Node], pscope: NamespaceBinding,
                    sb: StringBuilder, stripComment: Boolean,
                    convertAmp: Boolean): Unit = {
    val it = children.iterator
    while (it.hasNext) {
      toXML(it.next(), pscope, sb, stripComment, convertAmp)
    }
  }

}
