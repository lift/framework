/*
 * Copyright 2010 WorldWide Conferencing, LLC
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
package util {

import _root_.net.liftweb.common._
import _root_.scala.xml._
import parsing._
import _root_.java.io._

import _root_.nu.validator.htmlparser._
import sax.HtmlParser

import _root_.org.xml.sax.InputSource

object Html5 extends Html5Parser with Html5Writer

trait Html5Writer {
  /**
   * Write the attributes in HTML5 valid format
   * @param m the attributes
   * @param writer the place to write the attribute
   */
  protected def writeAttributes(m: MetaData, writer: Writer) {
    m match {
      case null => 
      case Null =>
      case up: UnprefixedAttribute => {
        writer.append(' ')
        writer.append(up.key)
        val v = up.value
        if (!v.isEmpty) {
          writer.append("=\"")
          val str = v.text
          var pos = 0
          val len = str.length
          while (pos < len) {
            str.charAt(pos) match {
              case '"' => writer.append("U+0022")
               case '<' => writer.append("U+003C")
              case c if c >= ' ' && c.toInt <= 127 => writer.append(c)
              case c if c == '\u0085' =>
              case c => {
                val str = Integer.toHexString(c)
                writer.append("U+")
                writer.append("0000".substring(str.length))
                writer.append(str)
              }
            }

            pos += 1
          }

          writer.append('"')          
        }

        writeAttributes(up.next, writer)        
      }

      case pa: PrefixedAttribute => {
        writer.append(' ')
        writer.append(pa.pre)
        writer.append(':')
        writer.append(pa.key)
        val v = pa.value
        if (!v.isEmpty) {
          writer.append("=\"")
          val str = v.text
          var pos = 0
          val len = str.length
          while (pos < len) {
            str.charAt(pos) match {
              case '"' => writer.append("U+0022")
               case '<' => writer.append("U+003C")
              case c if c >= ' ' && c.toInt <= 127 => writer.append(c)
              case c if c == '\u0085' =>
              case c => {
                val str = Integer.toHexString(c)
                writer.append("U+")
                writer.append("0000".substring(str.length))
                writer.append(str)
              }
            }

            pos += 1
          }

          writer.append('"')          
        }

        writeAttributes(pa.next, writer)        
      }
        
      case x => writeAttributes(x.next, writer)
    }
  }

  /**
   * Escape text data
   * @param str the String to escape
   * @param the place to send the escaped characters
   */
  protected def escape(str: String, sb: Writer) {
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
          HtmlEntities.revMap.get(c) match {
            case Some(str) => {
              sb.append('&')
              sb.append(str)
              sb.append(';')
            }
            case _ => 
              if (c >= ' ' && 
                  c != '\u0085' && 
                  !(c >= '\u007f' && c <= '\u0095')) sb.append(c)
          }
      }

      pos += 1
    }
  }

  /**
   * Convert a Node to a properly encoded Html5 String
   */
  def toString(x: Node): String = {
    val sr = new StringWriter()
    write(x, sr, false)
    sr.toString()
  }

  /**
   * Write the Node out as valid HTML5
   *
   * @param x the node to write out
   * @param writer the place to send the node
   * @param stripComment should comments be stripped from output?
   */
  def write(x: Node, writer: Writer, stripComment: Boolean): Unit = {
    x match {
      case Text(str) => escape(str, writer)

      case PCData(data) => {
        writer.append("<![CDATA[")
        writer.append(data)
        writer.append("]]>")
      }

      case scala.xml.PCData(data) => {
        writer.append("<![CDATA[")
        writer.append(data)
        writer.append("]]>")
      }

      case Unparsed(data) => writer.append(data)

      case a: Atom[_] if a.getClass eq classOf[Atom[_]] =>
        escape(a.data.toString, writer)
      
      case Comment(comment) if !stripComment => {
        writer.append("<!--")
        writer.append(comment)
        writer.append("-->")
      }
      
      case er: EntityRef =>
        HtmlEntities.entMap.get(er.entityName) match {
          case Some(chr) if chr.toInt >= 128 => writer.append(chr)
          case _ => {
            val sb = new StringBuilder()
            er.toString(sb)
            writer.append(sb)
          }
        }
      
      case x: SpecialNode => {
        val sb = new StringBuilder()
        x.toString(sb)
        writer.append(sb)
      }
      
      case g: Group =>
        for (c <- g.nodes)
          write(c, writer, stripComment)
      
      case e: Elem if (null eq e.prefix) && 
      Html5Constants.nonReplaceable_?(e.label) => {
        writer.append('<')
        writer.append(e.label)
        writeAttributes(e.attributes, writer)
        writer.append(">")
        e.child match {
          case null => 
          case seq => seq.foreach {
            case Text(str) => writer.append(str)
            case pc: PCData => {
              val sb = new StringBuilder()
              pc.toString(sb)
              writer.append(sb)
            }
            case pc: scala.xml.PCData => {
              val sb = new StringBuilder()
              pc.toString(sb)
              writer.append(sb)
            }
            case Unparsed(text) => writer.append(text)
            case a: Atom[_] if a.getClass eq classOf[Atom[_]] =>
              writer.append(a.data.toString)

            case _ =>
          }
        }
        writer.append(e.text)
        writer.append("</")
        writer.append(e.label)
        writer.append('>')
      }
      
      case e: Elem if (null eq e.prefix) && 
      Html5Constants.voidTag_?(e.label) => {
        writer.append('<')
        writer.append(e.label)
        writeAttributes(e.attributes, writer)
        writer.append(">")
      }
      

      case e: Elem if ((e.child eq null) || e.child.isEmpty) => {
        writer.append('<')
        if (null ne e.prefix) {
          writer.append(e.prefix)
          writer.append(':')
        }
        writer.append(e.label)
        writeAttributes(e.attributes, writer)
        writer.append(" />")
      }
      
      case e: Elem => {
        writer.append('<')
        if (null ne e.prefix) {
          writer.append(e.prefix)
          writer.append(':')
        }
        writer.append(e.label)
        writeAttributes(e.attributes, writer)
        writer.append(">")
        e.child.foreach(write(_, writer, stripComment))
        writer.append("</")
        if (null ne e.prefix) {
          writer.append(e.prefix)
          writer.append(':')
        }
        writer.append(e.label)
        writer.append('>')
      }
      
      case _ => // dunno what it is, but ignore it
    }
  }
}

object Html5Constants {
  val voidTags: Set[String] = Set("area",
                                  "base",
                                  "br",
                                  "col",
                                  "command",
                                  "embed",
                                  "hr",
                                  "img",
                                  "input",
                                  "keygen",
                                  "link",
                                  "meta",
                                  "param",
                                  "source",
                                  "wbr")

  /**
   * Is the tag a void tag?
   */
  def voidTag_?(t: String): Boolean = voidTags.contains(t.toLowerCase)

  /**
   * Is the tag a non-replaceable tag?
   */
  def nonReplaceable_?(t: String): Boolean =
    (t equalsIgnoreCase "script") ||
  (t equalsIgnoreCase "style")
}


/**
 * A utility that supports parsing of HTML5 file.
 * The Parser hooks up nu.validator.htmlparser
 * to 
 */
trait Html5Parser {
  /**
   * Parse an InputStream as HTML5.  A Full(Elem)
   * will be returned on successful parsing, otherwise
   * a Failure.
   */
  def parse(in: InputStream): Box[Elem] = {
    Helpers.tryo {
      val hp = new HtmlParser()
      val saxer = new NoBindingFactoryAdapter 

      saxer.scopeStack.push(TopScope)
      hp.setContentHandler(saxer)
      hp.parse(new InputSource(in))
      saxer.scopeStack.pop
      
      in.close()
      saxer.rootElem match {
        case null => Empty
        case e: Elem => Full(e)
        case _ => Empty
      }
    }.flatMap(a => a)
  }

  /**
   * Parse an InputStream as HTML5.  A Full(Elem)
   * will be returned on successful parsing, otherwise
   * a Failure.
   */
  def parse(str: String): Box[Elem] = 
    parse(new ByteArrayInputStream(str.getBytes("UTF-8")))
}

}
}
