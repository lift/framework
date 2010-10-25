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

import _root_.scala.util.parsing.combinator.{Parsers, ImplicitConversions}
import _root_.net.liftweb.common._

sealed trait CssSelector {
  def subNodes: Box[SubNode]
}

final case class IdSelector(id: String, subNodes: Box[SubNode]) extends 
  CssSelector

final case class ClassSelector(clss: String, subNodes: Box[SubNode]) extends
  CssSelector

final case class NameSelector(name: String, subNodes: Box[SubNode]) extends
  CssSelector

final case class AttrSelector(name: String, value: String, 
subNodes: Box[SubNode]) extends CssSelector

sealed trait SubNode

final case class KidsSubNode() extends SubNode
final case class AttrSubNode(attr: String) extends SubNode

/**
 * Parse a subset of CSS into the appropriate selector objects
 */
object CssSelectorParser extends Parsers with ImplicitConversions {
  private val cache = new LRUMap[String, CssSelector](25000)

  /**
   * Parse a String into a CSS Selector
   */
  def parse(toParse: String): Box[CssSelector] = synchronized {
    // this method is synchronized because the Parser combinator is not
    // thread safe, so we'll only parse one at a time, but given that most
    // of the selectors will be cached, it's not really a performance hit
    cache.get(toParse) or {
      internalParse(toParse).map {
        sel => {
          // cache the result
          cache(toParse) = sel
          sel
        }
      }
    }
  }

  import scala.util.parsing.input.CharSequenceReader


  type Elem = Char

  type UnitParser=Parser[Unit]

  private def internalParse(toParse: String): Box[CssSelector] = {
    val reader: Input = new CharSequenceReader(toParse, 0)
    topParser(reader) match {
      case Success(v, _) => Full(v)
      case x => Empty
    }
  }

  private implicit def str2chars(s: String): List[Char] = stringWrapper(s).toList

  private lazy val topParser: Parser[CssSelector] = {
    idMatch |
    classMatch |
    attrMatch
  }
    
  private lazy val idMatch: Parser[CssSelector] = '.' ~> id ~ opt(subNode) ^^ {
    case id ~ sn => IdSelector(id, sn)
  }

  private lazy val id: Parser[String] = letter ~ 
  rep(letter | number | '-' | '_' | ':' | '.') ^^ {
    case first ~ rest => (first :: rest).mkString
  }

  private def isLetter(c: Char): Boolean = c.isLetter

  private def isNumber(c: Char): Boolean = c.isDigit

    
  private lazy val letter: Parser[Char] = elem("letter", isLetter)
  private lazy val number: Parser[Char] = elem("number", isNumber)

  private lazy val classMatch: Parser[CssSelector] = 
    '#' ~> attrName ~ opt(subNode) ^^ {
      case cls ~ sn => ClassSelector(cls, sn)
    }

  private lazy val attrMatch: Parser[CssSelector] = 
    attrName ~ '=' ~ attrConst ~ opt(subNode) ^^ {
      case "name" ~ _ ~ const ~ sn => NameSelector(const, sn)
      case n ~ _  ~ v ~ sn => AttrSelector(n, v, sn)
    }

  private lazy val subNode: Parser[SubNode] = rep1(' ') ~> 
  ((opt('*') ~ '[' ~> attrName <~ ']' ^^ {
    name => AttrSubNode(name)
  }) | 
   '*' ^^ (a => KidsSubNode()))

  private lazy val attrName: Parser[String] = (letter | '_' | ':') ~
  rep(letter | number | '-' | '_' | ':' | '.') ^^ {
    case first ~ rest => (first :: rest).mkString
  }

  private lazy val attrConst: Parser[String] = {
    (('\'' ~> rep(elem("isValid", (c: Char) => {
      c != '\'' && c >= ' '
    })) <~ '\'') ^^ {
      case s => s.mkString
    }) |
    (('"' ~> rep(elem("isValid", (c: Char) => {
      c != '"' && c >= ' '
    })) <~ '"') ^^ {
      case s => s.mkString
    }) |
    (rep1(elem("isValid", (c: Char) => {
      c != '\'' && c != '"' && c > ' '
    })) ^^ {
      case s => s.mkString
    })
    
  }

  
}

}
}
