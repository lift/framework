/*
 * Copyright 2011 WorldWide Conferencing, LLC
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

import scala.util.parsing.combinator.{PackratParsers, Parsers, ImplicitConversions}
import scala.xml.{Elem, NodeSeq}

import net.liftweb.common._

sealed trait CssSelector {
  def subNodes: Box[SubNode]
  def withSubnode(sn: SubNode): CssSelector
}

final case class ElemSelector(elem: String, subNodes: Box[SubNode]) extends 
  CssSelector {
  def withSubnode(sn: SubNode): CssSelector = this.copy(subNodes = Full(sn))
}

final case class StarSelector(subNodes: Box[SubNode], singleDepth: Boolean) extends CssSelector {
  def withSubnode(sn: SubNode): CssSelector = this.copy(subNodes = Full(sn))
}

final case class IdSelector(id: String, subNodes: Box[SubNode]) extends 
  CssSelector {
  def withSubnode(sn: SubNode): CssSelector = this.copy(subNodes = Full(sn))
}

final case class ClassSelector(clss: String, subNodes: Box[SubNode]) extends
  CssSelector {
  def withSubnode(sn: SubNode): CssSelector = this.copy(subNodes = Full(sn))
}

final case class NameSelector(name: String, subNodes: Box[SubNode]) extends
  CssSelector {
  def withSubnode(sn: SubNode): CssSelector = this.copy(subNodes = Full(sn))
}

final case class EnclosedSelector(selector: CssSelector, kid: CssSelector) extends CssSelector {
  def subNodes: Box[SubNode] = Empty
  def withSubnode(sn: SubNode): CssSelector = this
}

final case class AttrSelector(name: String, value: String, 
subNodes: Box[SubNode]) extends CssSelector {
  def withSubnode(sn: SubNode): CssSelector = this.copy(subNodes = Full(sn))
}

sealed trait SubNode

object SubNode {
  def unapply(bind: CssBind): Option[Box[SubNode]] = 
    Some(bind.css.flatMap(_.subNodes))
}

sealed trait WithKids {
  def transform(original: NodeSeq, newNs: NodeSeq): NodeSeq
}

final case class KidsSubNode() extends SubNode with WithKids {
  def transform(original: NodeSeq, newNs: NodeSeq): NodeSeq = newNs
}

final case class PrependKidsSubNode() extends SubNode with WithKids {
  def transform(original: NodeSeq, newNs: NodeSeq): NodeSeq = newNs ++ original
}

final case object DontMergeAttributes extends SubNode {
  }

final case class SurroundKids() extends SubNode with WithKids {
  def transform(original: NodeSeq, newNs: NodeSeq): NodeSeq = {
    var changed = false

    val res: NodeSeq = newNs.flatMap{
      case e: Elem if !changed =>
        changed = true
        new Elem(e.prefix,
          e.label, e.attributes,
          e.scope, e.minimizeEmpty, e.child ++ original :_*)
      case x => x
    }

    if (changed) res else newNs ++ original
  }
}

final case class AppendKidsSubNode() extends SubNode with WithKids {
  def transform(original: NodeSeq, newNs: NodeSeq): NodeSeq = original ++ newNs
}


sealed trait AttributeRule

final case class AttrSubNode(attr: String) extends SubNode with AttributeRule
final case class AttrAppendSubNode(attr: String) extends SubNode with AttributeRule
final case class AttrRemoveSubNode(attr: String) extends SubNode with AttributeRule

final case class SelectThisNode(kids: Boolean) extends SubNode

/**
 * Parse a subset of CSS into the appropriate selector objects
 */
object CssSelectorParser extends PackratParsers with ImplicitConversions {
  private val cache = new LRUMap[String, CssSelector](25000)

  /**
   * Parse a String into a CSS Selector
   */
  def parse(_toParse: String): Box[CssSelector] = synchronized {
    // trim off leading and trailing spaces
    val toParse = _toParse.trim

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
      case x: NoSuccess => ParamFailure(x.msg, Empty, Empty, x)
    }
  }

  import scala.language.implicitConversions

  private implicit def str2chars(s: String): List[Char] = new scala.collection.immutable.WrappedString(s).toList

  private def fixAll(all: List[CssSelector], sn: Option[SubNode]): CssSelector = {
    (all, sn) match {
      // case (Nil, Some())
      case (r :: Nil, None) => r
      case (r :: Nil, Some(sn)) => r.withSubnode(sn)
      case (lst, None) => lst.reduceRight((b, a) => EnclosedSelector(b, a))
      case (lst, Some(sn)) => (lst.dropRight(1) ::: lst.takeRight(1).map(_.withSubnode(sn))).reduceRight((b, a) => EnclosedSelector(b, a))
    }
  }

  private val atEnd = Parser { in => if(in.atEnd) Success(CharSequenceReader.EofCh, in) else Failure("", in)}
  private lazy val topParser: Parser[CssSelector] =
    phrase(rep1((_idMatch | _dataNameMatch | _nameMatch | _classMatch | _attrMatch | _elemMatch |
      _colonMatch | _starMatch) <~ (rep1(' ') | atEnd)) ~ opt(subNode)) ^^ {
      case (one :: Nil) ~ sn => fixAll(List(one), sn)
    case all ~ None if all.takeRight(1).head == StarSelector(Empty, false) =>
      fixAll(all.dropRight(1), Some(KidsSubNode()))
    case all ~ sn => fixAll(all, sn)
  }

  private lazy val _colonMatch: Parser[CssSelector] =
  (':' ~> id) ^? {
    case "button" => AttrSelector("type", "button", Empty)
    case "checkbox" => AttrSelector("type", "checkbox", Empty)
    case "file" => AttrSelector("type", "file", Empty)
    case "password" => AttrSelector("type", "password", Empty)
    case "radio" => AttrSelector("type", "radio", Empty)
    case "reset" => AttrSelector("type", "reset", Empty)
    case "submit" => AttrSelector("type", "submit", Empty)
    case "text" => AttrSelector("type", "text", Empty)
  }

  private lazy val _idMatch: Parser[CssSelector] = '#' ~> id ^^ {
    case id => IdSelector(id, Empty)
  }

  private lazy val _nameMatch: Parser[CssSelector] = '@' ~> id ^^ {
    case name => NameSelector(name, Empty)
  }

  private lazy val _elemMatch: Parser[CssSelector] =  id ^^ {
    case elem => ElemSelector(elem, Empty)
  }

  private lazy val _starMatch: Parser[CssSelector] =  ('*' ^^ {
    case sn => StarSelector(Empty, false)
  }) | (
    '^' ^^ {
      case sn => StarSelector(Empty, true)
    }
    )

  private lazy val _dataNameMatch: Parser[CssSelector] = ';' ~> id ^^ {
    case name => AttrSelector("data-name", name, Empty)
  }


  private lazy val _classMatch: Parser[CssSelector] =
    '.' ~> attrName ^^ {
      case cls => ClassSelector(cls, Empty)
    }

  private lazy val _attrMatch: Parser[CssSelector] =
    attrName ~ '=' ~ attrConst ^^ {
      case "id" ~ _ ~ const  => IdSelector(const, Empty)
      case "name" ~ _ ~ const  => NameSelector(const, Empty)
      case n ~ _  ~ v => AttrSelector(n, v, Empty)
    }


  private lazy val id: Parser[String] = letter ~ 
  rep(letter | number | '-' | '_' | ':' | '.') ^^ {
    case first ~ rest => (first :: rest).mkString
  }

  private def isLetter(c: Char): Boolean = c.isLetter

  private def isNumber(c: Char): Boolean = c.isDigit

    
  private lazy val letter: Parser[Char] = elem("letter", isLetter)
  private lazy val number: Parser[Char] = elem("number", isNumber)


  private lazy val subNode: Parser[SubNode] = rep(' ') ~>
  ((opt('*') ~ '[' ~> attrName <~ '+' ~ ']' ^^ {
    name => AttrAppendSubNode(name)
  }) | 
  (opt('*') ~ '[' ~> attrName <~ '!' ~ ']' ^^ {
    name => AttrRemoveSubNode(name)
  }) |    (opt('*') ~ '[' ~> attrName <~ ']' ^^ {
     name => AttrSubNode(name)
   }) |

   ('!' ~ '!' ^^ (a => DontMergeAttributes)) |
   ('<' ~ '*' ~ '>') ^^ (a => SurroundKids()) |
   ('-' ~ '*' ^^ (a => PrependKidsSubNode())) |
   ('>' ~ '*' ^^ (a => PrependKidsSubNode())) |
   ('*' ~ '+' ^^ (a => AppendKidsSubNode())) |
   ('*' ~ '<' ^^ (a => AppendKidsSubNode())) |
   '*' ^^ (a => KidsSubNode()) |
   '^' ~ '*' ^^ (a => SelectThisNode(true)) |
   '^' ~ '^' ^^ (a => SelectThisNode(false)))

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

