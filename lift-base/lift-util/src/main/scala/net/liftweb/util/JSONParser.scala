/*
 * Copyright 2008-2010 WorldWide Conferencing, LLC
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
import Helpers._
import common._

object JSONParser extends SafeSeqParser with ImplicitConversions {
  implicit def strToInput(in: String): Input = new _root_.scala.util.parsing.input.CharArrayReader(in.toCharArray)
  type Elem = Char

    /**
     * Parse a JSON structure.  A JSON structure is a JSON array or JSON object
     * (which may contain other arrays or objects).  See http://json.org for
     * details about the JSON format.
     *
     * In the parsed results, each JSON object is returned as a
     * <code>Map[String,Any]</code> that maps JSON object names to JSON
     * values.  Each JSON array is returned as and a <code>List[Any]</code>
     * that contains JSON objects and/or JSON arrays.
     *
     * @return a <code>Box</code> containig the root JSON object or JSON array.
     */
  def parse(in: String): Box[Any] = theValue(in) match {
    case Success(v, _) => Full(v)
    case x => Empty
  }

  def unapply(in: Any): Option[Any] = in match {
    case s: String => parse(s)
    case _ => None
  }

  lazy val whitespace = elem(' ') | elem('\t') | elem('\n') | elem('\r')

  lazy val spaces = rep(whitespace)

  lazy val jsonObject: Parser[Map[String, Any]] = ( spaces ~ '{' ~ spaces ~> members <~ spaces ~ '}' ~ spaces ^^ {case xs =>
        Map(xs :_*)
    } )  |
  spaces ~'{' ~ spaces ~ '}' ~ spaces  ^^ {case _ => Map.empty}

  lazy val members = rep1sep(pair, spaces ~ ',' ~ spaces)

  lazy val pair: Parser[(String, Any)] = (string | pairId) ~ spaces ~ ':' ~ spaces ~ theValue ^^ {case s ~ _ ~ _ ~ _ ~ v => (s,v)}

  def pairChar(in: Char): Boolean = in.isLetter || in.isDigit || in == '_'

  lazy val pairId: Parser[String] = rep1(elem("pairChar", pairChar)) ^^ {case s => s.mkString}

  lazy val string: Parser[String] = ('\'' ~> rep(not('\'') ~> bchar) <~ '\'' ^^ {case xs => xs.mkString}) |
  ('"' ~> rep(not('"') ~> achar) <~ '"' ^^ {case xs => xs.mkString})

  lazy val achar = ('\\' ~> ('"' ^^ {case _ => '"'} |
                             '\\' ^^ {case _ => '\\'} |
                             '/' ^^ {case _ => '/'} |
                             'b' ^^ {case _ => '\b'} |
                             'n' ^^ {case _ => '\n'} |
                             'r' ^^ {case _ => '\r'} |
                             't' ^^ {case _ => '\t'} |
                             'u' ~> repN(4, hexDigit) ^^ {case dg => Integer.parseInt(dg.mkString, 16).toChar})) |
  (elem("any char", c => c != '"' && c >= ' '))

  lazy val bchar = ('\\' ~> ('"' ^^ {case _ => '"'} |
                             '\\' ^^ {case _ => '\\'} |
                             '/' ^^ {case _ => '/'} |
                             'b' ^^ {case _ => '\b'} |
                             'n' ^^ {case _ => '\n'} |
                             'r' ^^ {case _ => '\r'} |
                             't' ^^ {case _ => '\t'} |
                             'u' ~> repN(4, hexDigit) ^^ {case dg => Integer.parseInt(dg.mkString, 16).toChar})) |
  (elem("any char", c => c != '\'' && c >= ' '))

  lazy val number: Parser[Double] =
  ('-' ~> intFracExp ^^ (d => d * -1D)) |
  intFracExp |
  ('-' ~> intFrac ^^ (_ * -1D)) |
  intFrac |
  ('-' ~> intExp ^^ (_ * -1D)) |
  intExp |
  manyCharInt // (anInt ^^ {case n => n.doubleValue})

  lazy val exp: Parser[Int] = e ~ digits ^^ {case x ~ d => d.mkString.toInt * x}

  lazy val e = ('e' ~ '-' ^^ {case _ => -1}) | ('e' ~ '+' ^^ {case _ => 1}) | ('e' ^^ {case _ => 1}) |
  ('E' ~ '-' ^^ {case _ => -1}) | ('E' ~ '+' ^^ {case _ => 1}) | ('E' ^^ {case _ => 1})

  lazy val intFracExp: Parser[Double] = anInt ~ frac ~ exp ^^ {case i ~ f ~ exp => ((i.toString+"."+f+"e"+exp).toDouble)}

  lazy val intFrac: Parser[Double] = anInt ~ frac ^^ {case i ~ f => ((i.toString+"."+f).toDouble)}

  lazy val intExp = anInt ~ exp ^^ {case i ~ e => ((i.toString+"e"+e).toDouble)}

  lazy val anInt: Parser[Long] = (digit19 ~ digits ^? {case x ~ xs if xs.length < 12 => (x :: xs).mkString("").toLong}) |
  (digit ^^ {case x => x.toString.toLong}) |
  ('-' ~> digit19 ~ digits ^? {case x ~ xs if xs.length < 12 => ((x :: xs).mkString("").toLong * -1L)}) |
  ('-' ~> digit ^^ {case x => x.toString.toLong * -1L})

  lazy val manyCharInt: Parser[Double] = (digit19 ~ digits ^^ {case x ~ xs => (x :: xs).mkString.toDouble}) |
  (digit ^^ {case x => x.toString.toDouble}) |
  ('-' ~> digit19 ~ digits ^^ {case  x ~ xs => ((x :: xs).mkString.toDouble * -1d)}) |
  ('-' ~> digit ^^ {case x => x.toString.toDouble * -1d})

  lazy val digit19 = elem("digit", c => c >= '1' && c <= '9')

  lazy val digit = elem("digit", c => c >= '0' && c <= '9')

  lazy val hexDigit = elem("hex digit", c => (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))

  lazy val digits = rep1(digit)

  lazy val frac: Parser[String] = '.' ~> digits ^^ {digits => digits.mkString}

  lazy val theValue: Parser[Any] = string | number | jsonObject | array | istrue | isfalse | isnull

  lazy val array: Parser[List[Any]] = spaces ~ '[' ~ spaces ~> elements <~ spaces ~ ']' ~ spaces ^^ {case e => e}

  lazy val elements = repsep(theValue, spaces ~ ',' ~ spaces)

  lazy val istrue: Parser[Boolean] = acceptSeq("true") ^^ {case _ => true}
  lazy val isfalse: Parser[Boolean] = acceptSeq("false") ^^ {case _ => false}
  lazy val isnull = acceptSeq("null") ^^ {case _ => Empty}
}

}
}
