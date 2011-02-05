/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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

import _root_.scala.util.parsing.combinator._
import common._
import _root_.java.io._

object CSSHelpers extends ControlHelpers {

  /**
   * Adds a prefix to root relative paths in the url segments from the css content
   *
   * @param in - the text reader
   * @param rootPrefix - the prefix to be added
   * @return (Box[String], String) - returns the tuple containing the parsing output and the original input (as a String)
   */
  def fixCSS(in: Reader, rootPrefix: String): (Box[String], String) = {
      val reader = new BufferedReader(in)
      val res = new StringBuilder;
      var line: String = null;
      try {
        while ({line = reader.readLine(); line != null}) {
          res append line + "\n"
        }
      } finally {
        reader close
      }
      val str = res toString;
      (CSSParser(rootPrefix).fixCSS(str), str);
  }


}

/**
 * Combinator parser for prefixing root relative paths with a given prefix
 */
case class CSSParser(prefix: String) extends Parsers  {
  implicit def strToInput(in: String): Input = new _root_.scala.util.parsing.input.CharArrayReader(in.toCharArray)
  type Elem = Char


 lazy val contentParser = Parser[String] {
    case in =>
      val content = new StringBuilder;
      var seqDone = 0;

      def walk(in: Input)(f: Char => Boolean): Input = {
        var seq = in
        while (!seq.atEnd && !f(seq first)) {
          seq = seq rest
        }
        seq rest
      }

      val rest = walk(in) {
        case c =>
          content append c
          c.toLower match {
            case 'u' if (seqDone == 0) => seqDone = 1;
            case 'r' if (seqDone == 1) => seqDone = 2;
            case 'l' if (seqDone == 2) => seqDone = 3;
            case _ => seqDone = 0
          }
          seqDone == 3;
      }

      Success(content toString, rest);
  }



  lazy val spaces = (elem(' ') | elem('\t') | elem('\n') | elem('\r')).*
  // consider only root relative paths that start with /
  lazy val path = elem("path", c => c.isLetterOrDigit ||
                         c == '?' || c == '/' ||
                         c == '&' || c == '@' ||
                         c == ';' || c == '.' ||
                         c == '+' || c == '-' ||
                         c == '=' || c == ':' ||
                         c == ' ' || c == '_').+ ^^ {case l => l.mkString("")}

  // the URL might be wrapped in simple quotes
  lazy val seq1 = elem('\'') ~> path <~ elem('\'')
  // the URL might be wrapped in double quotes
  lazy val seq2 = elem('\"') ~> path <~ elem('\"')
  // do the parsing per CSS spec http://www.w3.org/TR/REC-CSS2/syndata.html#uri section 4.3.4
  lazy val expr = (spaces ~ elem('(') ~ spaces) ~> ( seq1 | seq2 | path ) <~ (spaces <~ elem(')')) ^^ {case s => {
      "('" + (s.trim.startsWith("/") match {
        case true => prefix + s.trim
        case _ => s.trim
      }) + "')"
    }
  }

  lazy val phrase = ((contentParser ~ expr).* ^^ {case l => l.flatMap(f => f._1 + f._2).mkString("")}) ~ contentParser ^^ {case a ~ b => a + b}

  def fixCSS(in: String): Box[String] = phrase(in) match {
    case Success(v, r) => (r atEnd) match {
      case true => Full(v)
      case _ => Empty // return Empty if the reader is not at end as it implies that parsing ended due to a parser error
      }
    case _ => Empty
  }

}

}
}
