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

import _root_.scala.collection.mutable._
import _root_.scala.util.parsing.combinator._

/**
 * Parser a VCard entry such as
 * 
 * BEGIN:VCARD
 * VERSION:2.1
 * N:Gump;Forrest
 * FN:Forrest Gump
 * ORG:Bubba Gump Shrimp Co.
 * TITLE:Shrimp Man
 * TEL;WORK;VOICE:(111) 555-1212
 * TEL;HOME;VOICE:(404) 555-1212
 * ADR;WORK:;;100 Waters Edge;Baytown;LA;30314;United States of America
 * END:VCARD
 *
 */
object VCardParser extends Parsers {
  type Elem = Char

  implicit def strToInput(in: String): Input = new _root_.scala.util.parsing.input.CharArrayReader(in.toCharArray)

  case class VCardKey(name: String, props: List[(String, String)])
  case class VCardEntry(key: VCardKey, value: List[String])

  lazy val multiLineSep = opt(elem('\n') ~ elem(' '))
  lazy val value = (multiLineSep ~> elem("value", {c => !c.isControl && c != ';'}) <~ multiLineSep).* ^^ {case l => l.mkString}
  lazy val spaces = (elem(' ') | elem('\t') | elem('\n') | elem('\r'))*
  lazy val key = elem("key", {c => c.isLetterOrDigit || c == '-' || c == '_'}).+ ^^ {case list => list.mkString}
  lazy val props = ((((elem(';') ~> key <~ elem('=')) ~ key) ^^ {case a ~ b => (a, b)}) | ((elem(';') ~> key) ^^ {case a => (a, "")}))*
  lazy val left = (key ~ props) ^^ {case k ~ l => VCardKey(k, l)}
  lazy val expr = (((spaces ~> left ~! elem(':')) ~ repsep(value, ';')) ^^ {case a ~ _ ~ b => VCardEntry(a, b)})+

  def parse(in: String): Either[List[VCardEntry], String] = expr(in) match {
    case Success(v, r) => Left(v)
    case err @ _ => Right(err toString)
  }
}

}
}
