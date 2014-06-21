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
package util

import scala.util.parsing.combinator.Parsers

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.{Arbitrary, Gen, Prop}
import Gen._
import Prop._


object CombParserHelpersSpec extends Specification with ScalaCheck {
  "CombParserHelpers Specification".title

  object ParserHelpers extends CombParserHelpers with Parsers
  import ParserHelpers._

  "The parser helpers" should {
    "provide an isEof function returning true iff a char is end of file" in {
      isEof('\u001a') must beTrue
    }
    "provide an notEof function returning true iff a char is not end of file" in {
      notEof('\u001a') must beFalse
    }
    "provide an isNum function returning true iff a char is a digit" in {
      isNum('0') must beTrue
    }
    "provide an notNum function returning true iff a char is not a digit" in {
      notNum('0') must beFalse
    }
    "provide an wsc function returning true iff a char is a space character" in {
      List(' ', '\t', '\r', '\n') foreach {wsc(_) must beTrue}
      wsc('a') must beFalse
    }
    "provide a whitespace parser: white. Alias: wsc" in {
      import WhiteStringGen._
      val whiteParse = (s: String) => wsc(s).isInstanceOf[Success[_]]
      check(forAll(whiteParse))
    }
    "provide a whiteSpace parser always succeeding and discarding its result" in {
      import StringWithWhiteGen._
      val whiteSpaceParse =
        (s: String) => whiteSpace(s) must beLike {
          case Success(x, y) => x.toString must_== "()"
        }
      check(forAll(whiteSpaceParse))
    }
    "provide an acceptCI parser to parse whatever string matching another string ignoring case" in {
      import AbcdStringGen._
      val ignoreCaseStringParse: Function2[String, String, Boolean] =
        (s: String, s2: String) => acceptCI(s).apply(s2) match {
          case Success(x, y) => s2.toUpperCase must startWith(s.toUpperCase)
          case _             => true
        }
      check(forAll(ignoreCaseStringParse))
    }

    "provide a digit parser - returning a String" in {
      val isDigit: String => Boolean =
        (s: String) => digit(s) match {
          case Success(x, y) => s must beMatching ("\\p{Nd}.*")
          case _             => true
        }
      check(forAll(isDigit))
    }
    "provide an aNumber parser - returning an Int if succeeding" in {
      val number: String => Boolean =
        (s: String) => {
          aNumber(s) match {
            case Success(x, y) => s must beMatching ("\\p{Nd}+.*")
            case _             => true
          }
        }
      check(forAll(number))
    }

    "provide a slash parser" in {
      slash("/").get must_== '/'
      slash("x") must beLike {case Failure(_, _) => 1 must_== 1}
    }
    "provide a colon parser" in {
      colon(":").get must_== ':'
      colon("x") must beLike {case Failure(_, _) => 1 must_== 1}
    }
    "provide a EOL parser which parses the any and discards any end of line character" in {
      List("\n", "\r") map {
        s =>
          val result = EOL(s)
          result.get.toString must_== "()"
          result.next.atEnd must beTrue
      }

      success
    }
    val parserA = elem("a", (c: Char) => c == 'a')
    val parserB = elem("b", (c: Char) => c == 'b')
    val parserC = elem("c", (c: Char) => c == 'c')
    val parserD = elem("d", (c: Char) => c == 'd')
    def shouldSucceed[T](r: ParseResult[T]) = r match {
      case Success(x, y) => true
      case _             => false
    }
    "provide a permute parser succeeding if any permutation of given parsers succeeds" in {
      def permuteParsers(s: String) = shouldSucceed(permute(parserA, parserB, parserC, parserD)(s))
      val permutationOk = (s: String) => permuteParsers(s)
      check(forAll(AbcdStringGen.abcdString)(permutationOk))
    }
    "provide a permuteAll parser succeeding if any permutation of the list given parsers, or a sublist of the given parsers succeeds" in {
      def permuteAllParsers(s: String) = shouldSucceed(permuteAll(parserA, parserB, parserC, parserD)(s))
      implicit def pick3Letters = AbcdStringGen.pickN(3, List("a", "b", "c"))
      check {
        forAll { (s: String) =>
          (!(new scala.collection.immutable.StringOps(s)).isEmpty) ==> permuteAllParsers(s)
        }
      }
    }
    "provide a repNN parser succeeding if an input can be parsed n times with a parser" in {
      def repNNParser(s: String) = shouldSucceed(repNN(3, parserA)(s))
      implicit def pick3Letters = AbcdStringGen.pickN(3, List("a", "a", "a"))
      check {
        forAll { (s: String) =>
          (!(new scala.collection.immutable.StringOps(s)).isEmpty) ==> repNNParser(s)
        }
      }
    }
  }
}


object AbcdStringGen {
  implicit def abcdString =
    for (
      len <- choose(4, 4);
      string <- pick(len, List("a", "b", "c", "d"))
    ) yield string.mkString("")

  def pickN(n: Int, elems: List[String]) =
    Arbitrary { for (string <- pick(n, elems)) yield string.mkString("") }
}


object WhiteStringGen {
  def genWhite =
    for (
      len <- choose(1, 4);
      string <- listOfN(len, frequency((1, value(" ")), (1, value("\t")), (1, value("\r")), (1, value("\n"))))
    ) yield string.mkString("")

  implicit def genWhiteString: Arbitrary[String] =
    Arbitrary { genWhite }
}


object StringWithWhiteGen {
  import WhiteStringGen._

  def genStringWithWhite =
    for (
      len <- choose(1, 4);
      string <- listOfN(len, frequency((1, value("a")), (2, value("b")), (1, genWhite)))
    ) yield string.mkString("")

  implicit def genString: Arbitrary[String] =
    Arbitrary { genStringWithWhite }
}

