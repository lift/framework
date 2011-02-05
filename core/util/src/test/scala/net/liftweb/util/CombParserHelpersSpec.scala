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

import _root_.org.specs._
import _root_.scala.util.parsing.input._
import _root_.org.specs.runner._
import _root_.scala.util.parsing.combinator.Parsers
import _root_.org.scalacheck._
import _root_.org.scalacheck.Gen._
import _root_.org.scalacheck.Prop._
import _root_.org.scalacheck.Shrink._
import _root_.org.specs.ScalaCheck
import common._

object ParserHelpers extends net.liftweb.util.CombParserHelpers with Parsers

class CombParserHelpersSpecTest extends Runner(CombParserHelpersSpec) with JUnit with Console
object CombParserHelpersSpec extends Specification with ScalaCheck {
  import ParserHelpers._
  "The parser helpers" should {
    "provide an isEof function returning true iff a char is end of file" in {
      isEof('\032') must beTrue
    }
    "provide an notEof function returning true iff a char is not end of file" in {
      notEof('\032') must beFalse
    }
    "provide an isNum function returning true iff a char is a digit" in {
      isNum('0') must beTrue
    }
    "provide an notNum function returning true iff a char is not a digit" in {
      notNum('0') must beFalse
    }
    "provide an wsc function returning true iff a char is a space character" in {
      List(' ', '\t', '\r', '\n') foreach { wsc(_) must beTrue }
      wsc('a') must beFalse
    }
    "provide a whitespace parser: white. Alias: wsc" in {
      import whiteStringGen._
      val whiteParse = (s: String) => wsc(s) must beLike { case Success(_, _) => true }
      forAll(whiteParse) must pass
    }
    "provide a whiteSpace parser always succeeding and discarding its result" in {
      import stringWithWhiteGen._
      val whiteSpaceParse = (s: String) => whiteSpace(s) must beLike {
        case Success(x, y) => x.toString == "()"
        case _ => false
      }
      forAll(whiteSpaceParse) must pass
    }
    "provide an acceptCI parser to parse whatever string matching another string ignoring case" in {
      import abcdStringGen._
      val ignoreCaseStringParse: Function2[String, String, Boolean] =
        (s: String, s2: String) => acceptCI(s).apply(s2) match {
          case Success(x, y) => s2.toUpperCase must startWith(s.toUpperCase)
          case _ => true
        }
      forAll(ignoreCaseStringParse) must pass
    }
    /** FIXME 280 -- these tests fail and I'm not sure why
    "provide a digit parser - returning a String" in {
      val isDigit: String => Boolean =
        (s: String) => digit(s) match {
          case Success(x, y) => s mustMatch("\\d")
          case _ => true
        }
      forAll(isDigit) must pass
    }
    "provide an aNumber parser - returning an Int if succeeding" in {
      val number: String => Boolean =
        (s: String) => {
          aNumber(s) match {
            case Success(x, y) => 
              s.toInt == x
            case _ => true
          }
        }
      forAll(number) must pass
    }
    */

    "provide a slash parser" in {
      slash("/").get must_== '/'
      slash("x") must beLike {case Failure(_, _) => true}
    }
    "provide a colon parser" in {
      colon(":").get must_== ':'
      colon("x") must beLike {case Failure(_, _) => true}
    }
    "provide a EOL parser which parses the any and discards any end of line character" in {
      List("\n", "\r") foreach { s =>
        val result = EOL(s)
        result.get.toString must_== "()"
        result.next.atEnd must beTrue
      }
    }
    val parserA = elem("a", (c: Char) => c == 'a')
    val parserB = elem("b", (c: Char) => c == 'b')
    val parserC = elem("c", (c: Char) => c == 'c')
    val parserD = elem("d", (c: Char) => c == 'd')
    def shouldSucceed[T](r: ParseResult[T]) = r match {
      case Success(x, y) => true
      case _ => false
    }
    "provide a permute parser succeeding if any permutation of given parsers succeeds" in {
      def permuteParsers(s: String) = shouldSucceed(permute(parserA, parserB, parserC, parserD)(s))
      val permutationOk = (s: String) => permuteParsers(s)
      abcdStringGen.abcdString must pass(permutationOk)
    }
    "provide a permuteAll parser succeeding if any permutation of the list given parsers, or a sublist of the given parsers succeeds" in {
      def permuteAllParsers(s: String) = shouldSucceed(permuteAll(parserA, parserB, parserC, parserD)(s))
      implicit def pick3Letters = abcdStringGen.pickN(3, List("a", "b", "c"))
      forAll((s: String) => (!(new scala.collection.immutable.StringOps(s)).isEmpty) ==> permuteAllParsers(s)) must pass
    }
    "provide a repNN parser succeeding if an input can be parsed n times with a parser" in {
      def repNNParser(s: String) = shouldSucceed(repNN(3, parserA)(s))
      implicit def pick3Letters = abcdStringGen.pickN(3, List("a", "a", "a"))
      forAll((s: String) => (!(new scala.collection.immutable.StringOps(s)).isEmpty) ==> repNNParser(s)) must pass
    }
  }
}
object abcdStringGen {
  implicit def abcdString = for (len <- choose(4, 4);
         string <- pick(len, List("a", "b", "c", "d"))
         ) yield string.mkString("")
  def pickN(n: Int, elems: List[String]) = Arbitrary {
    for (string <- pick(n, elems)) yield string.mkString("")
  }
}
object whiteStringGen {
  def genWhite = for (len <- choose(1, 4);
         string <- listOfN(len, frequency((1, value(" ")), (1, value("\t")), (1, value("\r")), (1, value("\n"))))
         ) yield string.mkString("")

  implicit def genWhiteString: Arbitrary[String] = Arbitrary {
    genWhite
  }
}
object stringWithWhiteGen {
  import whiteStringGen._
  implicit def genString: Arbitrary[String] = Arbitrary {
    for (len <- choose(1, 4);
         string <- listOfN(len, frequency((1, value("a")), (2, value("b")), (1, genWhite)))
    ) yield string.mkString("")
  }
}

}
}
