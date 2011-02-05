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
import _root_.org.specs.runner._
import common._

object ListHelpersSpec extends Specification with ListHelpers {
  "The ListHelpers first_? function" should {
    "return an Empty can if the list is empty" in {
      first_?((Nil: List[Int]))((i: Int) => true) must_== Empty
    }
    "return an Empty can if no element in the list satisfies the predicate" in {
      first_?(List(1, 2, 3))((i: Int) => i < 0) must_== Empty
    }
    "return a Full can with the first element in the list satisfying the predicate" in {
      first_?(List(1, 2, 3))((i: Int) => i > 0) must_== Full(1)
    }
  }
  "The ListHelpers first function" should {
    "return an Empty can if the list is empty" in {
      first((Nil: List[Int]))((i: Int) => Full(1)) must_== Empty
    }
    "return an Empty can if no element in the list returns a Full can when applied a function" in {
      first(List(1, 2, 3))((i: Int) => Empty) must_== Empty
    }
    "return the first Full can returned by a function f over the list elements" in {
      val f = (i: Int) => i >= 2 match { case true => Full(3) case false => Empty }
      first(List(1, 2, 3))(f) must_== Full(3)
    }
  }
  "The ciGet function on Lists of pairs of string" should {
    "return Empty if the list is Nil" in {
      (Nil: List[(String, String)]).ciGet("") must_== Empty
    }
    "return Empty if no pair has the key as its first element" in {
      List(("one", "1"), ("two", "2")).ciGet("three") must_== Empty
    }
    "return a Full can with the first second value of a pair matching the key" in {
      List(("one", "1"), ("two", "2")).ciGet("one") must_== Full("1")
    }
    "return a Full can with the first second value of a pair matching the key case-insensitively" in {
      List(("one", "1"), ("two", "2"), ("two", "3")).ciGet("two") must_== Full("2")
    }
  }
  "The ListHelpers enumToList and enumToStringList functions" should {
    "convert a java enumeration to a List" in {
      val v: _root_.java.util.Vector[Int] = new _root_.java.util.Vector[Int]
      v.add(1); v.add(2)
      enumToList(v.elements) must_== List(1, 2)
    }
    "convert a java enumeration containing any kind of object to a List of Strings" in {
      val v: _root_.java.util.Vector[Any] = new _root_.java.util.Vector[Any]
      v.add(1); v.add("hello")
      enumToStringList(v.elements) must_== List("1", "hello")
    }
  }
  "The ListHelpers head function (headOr on a list object)" should {
    "return the first element of a list" in {
      List(1).headOr(2) must_== 1
    }
    "return a default value if the list is empty" in {
      head(Nil, 2) must_== 2
    }
    "not evaluate the default valueif list is not empty" in {
      head(List(1), {error("stop"); 2}) must_== 1
    }
  }
  "The ListHelpers listIf function" should {
    "create a List containing an element if the predicate is true" in {
      listIf(true)(1) must_== List(1)
    }
    "return an empty List if the predicate is false" in {
      listIf(false)(1) must_== Nil
    }
    "not evaluate its argument if the predicate is false" in {
      listIf(false)({error("stop"); 1}) must_== Nil
    }
  }
  "The ListHelpers rotateList function (rotate method on a List object)" should {
    "create a List of all the circular permutations of a given list" in {
      List(1, 2, 3).rotate must_== List(List(1, 2, 3), List(2, 3, 1), List(3, 1, 2))
    }
  }
  "The ListHelpers permuteList function (permute method on a List object)" should {
    "create a List of all the permutations of a given list" in {
      List(1, 2, 3).permute must_== List(List(1, 2, 3), List(1, 3, 2), List(2, 3, 1), List(2, 1, 3), List(3, 1, 2), List(3, 2, 1))
    }
  }
  "The ListHelpers permuteWithSublists function (permuteAll method on a List object)" should {
    "create a List of all the permutations of a given list" in {
      List(1, 2, 3).permuteAll must_== List(List(1, 2, 3), List(1, 3, 2), List(2, 3, 1),
                                            List(2, 1, 3), List(3, 1, 2), List(3, 2, 1),
                                            List(2, 3), List(3, 2), List(3, 1),
                                            List(1, 3), List(1, 2), List(2, 1),
                                            List(3), List(2), List(1))
    }
  }
  "The ListHelpers" should {
    "provide an or method on Lists returning the list itself if not empty or another list if it is empty" in {
      List(1).or(List(2)) must_== List(1)
      (Nil: List[Int]).or(List(2)) must_== List(2)
    }
    "provide a str method on Lists joining the toString value of all elements" in {
      List("h", "e", "l", "l", "o").str must_== "hello"
    }
    "provide a comma method on Lists being an alias for mkString(\", \")" in {
      List("hello", "world").comma must_== "hello, world"
    }
    "provide a join method on Lists being an alias for mkString" in {
      List("hello", "world").join(", ") must_== "hello, world"
    }
    "provide a ? method return true iff the list is not empty" in {
      List().?  must beFalse
      List(1).?  must beTrue
    }
    "provide a replace method to replace one element of the list at a given position (0-based index)." +
    " If the position is negative, the first element is replaced" in {
      List(1, 2, 3).replace(1, 4)  must_== List(1, 4, 3)
      List(1, 2, 3).replace(4, 4)  must_== List(1, 2, 3)
      List(1, 2, 3).replace(-1, 4)  must_== List(4, 2, 3)
    }
  }
}
class ListHelpersSpecTest extends JUnit4(ListHelpersSpec)

}
}
