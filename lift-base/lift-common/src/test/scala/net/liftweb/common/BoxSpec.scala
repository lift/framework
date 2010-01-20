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
package common {

import _root_.org.specs._
import _root_.net.liftweb.common.Box._
import _root_.org.specs.runner._
import _root_.org.specs.Sugar._

class BoxSpecTest extends Runner(BoxSpec) with JUnit with Console
object BoxSpec extends Specification {
  "A Box" can {
    "be created from a Option. It is Empty if the option is None" in {
      Box(None) mustBe Empty
    }
    "be created from a Option. It is Full(x) if the option is Some(x)" in {
      Box(Some(1)) must_== Full(1)
    }
    "be created from a List containing one element. It is Empty if the list is empty" in {
      Box(Nil) mustBe Empty
    }
    "be created from a List containing one element. It is Full(x) if the list is List(x)" in {
      Box(List(1)) must_== Full(1)
    }
    "be created from a List containing more than one element. It is Full(x) if the list is x::rest" in {
      Box(List(1, 2, 3)) must_== Full(1)
    }
    "be used as an iterable" in {
      Full(1) reduceLeft {(x: Int, y: Int) => x + y} must_== 1
    }
    "be used as an Option" in {
      Full(1).get must_== 1
      Empty.isDefined must beFalse
    }
    "be implicitly defined from an Option. The open_! method can be used on an Option for example" in {
      Some(1).open_! must_== 1
    }
    "be defined from some legacy code (possibly passing null values). If the passed value is not null, a Full(value) is returned" in {
      Box.legacyNullTest("s") must_== Full("s")
    }
    "be defined from some legacy code (possibly passing null values). If the passed value is null, an Empty is returned" in {
      Box.legacyNullTest(null) must_== Empty
    }
  }
  "A Box" should {
    "provide a 'choice' method to either apply a function to the Box value or return another default can" in {
      def gotIt = (x: Int) => Full("got it: " + x.toString)

      Full(1).choice(gotIt)(Full("nothing")) must_== Full("got it: 1")
      Empty.choice(gotIt)(Full("nothing")) must_== Full("nothing")
    }
  }
  "A Full Box" should {
    "not beEmpty" in {
      Full(1).isEmpty must beFalse
    }
    "be defined" in {
      Full(1).isDefined must beTrue
    }
    "return its value when opened" in {
      Full(1).open_! mustBe 1
    }
    "return its value when opened with openOr(default value)" in {
      Full(1) openOr 0 mustBe 1
    }
    "return itself when or'ed with another Box" in {
      Full(1) or Full(2) must_== Full(1)
    }
    "define an 'exists' method returning true if the Box value verifies the function" in {
      Full(1) exists {_ > 0} must beTrue
    }
    "define an exists method returning false if the Box value doesn't verify the function" in {
      Full(0) exists {_ > 0} must beFalse
    }
    "define a 'filter' method, returning a Full Box if the filter is satisfied" in {
      Full(1) filter {_ > 0} must_== Full(1)
    }
    "define a 'filter' method, returning Empty if the filter is not satisfied" in {
      Full(1) filter {_ == 0} mustBe Empty
    }
    "define a 'filterMsg' method, returning a Failure if the filter predicate is not satisfied" in {
      Full(1).filterMsg("not equal to 0")(_ == 0) must_== Failure("not equal to 0", Empty, Empty)
    }
    "define a 'foreach' method using its value (to display it for instance)" in {
      var total = 0
      Full(1) foreach { total += _ }
      total must_== 1
    }
    "define a 'map' method to transform its value" in {
      Full(1) map { _.toString } must_== Full("1")
    }
    "define a 'flatMap' method transforming its value in another Box. If the value is transformed in a Full can, the total result is a Full can" in {
      Full(1) flatMap { x: Int => if (x > 0) Full("full") else Empty } must_== Full("full")
    }
    "define a 'flatMap' method transforming its value in another Box. If the value is transformed in an Empty can, the total result is an Empty can" in {
      Full(0) flatMap { x: Int => if (x > 0) Full("full") else Empty } mustBe Empty
    }
    "define an 'elements' method returning an iterator containing its value" in {
      Full(1).elements.next must_== 1
    }
    "define a 'toList' method returning a List containing its value" in {
      Full(1).toList must_== List(1)
    }
    "define a 'toOption' method returning a Some object containing its value" in {
      Full(1).toOption must_== Some(1)
    }
    "return itself if asked for its status with the operator ?~" in {
      Full(1) ?~ "error" must_== Full(1)
    }
    "return itself if asked for its status with the operator ?~!" in {
      Full(1) ?~! "error" must_== Full(1)
    }
    "define a 'pass' method passing the can to a function and returning itself (alias: $)" in {
      var empty = false
      def emptyString(s: Box[String]) = s foreach {c: String => empty = c.isEmpty}
      Full("") $ emptyString _
      empty must beTrue
    }
    "define a 'run' method either returning a default value or applying a user-defined function on it" in {
      def appendToString(s: String, x: Int) = s + x.toString
      Full(1).run("string")(appendToString) must_== "string1"
    }
    "define a 'isA' method returning a Full(value) if the value is the instance of a given class" in {
      Full("s").isA(classOf[String]) must_== Full("s")
    }
    "define a 'isA' method returning Empty if the value is not the instance of a given class" in {
      Full("s").isA(classOf[Double]) must_== Empty
    }
    "define a 'asA' method returning a Full(value) if the value is the instance of a given type" in {
      Full("s").asA[String] must_== Full("s")
    }
    "define a 'asA' method returning Empty if the value is not the instance of a given type" in {
      Full("s").asA[Double] must_== Empty
    }
  }
  "An Empty Box" should {
    "beEmpty" in {
      Empty.isEmpty must beTrue
    }
    "not be defined" in {
      Empty.isDefined must beFalse
    }
    "throw an exception if opened" in {
      {Empty.open_!; ()} must throwA[NullPointerException]
    }
    "return a default value if opened with openOr" in {
      Empty.openOr(1) mustBe 1
    }
    "return the other Box if or'ed with another Box" in {
      Empty.or(Full(1)) must_== Full(1)
    }
    "return itself if filtered with a predicate" in {
      val empty: Box[Int] = Empty
      empty.filter {_ > 0} mustBe Empty
    }
    "define an 'exists' method returning false" in {
      val empty: Box[Int] = Empty
      empty exists {_ > 0} must beFalse
    }
    "define a 'filter' method, returning Empty" in {
      val empty: Box[Int] = Empty
      empty filter {_ > 0} mustBe Empty
    }
    "define a 'filterMsg' method, returning a Failure" in {
      Empty.filterMsg("not equal to 0")(_ == 0) must_== Failure("not equal to 0", Empty, Empty)
    }
    "define a 'foreach' doing nothing" in {
      var total = 0
      val empty: Box[Int] = Empty
      empty foreach { total += _ }
      total must_== 0
    }
    "define a 'map' method returning Empty" in {
      Empty map { _.toString } mustBe Empty
    }
    "define a 'flatMap' method returning Empty" in {
      Empty flatMap { x: Int => Full("full") } mustBe Empty
    }
    "define an 'elements' method returning an empty iterator" in {
      Empty.elements.hasNext must beFalse
    }
    "define a 'toList' method returning Nil" in {
      Empty.toList must_== Nil
    }
    "define a 'toOption' method returning None" in {
      Empty.toOption must_== None
    }
    "return a failure with a message if asked for its status with the operator ?~" in {
      Empty ?~ "nothing" must_== Failure("nothing", Empty, Empty)
    }
    "return a failure with a message if asked for its status with the operator ?~!" in {
      Empty ?~! "nothing" must_== Failure("nothing", Empty, Empty)
    }
    "define a 'isA' method returning Empty" in {
      Empty.isA(classOf[Double]) must_== Empty
    }
    "define a 'asA' method returning Empty" in {
      Empty.asA[Double] must_== Empty
    }
  }
  "A Failure is an Empty Box which" can {
    "return its cause as an exception" in {
      case class LiftException(m: String) extends Exception
      Failure("error", Full(new LiftException("broken")), Empty).exception.get must_== new LiftException("broken")
    }
    "return a chained list of causes" in {
      Failure("error",
               Full(new Exception("broken")),
               Full(Failure("nested cause", Empty, Empty))).chain must_== Full(Failure("nested cause", Empty, Empty))
    }
  }
  "A Failure is an Empty Box which" should {
    "return itself if mapped or flatmapped" in {
      Failure("error", Empty, Empty) map {_.toString} must_== Failure("error", Empty, Empty)
      Failure("error", Empty, Empty) flatMap {x: String => Full(x.toString)} must_== Failure("error", Empty, Empty)
    }
    "return a itself when asked for its status with the operator ?~" in {
      Failure("error", Empty, Empty) ?~ "nothing" must_== Failure("error", Empty, Empty)
    }
    "create a new failure with a chained message if asked for its status with the operator ?~!" in {
      Failure("error", Empty, Empty) ?~! "error2" must_== Failure("error2", Empty, Full(Failure("error", Empty, Empty)))
    }
  }
}

}
}
