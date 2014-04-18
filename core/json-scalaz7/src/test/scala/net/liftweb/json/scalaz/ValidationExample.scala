package net.liftweb.json.scalaz

import scala.language.reflectiveCalls

import scalaz._
import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.syntax.validation._
import JsonScalaz._
import net.liftweb.json._

import org.specs2.mutable.Specification

object ValidationExample extends Specification {

  case class Person(name: String, age: Int)

  "Validation" should {
    def min(x: Int): Int => Result[Int] = (y: Int) => 
      if (y < x) Fail("min", y + " < " + x) else y.success

    def max(x: Int): Int => Result[Int] = (y: Int) => 
      if (y > x) Fail("max", y + " > " + x) else y.success

    val json = JsonParser.parse(""" {"name":"joe","age":17} """)

    "fail when age is less than min age" in {
      // Age must be between 18 an 60
      val ageResult = (jValue: JValue) => for {
        age <- field[Int]("age")(jValue)
        _ <- min(18)(age)
        _ <- max(60)(age)
      } yield age
      val person = Person.applyJSON(field[String]("name"), ageResult)
      person(json) mustEqual Failure(NonEmptyList(UncategorizedError("min", "17 < 18", Nil)))
    }

    "pass when age within limits" in {
      // Age must be between 16 an 60
      val ageResult = (jValue: JValue) => for {
        age <- field[Int]("age")(jValue)
        _ <- min(16)(age)
        _ <- max(60)(age)
      } yield age
      val person = Person.applyJSON(field[String]("name"), ageResult)
      person(json) mustEqual Success(Person("joe", 17))
    }
  }

  case class Range(start: Int, end: Int)

  // This example shows:
  // * a validation where result depends on more than one value
  // * parse a List with invalid values

  "Range filtering" should {
    val json = JsonParser.parse(""" [{"s":10,"e":17},{"s":12,"e":13},{"s":11,"e":8}] """)

    def ascending: (Int, Int) => Result[(Int, Int)] = (x1: Int, x2: Int) => 
      if (x1 > x2) Fail("asc", x1 + " > " + x2) else (x1, x2).success

    // Valid range is a range having start <= end
    implicit def rangeJSON: JSONR[Range] = new JSONR[Range] {
      def read(json: JValue) = {
        for {
          s <- field[Int]("s")(json)
          e <- field[Int]("e")(json)
          r <- ascending(s, e)
        } yield Range.tupled(r)
      }
    }

    "fail if lists contains invalid ranges" in {
      val r = fromJSON[List[Range]](json)
      r mustEqual Failure(NonEmptyList(UncategorizedError("asc", "11 > 8", Nil)))
    }
 
    "optionally return only valid ranges" in {
      val ranges = json.children.map(fromJSON[Range]).filter(_.isSuccess).sequence[({type λ[α]=ValidationNel[Error, α]})#λ, Range]
      ranges mustEqual Success(List(Range(10, 17), Range(12, 13)))
    }
  }

}
