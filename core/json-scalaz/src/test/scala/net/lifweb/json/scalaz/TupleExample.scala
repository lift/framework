package net.liftweb.json.scalaz

import scalaz._
import Scalaz._
import JsonScalaz._
import net.liftweb.json._

import org.specs.Specification
import org.specs.runner.{Runner, JUnit}

class TupleExampleTest extends Runner(TupleExample) with JUnit
object TupleExample extends Specification {
  "Parse tuple from List" in {
    val json = JsonParser.parse(""" [1,2,3] """)
    fromJSON[Tuple3[Int, Int, Int]](json) mustEqual Success(1, 2, 3)
  }
}
