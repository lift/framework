package net.liftweb.json.scalaz

import scalaz._
import Scalaz._
import net.liftweb.json._
import net.liftweb.json.JsonAST._

import org.specs.Specification
import org.specs.runner.{Runner, JUnit}

class ExampleTest extends Runner(Example) with JUnit
object Example extends Specification {
  import JsonScalaz._

  case class Address(street: String, zipCode: String)
  case class Person(name: String, age: Int, address: Address)
  
  "Parse address in Applicative style" in {
    val json = JsonParser.parse(""" {"street": "Manhattan 2", "zip": "00223" } """)
    val a = field[String](json, "zip") <*> (field[String](json, "street") map Address.curried)
    a mustEqual Success(Address("Manhattan 2", "00223"))
  }

  "Failed address parsing" in {
    val json = JsonParser.parse(""" {"street": "Manhattan 2", "zip": "00223" } """)
    val a = field[String](json, "zip") <*> (field[String](json, "streets") map Address.curried)
    a mustEqual Failure("no such field 'streets'")
  }

  "Parse Person with Address" in {
    implicit def addrJSON: JSON[Address] = new JSON[Address] {
      def read(json: JValue) = 
        field[String](json, "zip") <*> (field[String](json, "street") map Address.curried)

      def write(value: Address) = error("fixme")
    }

    val p = JsonParser.parse(""" {"name":"joe","age":34,"address":{"street": "Manhattan 2", "zip": "00223" }} """)
    val person = field[Address](p, "address") <*> (field[Int](p, "age") <*> (field[String](p, "name") map Person.curried))
    person mustEqual Success(Person("joe", 34, Address("Manhattan 2", "00223")))
  }
}
