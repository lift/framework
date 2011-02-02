package net.liftweb.json.scalaz

import scalaz._
import Scalaz._
import net.liftweb.json._
import net.liftweb.json.JsonAST._ // FIXME remove
import net.liftweb.json.Printer._ // FIXME remove

import org.specs.Specification
import org.specs.runner.{Runner, JUnit}

class ExampleTest extends Runner(Example) with JUnit
object Example extends Specification {
  import JsonScalaz._

  case class Address(street: String, zipCode: String)
  case class Person(name: String, age: Int, address: Address)
  
  "Parse address in Applicative style" in {
    val json = JsonParser.parse(""" {"street": "Manhattan 2", "zip": "00223" } """)
    val a1 = field[String](json, "zip") <*> (field[String](json, "street") map Address.curried)
    val a2 = (field[String](json, "street") |@| field[String](json, "zip")) { Address }
    a1 mustEqual Success(Address("Manhattan 2", "00223"))
    a2 mustEqual a1
  }

  "Failed address parsing" in {
    val json = JsonParser.parse(""" {"street": "Manhattan 2", "zip": "00223" } """)
    val a = (field[String](json, "streets") |@| field[String](json, "zip")) { Address }
    a mustEqual Failure("no such field 'streets'")
  }

  "Parse Person with Address" in {
    implicit def addrJSON: JSONR[Address] = new JSONR[Address] {
      def read(json: JValue) = 
        (field[String](json, "street") |@| field[String](json, "zip")) { Address }
    }

    val p = JsonParser.parse(""" {"name":"joe","age":34,"address":{"street": "Manhattan 2", "zip": "00223" }} """)
    val person = (field[String](p, "name") |@| field[Int](p, "age") |@| field[Address](p, "address")) { Person }
    person mustEqual Success(Person("joe", 34, Address("Manhattan 2", "00223")))
  }

  "Format Person with Address" in {
    implicit def addrJSON: JSONW[Address] = new JSONW[Address] {
      def write(a: Address) = 
        makeObj(("street" -> toJSON(a.street)) :: ("zip" -> toJSON(a.zipCode)) :: Nil)
    }

    val p = Person("joe", 34, Address("Manhattan 2", "00223"))
    val json = makeObj(("name" -> toJSON(p.name)) :: 
                       ("age" -> toJSON(p.age)) :: 
                       ("address" -> toJSON(p.address)) :: Nil)
    compact(render(json)) mustEqual 
      """{"name":"joe","age":34,"address":{"street":"Manhattan 2","zip":"00223"}}"""
  }
}
