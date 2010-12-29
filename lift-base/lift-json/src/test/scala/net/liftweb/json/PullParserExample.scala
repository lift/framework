package net.liftweb.json

import org.specs.Specification
import org.specs.runner.{Runner, JUnit}

class PullParserExampleTest extends Runner(PullParserExample) with JUnit
object PullParserExample extends Specification {
  import JsonParser._

  "Pull parsing example" in {
    val parser = (p: Parser) => {
      def parse: BigInt = p.nextToken match {
        case FieldStart("postalCode") => p.nextToken match {
          case IntVal(code) => code
          case _ => p.fail("expected int")
        }
        case End => p.fail("no field named 'postalCode'")
        case _ => parse
      }

      parse
    }

    val postalCode = parse(json, parser)
    postalCode mustEqual 10021
  }

  val json = """
  {
     "firstName": "John",
     "lastName": "Smith",
     "address": {
         "streetAddress": "21 2nd Street",
         "city": "New York",
         "state": "NY",
         "postalCode": 10021
     },
     "phoneNumbers": [
         { "type": "home", "number": "212 555-1234" },
         { "type": "fax", "number": "646 555-4567" }
     ],
     "newSubscription": false,
     "companyName": null
 }"""
}
