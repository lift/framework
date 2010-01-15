package net.liftweb.json

import _root_.org.scalacheck._
import _root_.org.scalacheck.Prop._
import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}
import _root_.org.specs.ScalaCheck

class ParserTest extends Runner(ParserSpec) with JUnit
object ParserSpec extends Specification with JValueGen with ScalaCheck {
  import JsonAST._
  import JsonParser._
  import Printer._

  "Any valid json can be parsed" in {
    val parsing = (json: JValue) => { parse(Printer.pretty(render(json))); true }
    forAll(parsing) must pass
  }

  "Buffer size does not change parsing result" in {
    val bufSize = Gen.choose(2, 64)
    val parsing = (x: JValue, s1: Int, s2: Int) => { parseVal(x, s1) == parseVal(x, s2) }
    forAll(genObject, bufSize, bufSize)(parsing) must pass
  }

  "All valid string escape characters can be parsed" in {
    parse("[\"abc\\\"\\\\\\/\\b\\f\\n\\r\\t\\u00a0\"]") must_== JArray(JString("abc\"\\/\b\f\n\r\t\u00a0")::Nil)
  }

  implicit def arbJValue: Arbitrary[JValue] = Arbitrary(genObject)

  private def parseVal(json: JValue, bufSize: Int) = {
    val existingSize = JsonParser.Buffer.bufSize
    try {
      JsonParser.Buffer.bufSize = bufSize
      JsonParser.Buffer.clear
      JsonParser.parse(compact(render(json)))
    } finally {
      JsonParser.Buffer.bufSize = existingSize
    }
  }
}
