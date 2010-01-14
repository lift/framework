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
    val parsing = (json: JValue, s1: Int, s2: Int) => { 
      (s1 > 1 && s2 > 1) ==> (parseWithNewBuf(json, s1) == parseWithNewBuf(json, s2))
    }
    forAll(parsing) must pass
  }

  "All valid string escape characters can be parsed" in {
    parse("[\"abc\\\"\\\\\\/\\b\\f\\n\\r\\t\\u00a0\"]") must_== JArray(JString("abc\"\\/\b\f\n\r\t\u00a0")::Nil)
  }

  implicit def arbJValue: Arbitrary[JValue] = Arbitrary(genObject)

  private def parseWithNewBuf(json: JValue, bufSize: Int) = {
    println(json)
    println(bufSize)
    val existingSize = JsonParser.Buf.bufSize
    try {
      JsonParser.Buf.bufSize = bufSize
      JsonParser.Buf.clear
      parse(compact(render(json)))
    } finally {
      JsonParser.Buf.bufSize = existingSize
    }
  }
}
