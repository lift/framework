package net.liftweb.json

import _root_.org.scalacheck._
import _root_.org.scalacheck.Prop.forAll
import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}
import _root_.org.specs.ScalaCheck

class PrintingTest extends Runner(PrintingSpec) with JUnit
object PrintingSpec extends Specification with JValueGen with ScalaCheck {
  import JsonAST._
  import scala.text.Document

  "rendering does not change semantics" in {
    val rendering = (json: Document) => parse(Printer.pretty(json)) == parse(Printer.compact(json))
    forAll(rendering) must pass
  }

  private def parse(json: String) = scala.util.parsing.json.JSON.parse(json)

  implicit def arbDoc: Arbitrary[Document] = Arbitrary(genJValue.map(render(_)))
}
