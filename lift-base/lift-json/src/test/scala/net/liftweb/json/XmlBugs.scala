package net.liftweb.json

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

class XmlBugsTest extends Runner(XmlBugs) with JUnit
object XmlBugs extends Specification {
  import JsonAST._
  import Xml._
  import scala.xml.{Group, Text}

  "HarryH's XML parses correctly" in {
    val xml1 = <venue><id>123</id></venue>
    val xml2 = <venue> <id>{"1"}{"23"}</id> </venue>
    Xml.toJson(xml1) must_== Xml.toJson(xml2)
  }

  "HarryH's XML with attributes parses correctly" in {
    val json = toJson(<tips><group type="Nearby"><tip><id>10</id></tip></group></tips>)
    Printer.compact(render(json)) mustEqual """{"tips":{"group":{"type":"Nearby","tip":{"id":"10"}}}}"""
  }
}
