package net.liftweb.json

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

trait JValueGen {
  import JsonAST._

  def genJValue: Gen[JValue] = frequency((5, genSimple), (1, wrap(genArray)), (1, wrap(genObject)))
  def genSimple: Gen[JValue] = oneOf(
    value(JNull), 
    arbitrary[Int].map(JInt(_)),
    arbitrary[Double].map(JDouble(_)),
    arbitrary[Boolean].map(JBool(_)),
    arbitrary[String].map(JString(_)))

  def genArray: Gen[JValue] = for (l <- genList) yield JArray(l)
  def genObject: Gen[JObject] = for (l <- genFieldList) yield JObject(l)

  def genList = Gen.containerOfN[List, JValue](listSize, genJValue)
  def genFieldList = Gen.containerOfN[List, JField](listSize, genField)
  def genField = for (name <- identifier; value <- genJValue; id <- choose(0, 1000000)) yield JField(name+id, value)

  def genJValueClass: Gen[Class[_ <: JValue]] = oneOf(
    JNull.getClass.asInstanceOf[Class[JValue]], JNothing.getClass.asInstanceOf[Class[JValue]], classOf[JInt], 
    classOf[JDouble], classOf[JBool], classOf[JString], classOf[JField], classOf[JArray], classOf[JObject])

  def listSize = choose(0, 5).sample.get
}

trait NodeGen {
  import Xml.{XmlNode, XmlElem}
  import scala.xml.{Node, NodeSeq, Text}

  def genXml: Gen[Node] = frequency((2, wrap(genNode)), (3, genElem))
  
  def genNode = for {
    name <- genName
    node <- Gen.containerOfN[List, Node](children, genXml) map { seq => new XmlNode(name, seq) }
  } yield node

  def genElem = for {
    name <- genName
    value <- arbitrary[String]
  } yield new XmlElem(name, value)

  def genName = frequency((2, identifier), (1, value("const")))
  private def children = choose(1, 3).sample.get
}
