/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb
package json

import org.scalacheck._
  import rng.Seed
import Gen._
import Arbitrary.arbitrary

trait JValueGen {
  def genJValue: Gen[JValue] = frequency((5, genSimple), (1, delay(genArray)), (1, delay(genObject)))
  def genSimple: Gen[JValue] = oneOf(
    const(JNull),
    arbitrary[Int].map(JInt(_)),
    arbitrary[Double].map(JDouble(_)),
    arbitrary[Boolean].map(JBool(_)),
    arbitrary[String].map(JString(_)))

  def genArray: Gen[JValue] = for (l <- genList) yield JArray(l)
  def genObject: Gen[JObject] = for (l <- genFieldList) yield JObject(l)

  implicit def cogenJValue: Cogen[JValue] =
    Cogen[JValue]({ (seed: Seed, jvalue: JValue) =>
      jvalue match {
        case JNothing => implicitly[Cogen[Unit]].perturb(seed, ())
        case JNull => implicitly[Cogen[Unit]].perturb(seed, ())
        case JInt(value) => implicitly[Cogen[BigInt]].perturb(seed, value)
        case JDouble(value) => implicitly[Cogen[Double]].perturb(seed, value)
        case JString(value) => implicitly[Cogen[String]].perturb(seed, value)
        case JBool(value) => implicitly[Cogen[Boolean]].perturb(seed, value)
        case JArray(value) => implicitly[Cogen[List[JValue]]].perturb(seed, value)
        case JObject(value) => implicitly[Cogen[List[JField]]].perturb(seed, value)
      }
    })
  implicit def cogenJField: Cogen[JField] =
    Cogen[JField]({ (seed: Seed, field: JField) =>
      Cogen.perturbPair(seed, (field.name -> field.value))
    })

  val genJValueFn: Gen[JValue=>JValue] = function1(genJValue)

  def genList = Gen.containerOfN[List, JValue](listSize, genJValue)
  def genFieldList = Gen.containerOfN[List, JField](listSize, genField)
  def genField = for (name <- identifier; value <- genJValue; id <- choose(0, 1000000)) yield JField(name+id, value)

  def genJValueClass: Gen[Class[_ <: JValue]] = oneOf(
    JNull.getClass.asInstanceOf[Class[JValue]], JNothing.getClass.asInstanceOf[Class[JValue]], classOf[JInt],
    classOf[JDouble], classOf[JBool], classOf[JString], classOf[JArray], classOf[JObject])

  def listSize = choose(0, 5).sample.get
}

trait NodeGen {
  import Xml.{XmlNode, XmlElem}
  import scala.xml.{Node, NodeSeq, Text}

  def genXml: Gen[Node] = frequency((2, delay(genNode)), (3, genElem))

  def genNode = for {
    name <- genName
    node <- Gen.containerOfN[List, Node](children, genXml) map { seq => new XmlNode(name, seq) }
  } yield node

  def genElem = for {
    name <- genName
    value <- arbitrary[String]
  } yield new XmlElem(name, value)

  def genName = frequency((2, identifier), (1, const("const")))
  private def children = choose(1, 3).sample.get
}
