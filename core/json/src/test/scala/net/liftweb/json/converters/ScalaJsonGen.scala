package net.liftweb.json.converters

import org.scalacheck.Gen

import scalajson.ast.unsafe._

object ScalaJsonGen {
  def jString = for {
    innerString <- Gen.alphaStr
  } yield {
    JString(innerString)
  }

  def jNumber = for {
    innerNumber <- Gen.chooseNum(Integer.MIN_VALUE, Integer.MAX_VALUE)
  } yield {
    JNumber(innerNumber.toString)
  }

  def jBool = for {
    bool <- Gen.oneOf(JTrue, JFalse)
  } yield {
    bool
  }

  val jNull = Gen.const(JNull)

  def jArray = for {
    innerValues <- Gen.containerOf[Array, JValue](jValue)
  } yield {
    JArray(innerValues)
  }

  def jField = for {
    fieldName <- Gen.alphaStr
    fieldValue <- jValue
  } yield {
    JField(fieldName, fieldValue)
  }

  def jObject = for {
    fields <- Gen.containerOf[Array, JField](jField)
  } yield {
    JObject(fields)
  }

  def jValue: Gen[JValue] = Gen.oneOf(jString, jNumber, jBool, jNull)

  def topJValue: Gen[JValue] = Gen.oneOf(jString, jNumber, jBool, jNull, jArray, jObject)
}
