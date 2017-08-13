package net.liftweb.json
package converters

import org.specs2.mutable._

import ScalaJsonConverters._

import scalajson.ast
import scalajson.ast.unsafe

class ScalaJsonConvertersSpec extends Specification {
  "JsonConverters" should {
    "work on lift-json JValues" in {
      liftJsonRepr.toStandardScalaAST.get must_== standardJsonRepr
      liftJsonRepr.toUnsafeScalaAST.get must_== unsafeJsonRepr
    }

    "work on scala standard JValues" in {
      unsafeJsonRepr.toLiftAST must_== liftJsonRepr
    }

    "work on scala unsafe JValues" in {
      standardJsonRepr.toLiftAST must_== liftJsonRepr
    }
  }

  val liftJsonRepr = JObject(
    JField("name", JString("Matt Farmer")) ::
    JField("occupation", JString("Magician")) ::
    JField("age", JInt(27)) ::
    Nil
  )

  val standardJsonRepr = ast.JObject(Map(
    "name" -> ast.JString("Matt Farmer"),
    "occupation" -> ast.JString("Magician"),
    "age" -> ast.JNumber(27)
  ))

  val unsafeJsonRepr = unsafe.JObject(Array(
    unsafe.JField("name", unsafe.JString("Matt Farmer")),
    unsafe.JField("occupation", unsafe.JString("Magician")),
    unsafe.JField("age", unsafe.JNumber("27"))
  ))
}
