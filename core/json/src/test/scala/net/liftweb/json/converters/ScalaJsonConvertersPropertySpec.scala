package net.liftweb.json
package converters

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck._
  import Arbitrary._
  import Prop.{forAll, forAllNoShrink}
import scalajson.ast.unsafe

class ScalaJsonConvertersPropertySpec extends Specification with ScalaCheck with JValueGen {
  "Conversions" should {
    "from Lift" should {
      "be reversable" in {
        forAll(genJValue) { inputValue =>
          val scalaValue = LiftToScalaJsonConversions.toScalaAST(inputValue).get
          val reversedValue = ScalaJsonToLiftConversions.toLiftAST(scalaValue)

          inputValue must_== reversedValue
        }
      }
    }

    "from Scala" should {
      "be reversable" in {
        forAll(ScalaJsonGen.topJValue) { inputValue =>
          val liftValue = ScalaJsonToLiftConversions.toLiftAST(inputValue)
          val scalaValue = LiftToScalaJsonConversions.toScalaAST(liftValue).get

          // We have to provide special handling for arrays. It seems scalatest
          // is smart enough to do the Right Thing™ if the top type of your
          // should is actually an Array, but if you're comparing a case class
          // that contains an Array the bad things happen?
          (inputValue, scalaValue) match {
            case (unsafe.JObject(inValues), unsafe.JObject(outValues)) =>
              inValues must_== outValues

            case (unsafe.JArray(inValues), unsafe.JArray(outValues)) =>
              inValues must_== outValues

            case _ =>
              inputValue must_== scalaValue
          }
        }
      }
    }
  }
}
