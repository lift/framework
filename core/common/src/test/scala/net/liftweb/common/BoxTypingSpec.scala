/*
 * Copyright 2007-2011 WorldWide Conferencing, LLC
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
package common

import org.specs2.mutable.Specification

/**
 * Verifications for complex Box typing like PresenceBox flatMap T=>PresenceBox
 * yields PresenceBox, etc.
 */
class BoxTypingSpec extends Specification {
  //     IMPORTANT
  // We use a few implicits to help us do compile-time proofs of the inferred
  // compiler types, but if you choose to edit this spec make sure you don't
  // lose typing information during the checks! e.g., if we want to verify that
  // fullBox.operation(...) is only a TryBox and nothing else, and we put that
  // operation result in a List with other operation results, we're no longer
  // checking that operation's result, but rather the supertype of all the
  // results of the operations!
  //
  // As a result, you'll see a bit more repetition below than may seem ideal.

  // Implicit helpers to ask Scala to help us prove the compiler inferred the
  // right type for method returns.
  trait PlainBoxProof { def plainBox_?(): Boolean }
  implicit class DisprovePlainBox[T](notSpecificBox: Box[T]) extends PlainBoxProof {
    def plainBox_? = true
  }
  trait FullProof { def full_?(): Boolean }
  implicit class ProveFull[T](full: Full[T]) extends FullProof with PlainBoxProof {
    def full_? = true
    def plainBox_? = false
  }
  implicit class DisproveFull[T](notFull: Box[T]) extends FullProof {
    def full_? = false
  }
  trait EmptyProof { def empty_?(): Boolean }
  implicit class ProveEmpty(empty: Empty.type) extends EmptyProof with PlainBoxProof {
    def empty_? = true
    def plainBox_? = false
  }
  implicit class DisproveEmpty[T](notEmpty: Box[T]) extends EmptyProof {
    def empty_? = false
  }
  trait FailureProof { def failure_?(): Boolean }
  implicit class ProveFailure(failure: Failure) extends FailureProof with PlainBoxProof {
    def failure_? = true
    def plainBox_? = false
  }
  implicit class DisproveFailure[T](notFailure: Box[T]) extends FailureProof {
    def failure_? = false
  }
  trait ParamFailureProof { def paramFailure_?(): Boolean }
  implicit class ProveParamFailure[A](paramFailure: ParamFailure[A]) extends ParamFailureProof with PlainBoxProof {
    def paramFailure_? = true
    def plainBox_? = false
  }
  implicit class DisproveParamFailure[T](notFailure: Box[T]) extends ParamFailureProof {
    def paramFailure_? = false
  }
  trait  TryProof { def try_?(): Boolean }
  implicit class ProveTry[T](`try`: TryBox[T]) extends TryProof with PlainBoxProof {
    def try_? = true
    def plainBox_? = false
  }
  implicit class DisproveTry[T](notTry: Box[T]) extends TryProof {
    def try_? = false
  }
  trait  ParamTryProof { def paramTry_?(): Boolean }
  implicit class ProveParamTry[T](`try`: ParamTryBox[T, Double]) extends ParamTryProof with PlainBoxProof {
    def paramTry_? = true
    def plainBox_? = false
  }
  implicit class DisproveParamTry[T](notParamTry: Box[T]) extends ParamTryProof {
    def paramTry_? = false
  }
  trait  PresenceProof { def presence_?(): Boolean }
  implicit class ProvePresence[T](presence: PresenceBox[T]) extends PresenceProof with PlainBoxProof {
    def presence_? = true
    def plainBox_? = false
  }
  implicit class DisprovePresence[T](notPresence: Box[T]) extends PresenceProof {
    def presence_? = false
  }

  "Box when doing complex typing invocations" should {
    val fullBox: Full[Int] = Full(5)
    val fullParamTry: ParamTryBox[Int, Double] = fullBox
    val fullTry: TryBox[Int] = fullBox
    val fullPresence: PresenceBox[Int] = fullBox
    val fullPlain: Box[Int] = fullBox

    val emptyPresence: PresenceBox[Int] = Empty
    val emptyPlain: Box[Int] = Empty

    val failureBox: Failure = Failure("boom")
    val failureTry: TryBox[Int] = failureBox
    val failurePlain: Box[Int] = failureBox

    val paramFailureBox: ParamFailure[Double] = failureBox ~> 2.0
    val paramFailureParamTry: ParamTryBox[Int, Double] = paramFailureBox
    val paramFailureTry: TryBox[Int] = paramFailureBox
    val paramFailurePlain: Box[Int] = paramFailureBox

    val fullStringBox: Full[String] = Full("boom")
    val fullStringParamTry: ParamTryBox[String, Double] = fullStringBox
    val fullStringTry: TryBox[String] = fullStringBox
    val fullStringPresence: PresenceBox[String] = fullStringBox
    val fullStringPlain: Box[String] = fullStringBox

    val emptyStringPresence: PresenceBox[String] = Empty
    val emptyStringPlain: Box[String] = Empty

    val failureStringTry: TryBox[String] = failureBox
    val failureStringPlain: Box[String] = failureBox

    val paramFailureStringParamTry: ParamTryBox[String, Double] = paramFailureBox
    val paramFailureStringTry: TryBox[String] = paramFailureBox
    val paramFailureStringPlain: Box[String] = paramFailureBox

    "flatMap outer Full correctly" in {
      fullBox.flatMap(_ => fullStringBox).try_? must beTrue
      fullBox.flatMap(_ => fullStringBox).paramTry_? must beTrue
      fullBox.flatMap(_ => fullStringBox).presence_? must beTrue

      fullParamTry.flatMap(_ => fullStringBox).try_? must beTrue
      fullParamTry.flatMap(_ => fullStringBox).paramTry_? must beTrue
      fullParamTry.flatMap(_ => fullStringBox).presence_? must beFalse

      fullTry.flatMap(_ => fullStringBox).try_? must beTrue
      fullTry.flatMap(_ => fullStringBox).paramTry_? must beFalse
      fullTry.flatMap(_ => fullStringBox).presence_? must beFalse

      fullPresence.flatMap(_ => fullStringBox).try_? must beFalse
      fullPresence.flatMap(_ => fullStringBox).paramTry_? must beFalse
      fullPresence.flatMap(_ => fullStringBox).presence_? must beTrue

      fullPlain.flatMap(_ => fullStringBox).try_? must beFalse
      fullPlain.flatMap(_ => fullStringBox).paramTry_? must beFalse
      fullPlain.flatMap(_ => fullStringBox).presence_? must beFalse

      fullBox.flatMap(_ => fullStringBox) must_== fullStringBox
      fullParamTry.flatMap(_ => fullStringBox) must_== fullStringBox
      fullTry.flatMap(_ => fullStringBox) must_== fullStringBox
      fullPresence.flatMap(_ => fullStringBox) must_== fullStringBox
      fullPlain.flatMap(_ => fullStringBox) must_== fullStringBox
    }

    "flatMap outer Empty correctly" in {
      Empty.flatMap(_ => fullStringBox).try_? must beFalse
      Empty.flatMap(_ => fullStringBox).paramTry_? must beFalse
      Empty.flatMap(_ => fullStringBox).presence_? must beTrue

      emptyPresence.flatMap(_ => fullStringBox).try_? must beFalse
      emptyPresence.flatMap(_ => fullStringBox).paramTry_? must beFalse
      emptyPresence.flatMap(_ => fullStringBox).presence_? must beTrue

      emptyPlain.flatMap(_ => fullStringBox).try_? must beFalse
      emptyPlain.flatMap(_ => fullStringBox).paramTry_? must beFalse
      emptyPlain.flatMap(_ => fullStringBox).presence_? must beFalse

      Empty.flatMap(_ => fullStringBox) must_== Empty
      emptyPresence.flatMap(_ => fullStringBox) must_== Empty
      emptyPlain.flatMap(_ => fullStringBox) must_== Empty
    }

    "flatMap outer Failure correctly" in {
      failureBox.flatMap(_ => fullStringBox).try_? must beTrue
      failureBox.flatMap(_ => fullStringBox).paramTry_? must beFalse
      failureBox.flatMap(_ => fullStringBox).presence_? must beFalse

      failureTry.flatMap(_ => fullStringBox).try_? must beTrue
      failureTry.flatMap(_ => fullStringBox).paramTry_? must beFalse
      failureTry.flatMap(_ => fullStringBox).presence_? must beFalse

      failurePlain.flatMap(_ => fullStringBox).try_? must beFalse
      failurePlain.flatMap(_ => fullStringBox).paramTry_? must beFalse
      failurePlain.flatMap(_ => fullStringBox).presence_? must beFalse

      failureBox.flatMap(_ => fullStringBox) must_== failureBox
      failureTry.flatMap(_ => fullStringBox) must_== failureBox
      failurePlain.flatMap(_ => fullStringBox) must_== failureBox
    }

    "flatMap outer ParamFailure correctly" in {
      paramFailureBox.flatMap(_ => fullStringBox).try_? must beTrue
      paramFailureBox.flatMap(_ => fullStringBox).paramTry_? must beTrue
      paramFailureBox.flatMap(_ => fullStringBox).presence_? must beFalse

      paramFailureParamTry.flatMap(_ => fullStringBox).try_? must beTrue
      paramFailureParamTry.flatMap(_ => fullStringBox).paramTry_? must beTrue
      paramFailureParamTry.flatMap(_ => fullStringBox).presence_? must beFalse

      paramFailureTry.flatMap(_ => fullStringBox).try_? must beTrue
      paramFailureTry.flatMap(_ => fullStringBox).paramTry_? must beFalse
      paramFailureTry.flatMap(_ => fullStringBox).presence_? must beFalse

      paramFailurePlain.flatMap(_ => fullStringBox).try_? must beFalse
      paramFailurePlain.flatMap(_ => fullStringBox).paramTry_? must beFalse
      paramFailurePlain.flatMap(_ => fullStringBox).presence_? must beFalse

      paramFailureBox.flatMap(_ => fullStringBox) must_== paramFailureBox
      paramFailureParamTry.flatMap(_ => fullStringBox) must_== paramFailureBox
      paramFailureTry.flatMap(_ => fullStringBox) must_== paramFailureBox
      paramFailurePlain.flatMap(_ => fullStringBox) must_== paramFailureBox
    }

    "filter Empty correctly" in {
      emptyPresence.filter(_ == 5).presence_? must beTrue
      emptyPresence.filter(_ == 5).paramTry_? must beFalse
      emptyPresence.filter(_ == 5).try_? must beFalse

      emptyPlain.filter(_ == 5).presence_? must beFalse
      emptyPlain.filter(_ == 5).paramTry_? must beFalse
      emptyPlain.filter(_ == 5).try_? must beFalse
    }

    "filter Failure correctly" in {
      failureBox.filter(_ == 5).presence_? must beFalse
      failureBox.filter(_ == 5).paramTry_? must beFalse
      failureBox.filter(_ == 5).try_? must beTrue
    }

    "filter ParamFailure correctly" in {
      paramFailureBox.filter(_ == 5).presence_? must beFalse
      paramFailureBox.filter(_ == 5).paramTry_? must beTrue
      paramFailureBox.filter(_ == 5).try_? must beTrue
    }

    "when filtering" in {
      // For a Full/Empty, filter leads to PresenceBox, otherwise it's just Box.
      "filter a known Full to PresenceBox" in {
        fullBox.filter(_ == 7).presence_? must beTrue
        fullBox.filter(_ == 7).paramTry_? must beFalse
        fullBox.filter(_ == 7).try_? must beFalse
      }

      "filter a PresenceBox to PresenceBox" in {
        fullPresence.filter(_ == 7).presence_? must beTrue
        fullPresence.filter(_ == 7).paramTry_? must beFalse
        fullPresence.filter(_ == 7).try_? must beFalse

        emptyPresence.filter(_ == 7).presence_? must beTrue
        emptyPresence.filter(_ == 7).paramTry_? must beFalse
        emptyPresence.filter(_ == 7).try_? must beFalse
      }

      // In order for Full to be able to return a PresenceBox for filter, the
      // other types (TryBox, etc) need to return Box (since Full extends those
      // as well). However, if we know we are looking at a Failure or
      // ParamFailure, we can be specific.
      "filter known Failures/ParamFailures to themselves" in {
        paramFailureBox.filter(_ == 7).presence_? must beFalse
        paramFailureBox.filter(_ == 7).paramTry_? must beTrue
        paramFailureBox.filter(_ == 7).try_? must beTrue

        failureBox.filter(_ == 7).presence_? must beFalse
        failureBox.filter(_ == 7).paramTry_? must beFalse
        failureBox.filter(_ == 7).try_? must beTrue
      }

      "filter all other types to Box" in {
        fullParamTry.filter(_ == 7).plainBox_? must beTrue
        fullTry.filter(_ == 7).plainBox_? must beTrue
        fullPlain.filter(_ == 7).plainBox_? must beTrue

        emptyPlain.filter(_ == 7).plainBox_? must beTrue

        paramFailurePlain.filter(_ == 7).plainBox_? must beTrue
        paramFailureParamTry.filter(_ == 7).plainBox_?
        paramFailureTry.filter(_ == 7).plainBox_?

        failurePlain.filter(_ == 7).plainBox_? must beTrue
        failureTry.filter(_ == 7).plainBox_? must beTrue
      }

      "filter correctly regardless of type" in {
        List(fullBox, fullParamTry, fullTry, fullPresence, fullPlain)
          .map(_.filter(_ == 7))
          .forall(_ == Empty) must beTrue

        List(fullBox, fullParamTry, fullTry, fullPresence, fullPlain)
          .map(_.filter(_ == 5))
          .exists(_ == Empty) must beFalse
      }
    }

    "when or-ing" should {
      "or known Fulls with all other types preserving the Fullness" in {
        (fullBox or failureBox).full_? must beTrue
        (fullBox or failureTry).full_? must beTrue
        (fullBox or paramFailureBox).full_? must beTrue
        (fullBox or paramFailureParamTry).full_? must beTrue
        (fullBox or emptyPresence).full_? must beTrue
        (fullBox or emptyPlain).full_? must beTrue
      }

      "or Empties with other types preserving the supertypes" in {
        (Empty or fullBox).full_? must beTrue
        (Empty or paramFailureParamTry).paramTry_? must beTrue
        (Empty or failureBox).failure_? must beFalse
        (Empty or failureBox).try_? must beTrue
        (Empty or failureTry).try_? must beTrue
        (Empty or paramFailureBox).paramFailure_? must beFalse
        (Empty or paramFailureBox).paramTry_? must beTrue
      }

      "or Failures with other types preserving the supertypes" in {
        (failureBox or fullBox).full_? must beTrue
        (failureBox or paramFailureParamTry).paramTry_? must beTrue
        (failureBox or failureBox).failure_? must beFalse
        (failureBox or failureBox).try_? must beTrue
        (failureBox or failureTry).try_? must beTrue
        (failureBox or paramFailureBox).paramFailure_? must beFalse
        (failureBox or paramFailureBox).paramTry_? must beTrue
      }

      "or ParamFailures with other types preserving the supertypes" in {
        (paramFailureBox or fullBox).full_? must beTrue
        (paramFailureBox or paramFailureParamTry).paramTry_? must beTrue
        (paramFailureBox or failureBox).failure_? must beFalse
        (paramFailureBox or failureBox).try_? must beTrue
        (paramFailureBox or failureTry).try_? must beTrue
        (paramFailureBox or paramFailureBox).paramFailure_? must beFalse
        (paramFailureBox or paramFailureBox).paramTry_? must beTrue
      }

      "or plain Box with other types to Box" in {
        (emptyPlain or fullBox).plainBox_? must beTrue
        (emptyPlain or Empty).plainBox_? must beTrue
        (emptyPlain or failureBox).plainBox_? must beTrue
        (emptyPlain or paramFailureBox).plainBox_? must beTrue
      }
    }

    "when isA/asAing" should {
      // Full [ai]sA -> PresenceBox
      // Empty [ai]sA -> PresenceBox
      // Failure/ParamFailure [ai]sA -> Failure
      // Box [ai]sA -> Box
      0 must_== 0
    }

    // collect/collectFirst go PresenceBox for Full/Empty, Box for everything else
    // flattenPresence/flattenTry
    // for comprehensions
  }
}
