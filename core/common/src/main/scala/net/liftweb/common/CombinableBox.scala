/*
 * Copyright 2010-2011 WorldWide Conferencing, LLC
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

import scala.language.implicitConversions

/**
 * Via an `[[HLists.HList HList]]` containing a collection of `[[Box]]`, either generates an
 * `HList` of the things (unboxed) or a `List[Failure]`.
 *
 * Usage of this is in two parts: first, you import `CombinableBox._`; then, you
 * can use the `:&:` operator to build up your `HList`. For example:
 *
 * {{{
 * scala> import net.liftweb.common._; import CombinableBox._
 * scala> Full("test") :&: Full("second test")
 * res0: net.liftweb.common.CombinableBox.Result[String :+: String :+: net.liftweb.common.HLists.HNil] = Right(test :+: second test :+: HNil)
 * scala> Full("test") :&: Failure("boom")
 * res1: net.liftweb.common.CombinableBox.Result[String :+: Nothing :+: net.liftweb.common.HLists.HNil] = Left(List(Failure(boom,Empty,Empty)))
 * scala> Failure("boom") :&: Failure("boom boom")
 * res2: net.liftweb.common.CombinableBox.Result[Nothing :+: Nothing :+: net.liftweb.common.HLists.HNil] = Left(List(Failure(boom,Empty,Empty), Failure(boom boom,Empty,Empty)))
 * }}}
 *
 * `CombinableBox` also contains some implicits to help deal with the outcome.
 * By default, results are represented by an `Either[List[Failure], <HList type>]`.
 * Instead, we can convert this back to a `Box` representation:
 *
 * {{{
 * scala> import net.liftweb.common.HLists._
 * scala> type ExpectedHList = String :+: String :+: HNil
 * defined type alias ExpectedHList
 * scala> val combinedFulls: Box[ExpectedHList] = Full("test") :&: Full("second test")
 * combinedFulls: net.liftweb.common.Box[ExpectedHList] = Full(test :+: second test :+: HNil)
 * scala> val combinedFailure: Box[ExpectedHList] = Full("test") :&: (Failure("boom"): Box[String])
 * combinedFailure: net.liftweb.common.Box[ExpectedHList] = Failure(boom,Empty,Empty)
 * scala> val combinedFailures: Box[ExpectedHList] = (Failure("boom"): Box[String]) :&: (Failure("boom boom"): Box[String])
 * combinedFailures: net.liftweb.common.Box[ExpectedHList] = ParamFailure(Multiple Failures, Empty, Empty, FailureList(List(Failure(boom,Empty,Empty), Failure(boom boom,Empty,Empty))))
 * }}}
 *
 * Note that in case multiple failures are involved, we generate a `ParamFailure`
 * whose parameter is a `FailureList` object, which contains the failures found
 * along the way.
 *
 * In many cases, it is more straightforward to use `ListOfBoxes` to deal with a
 * list of `Box` that needs to be a `Box` of `List` instead. It works in a very
 * similar way, but works on regular lists instead of ``HList``s. `CombinableBox`
 * is best reserved for when you want to preserve heterogeneous types out of a
 * set of boxes.
 */
object CombinableBox {
  import HLists._

  type Result[A] = Either[List[Failure], A]

  private implicit def emptyBoxToFailure(eb: EmptyBox): Failure = eb match {
    case (f: Failure) => f
    case Empty        => Failure("Empty")
  }

  implicit def boxToCombinableBox[A](in: Box[A]): CombinableBox[A, HNil] = CombinableBox(in match {
    case Full(a)      => Right(a :+: HNil)
    case (f: Failure) => Left(f :: Nil)
    case _            => Left(Failure("Empty") :: Nil)
  })

  implicit def boxableToCombinableBox[A](in: Boxable[A]): CombinableBox[A, HNil] = in.asBox

  implicit def boxCombinationToCombinableBox[A, B <: HList](in: Result[A :+: B]): CombinableBox[A, B] =
    CombinableBox(in)

  implicit def resultToBox[A](result: Result[A]): Box[A] = result match {
    case Left(Nil) => Empty
    case Left(f :: Nil) => f
    case Left(f) => new ParamFailure("Multiple Failures",
                                     Empty,
                                     Empty,
                                     FailureList(f))
    case Right(x) => Full(x)
  }

  /**
   * If the `[[Failure]]` is going to be condensed, a `FailureList` is generated
   * to allow type-safe pattern matches without worrying about erasure.
   */
  final case class FailureList(failures: List[Failure])

  /**
   * The place where the results are accumulated.
   */
  final case class CombinableBox[B, C <: HList](rhs: Result[B :+: C]) {
    def :&: [A](lhs: Boxable[A]): Result[A :+: B :+: C] = this.:&:(lhs.asBox)
    def :&: [A](lhs: Box[A]): Result[A :+: B :+: C] =
      (lhs, rhs) match {
        case (failure: EmptyBox, Left(failures)  ) => Left(failure :: failures)
        case (failure: EmptyBox, _               ) => Left(failure :: Nil)
        case (_,                 Left(failures)  ) => Left(failures)
        case (Full(success),     Right(successes)) => Right(success :+: successes)
      }
  }

}

