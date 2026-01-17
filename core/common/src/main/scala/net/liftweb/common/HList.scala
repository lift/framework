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

/**
 * Basic support for heterogeneous lists, aka
 * [[http://apocalisp.wordpress.com/2010/07/06/type-level-programming-in-scala-part-6a-heterogeneous-list%C2%A0basics/ HLists]].
 *
 * An `HList` can be constructed like so:
 *
 * {{{
 * import net.liftweb.common.HLists._
 * 
 * trait Base
 * case class Type1(value: String) extends Base
 * case class Type2(otherValue: String) extends Base
 *
 * val myHList = Type1("Value") :+: Type2("Other Value") :+: HNil
 * myHList match {
 *   case firstThing :+: secondThing :+: HNil =>
 *     println(firstThing.value)
 *     println(secondThing.otherValue)
 * }
 * }}}
 *
 * Above, we see that the `HList` preserved the value of the types of its
 * members, otherwise we wouldn't have been able to fetch `value` and
 * `otherValue`, respectively.
 *
 * Trying the same thing with a list won't work:
 *
 * {{{
 * val myList = Type1("Value") :: Type2("Other Value") :: Nil
 * myList match {
 *   case firstThing :: secondThing :: Nil =>
 *     // error: value value is not a member of Product with Serializable with Base
 *     println(firstThing.value)
 * }
 * }}}
 *
 * This is because `value` is not defined in `Base`. The inferred type of the
 * `List` has to be a common ancestor class or trait of `Type1` and `Type2`, and
 * no such type has a `value` method.
 */
object HLists {

  /**
   * The base trait for `HList`s. Functions that take `HList`s will need a type
   * parameter subtype of `HList`:
   *
   * {{{
   * def myHListFunction[T <: HList](list: HList) = {
   *   println(s"This HList has \${list.length} items!")
   * }
   * }}}
   */
  sealed trait HList

  /**
   * The last element of an `HList`. This is the starting point for an `HList`,
   * and you can use `[[HListMethods.:+: :+:]]` to start one based on it:
   *
   * {{{
   * scala> Type1("Value") :+: HNil
   * res0: net.liftweb.common.HLists.HCons[Type1,net.liftweb.common.HLists.HNil] = Type1(Value) :+: HNil
   * }}}
   */
  final class HNil extends HList {
    override def toString = "HNil"
  }

  /**
   * The HNil singleton.
   */
  val HNil = new HNil()

  /**
   * The `HList` cons cell, which represents one part of an `HList` in linked
   * list style.
   *
   * Carries the information about the type of this element, plus the `HList`
   * type of the rest of the list.
   *
   * You can use `[[HListMethods.:+: :+:]]` to make this `HList` longer:
   *
   * {{{
   * scala> val first = Type1("Value") :+: HNil
   * first: net.liftweb.common.HLists.HCons[Type1,net.liftweb.common.HLists.HNil] = Type1(Value) :+: HNil
   * scala> Type2("Other Value") :+: first
   * res0: net.liftweb.common.HLists.HCons[Type2,
   *         net.liftweb.common.HLists.HCons[Type1,
   *           net.liftweb.common.HLists.HNil]] =
   *       Type2(Other Value) :+: Type1(Value) :+: HNil
   * }}}
   */
  final case class :+:[+H, +T <: HList](head: H, tail: T) extends HList {
    override def toString = s"$head :+: $tail"
  }

  /**
   * Provides the methods that can be used on an `HList`. These are set apart
   * here due to certain issues we can experience otherwise with the type variance
   * on the `:+:` class.
   */
  implicit final class HListMethods[ListSoFar <: HList](hlist: ListSoFar) extends AnyRef {
    def :+:[T](v: T): :+:[T, ListSoFar] = {
      HLists.:+:(v, hlist)
    }

    /**
     * The length of this HList; note that this is O(n) in the list of elements.
     */
    def length: Int = {
      hlist match {
        case HNil =>
          0
        case head :+: rest =>
          1 + rest.length
      }
    }
  }
}

/**
 * Encoding for "A is not a subtype of B".
 */
sealed trait ExcludeThisType[A, B]

/**
 * The companion object to `ExcludeThisType`. This allows one of specify that a
 * type is not a subtype of another type.
 *
 * Based on work by Miles Sabin.
 */
object ExcludeThisType {
  def unexpected: Nothing = sys.error("Unexpected invocation")

  // Uses ambiguity to rule out the cases we're trying to exclude
  implicit def nsub[A, B]: A ExcludeThisType B = null

  implicit def `This type was excluded because it was explicitly excluded`[A, B >: A]: A ExcludeThisType B = unexpected

  implicit def `Ignore me, I only exist to cause the compiler to fail`[A, B >: A]: A ExcludeThisType B = unexpected

  // Type alias for context bound
  type exclude[T] = {
    type other[U] = U ExcludeThisType T
  }
}



