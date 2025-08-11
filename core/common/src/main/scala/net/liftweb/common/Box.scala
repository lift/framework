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

import scala.language.implicitConversions
import scala.language.existentials
import scala.reflect.Manifest

import java.util.{Iterator => JavaIterator, ArrayList => JavaArrayList}

/**
 * A bridge to make using Lift `[[Box]]` from Java easier.
 *
 * In particular, provides access to the `Box` companion object so that
 * functions like `[[Box$.legacyNullTest legacyNullTest]]` can be used easily
 * from Java, as well as access to the `[[Empty]]` singleton so that empty
 * values can be created easily from Java.
 */
class BoxJBridge {
  /**
   * Get the Box companion object
   */
  def box: BoxTrait = Box

  /**
   * Get the `[[Empty]]` singleton.
   */
  def empty: EmptyBox = Empty
}

/**
 * The Box companion object provides methods to create a Box from:
 *  - an `[[scala.Option Option]]`
 *  - a `[[scala.collection.immutable.List List]]`
 *  - any `[[scala.AnyRef AnyRef]]` object, converting `null` to `[[Empty]]` and
 *    anything else to a `[[Full]]` with the given object
 *
 * It also provides implicit methods to transform `Option` to `Box`, `Box` to
 * `[[scala.collection.Iterable Iterable]]`, and `Box` to `Option`.
 */
object Box extends BoxTrait with Tryo {
  /**
   * Helper class to provide an easy way for converting a `List[Box[T]]` into
   * a `Box[List[T]]`.
  **/
  implicit class ListOfBoxes[T](val theListOfBoxes: List[Box[T]]) extends AnyVal {
    /**
     * Convert a `List` of `Box`es into a single `Box` containting a `List[T]`,
     * where `T` is the parameterized type of the `Box`es.
     *
     * This method is useful for those cases where you have a lot of operations
     * being executed that all return some `Box[T]`. You want just a `List[T]`
     * if all of those operations succeeded, but you don't want to have
     * Failures disappear if any were present in the list.
     *
     * If all of the `Box`es in the `List` are `Full` or `Empty`, we return a
     * `Full` box containing a `List` of all of the `Full` `Box` values that
     * were present. If any of the `Box`es contain a `Failure`, a
     * `ParamFailure` is returned, containing the original `List[Box[T]]` as
     * the param. The `ParamFailure` itself is chained to a `Failure` chain
     * containing all of the `Failure` boxes in the list.
     *
     * It is worth noting that the size of the list in the resulting
     * `Box[List[T]]` may not be equal to the size of the `List[Box[T]]` that
     * is fed as `Empty` values will disappear altogether in the conversion.
     *
     * @param failureErrorMessage The string that should be placed in the message for the Failure.
     * @return A `Full[List[T]]` if no `Failure`s were present. `ParamFailure[List[Box[T]]]` otherwise.
    **/
    def toSingleBox(failureErrorMessage: String): Box[List[T]] = {
      if (theListOfBoxes.exists(_.isInstanceOf[Failure])) {
        val failureChain =
          theListOfBoxes.collect {
            case fail: Failure => fail
          }.reduceRight { (topmostFailure, latestFailure) =>
            topmostFailure.copy(chain = Full(latestFailure))
          }

        ParamFailure(
          failureErrorMessage,
          Empty,
          Full(failureChain),
          theListOfBoxes
        )
      } else {
        Full(theListOfBoxes.flatten)
      }
    }
  }

}

private[common] sealed trait OptionImplicits {
  /**
   * This implicit transformation allows one to use an `Option` as a `Box`.
   *
   * @return `Full` with the contents if the `Option` is `Some` and `Empty`
   *         otherwise.
   */
  implicit def option2Box[T](in: Option[T]): Box[T] = Box(in)

  /**
   * This implicit transformation allows one to use a `Box` as an `Option`.
   *
   * Note that `Box` implements `get` specifically to avoid usage of `.get` on
   * `Box` instances. Boxes should be opened using `openOrThrowException` and
   * their contents compared using `== Full(expectedValue)`.
   *
   * @return `Some` with the contents if the box is `Full` and `[[scala.None None]]`
   *         otherwise.
   */
  implicit def box2Option[T](in: Box[T]): Option[T] = in.toOption
}

/**
 * Implementation for the `[[Box$ Box]]` singleton.
 */
sealed trait BoxTrait extends OptionImplicits {
  val primitiveMap: Map[Class[_], Class[_]] = Map(
    java.lang.Boolean.TYPE -> classOf[java.lang.Boolean],
    java.lang.Character.TYPE -> classOf[java.lang.Character],
    java.lang.Byte.TYPE -> classOf[java.lang.Byte],
    java.lang.Double.TYPE -> classOf[java.lang.Double],
    java.lang.Float.TYPE -> classOf[java.lang.Float],
    java.lang.Integer.TYPE -> classOf[java.lang.Integer],
    java.lang.Long.TYPE -> classOf[java.lang.Long],
    java.lang.Short.TYPE -> classOf[java.lang.Short])

  @deprecated("Use the correctly-spelled primitiveMap instead.","3.0")
  val primativeMap = primitiveMap

  /**
   * Create a `Box` from the specified `Option`.
   *
   * @return `Full` with the contents if the `Option` is `Some`
   *         and `Empty` otherwise.
   */
  def apply[T](in: Option[T]) = in match {
    case Some(x) => Full(x)
    case _ => Empty
  }

  /**
   * Create a `Box` from the specified `Box`, checking for `null`.
   *
   * @return `Full(in)` if `in` is a `Full` box and its value is non-null,
   *         `Empty` otherwise.
   */
  def apply[T](in: Box[T]) = in match {
    case Full(x) => legacyNullTest(x)
    case x: EmptyBox => x
    case _ => Empty
  }

  /**
   * Transform a `List` with zero or more elements to a `Box`, losing all but
   * the first element if there are more than one.
   *
   * @return `Full(x)` with the head of the list if it contains at least one
   *         element and `Empty` otherwise.
   */
  def apply[T](in: List[T]) = in match {
    case x :: _ => Full(x)
    case _ => Empty
  }

  /**
   * Apply the specified `PartialFunction` to the specified `value` and return the result
   * in a `Full`; if the `pf`` is not defined at that point return `Empty`.
   *
   * @param pf The partial function to use to transform the value.
   * @param value The value to transform.
   *
   * @return A `Full` containing the transformed value if
   *         `pf.isDefinedAt(value)` and `Empty` otherwise.
   */
  def apply[InType, OutType](pf: PartialFunction[InType, OutType])(value: InType): Box[OutType] = {
    apply(value)(pf)
  }

  /**
   * Apply the specified `PartialFunction` to the specified `value` and return
   * the result in a `Full`; if the `pf`` is not defined at that point return
   * `Empty`.
   *
   * @param pf The partial function to use to transform the value.
   * @param value The value to transform.
   * @return A `Full` containing the transformed value if
   *         `pf.isDefinedAt(value)` and `Empty` otherwise.
   */
  def apply[InType, OutType](value: InType)(pf: PartialFunction[InType, OutType]): Box[OutType] = {
    pf.andThen(Full.apply(_)).applyOrElse(value, (_:  InType) => Empty)
  }

  /**
   * This implicit transformation allows one to use a `Box` as an `Iterable` of
   * zero or one elements.
   *
   * @return A single-element `List` with the contents if the box is `Full`
   *         and `[[scala.collection.immutable.Nil Nil]]` otherwise.
   */
  implicit def box2Iterable[T](in: Box[T]): Iterable[T] = in.toList

  /**
   * This method allows one to encapsulate any object in a Box in a null-safe
   * manner, converting `null` values to `Empty`.
   *
   * @return `Full` if `in` is not null and `Empty` otherwise.
   */
  def legacyNullTest[T](in: T): Box[T] = in match {
    case null => Empty
    case _ => Full(in)
  }

  /**
   * Alias for `[[legacyNullTest]]`.
   */
  def !![T](in: T): Box[T] = legacyNullTest(in)

  /**
   * Create a `Full` box containing the specified value if `in` is an instance of
   * the specified class `clz` and `Empty` otherwise.
   *
   * This is basically a Java-friendly version of `[[asA]]`, which you should
   * prefer when using Scala.
   *
   * For example:
   * {{{
   * scala> Box.isA("boom", classOf[Int])
   * res0: net.liftweb.common.Box[Int] = Empty
   *
   * scala> Box.isA(5, classOf[Int])
   * res1: net.liftweb.common.Box[Int] = Full(5)
   * }}}
   */
  def isA[A, B](in: A, clz: Class[B]): Box[B] = {
    (Box !! in).isA(clz)
  }

  // NOTE: We use an existential type here so that you can invoke asA with
  // just one type parameter. To wit, this lets you do:
  //
  //   Box.asA[Int](myVariableWithDifferentType)
  //
  // If instead asA was defined as asA[T, B], you would have to do:
  //
  //   Box.asA[DifferentType, Int](myVariableWithDifferentType)
  //
  // Uglier, and generally not as nice.
  /**
   * Create a `Full` box containing the specified value if `in` is of type
   * `B` and `Empty` otherwise.
   *
   * For example:
   * {{{
   * scala> Box.asA[Int]("boom")
   * res0: net.liftweb.common.Box[Int] = Empty
   *
   * scala> Box.asA[Int](5)
   * res1: net.liftweb.common.Box[Int] = Full(5)
   * }}}
   */
  def asA[B](in: Any)(implicit m: Manifest[B]): Box[B] = {
    (Box !! in).asA[B]
  }
}

/**
 * Used as a return type for certain methods that should not be called. One
 * example is the `get` method on a Lift `Box`. It exists to prevent client
 * code from using `.get` as an easy way to open a `Box`, so it needs a return
 * type that will match no valid client return types.
 */
final class DoNotCallThisMethod

/**
 * The `Box` class is a container which is able to declare if it is `Full`
 * (containing a single non-null value) or `[[EmptyBox]]`. An `EmptyBox`,
 * or empty, can be the `[[Empty]]` singleton, `[[Failure]]` or
 * `[[ParamFailure]]`. `Failure` and `ParamFailure` contain information about
 * why the `Box` is empty including exception information, possibly chained
 * `Failure`s and a `String` message.
 *
 * This serves a similar purpose to the `[[scala.Option Option]]` class from
 * Scala standard library but adds several features:
 *  - You can transform it to a `Failure` object if it is `Empty` (with the
 *    `[[?~]]` or `[[failMsg]]` method).
 *  - You can chain failure messages on `Failure`s (with the `?~!` or
 *    `[[compoundFailMsg]]` method).
 *  - You can "run" a function on a `Box`, with a default to return if the box
 *    is `Empty`:
 *    {{{
 *    val littleTeddyBears: Box[Int] = Full(10)
 *    littleTeddyBears.run("and then there were none") { (default: String, teddyBears: Int) =>
 *      s"\$teddyBears little teddy bears"
 *    } // => 10 little teddy bears
 *
 *    val updatedTeddyBears: Box[Int] = Empty
 *    littleTeddyBears.run("and then there were none") { (default: String, teddyBears: Int) =>
 *      s"\$teddyBears little teddy bears"
 *    } // => and then there were none
 *    }}}
 *  - You can "pass" a `Box` to a function for side effects:
 *    {{{
 *    val littleTeddyBears: Box[Int] = Full(10)
 *
 *    doSomething(
 *      littleTeddyBears $ { teddyBears: Box[Int] =>
 *        println("Are there any?")
 *        println(teddyBears openOr 0)
 *      }
 *    ) // doSomething gets a Box[Int] as well
 *    }}}
 *
 * === Exceptions and Empty Box Handling ===
 *
 * If you grew up on Java, you're used to `Exception`s as part of your program
 * logic.  The Scala philosophy and the Lift philosophy is that exceptions are
 * for exceptional conditions such as failure of an external resource (e.g.,
 * your database goes offline) rather than simply indicating that a parameter
 * wasn't supplied or couldn't be parsed.
 *
 * Lift's `Box` and Scala's `Option` provide mechanisms for being explicit
 * about a value existing or not existing rather than relying on a reference
 * being not-null.  However, extracting a value from a `Box` should be done
 * correctly. Available options are:
 *  - Using a `for` comprehension, especially for multiple boxes:
 *    {{{
 *    val loggedInUser: Box[User] =
 *      for {
 *        username <- possibleUsername
 *        password <- possiblePassword
 *        user <- User.find("username" -> username)
 *        if User.checkPassword(password, user.password)
 *      } yield {
 *        user
 *      }
 *    }}}
 *  - Using `map`, `flatMap`, `filter`, and `foreach` (`for` comprehensions
 *    use these under the covers):
 *    {{{
 *    val fullName: Box[String] =
 *      loggedInUser.map { user =>
 *        user.name + " (" + user.nickname + ")"
 *      }
 *    val bestFriend: Box[User] =
 *      loggedInUser.flatMap { user =>
 *        UserFriends.find(user.bestFriend.id)
 *      }
 *    val allowedUser: Box[User] =
 *      loggedInUser.filter(_.canAccess_?(currentPage))
 *
 *    fullName.foreach { name =>
 *      logger.info(s"User \$name is in the building.")
 *    }
 *    }}}
 *  - Using pattern-matching (a good way to deal with `Failure`s):
 *    {{{
 *    val loginMessage: String =
 *      loggedInUser match {
 *        case Full(user) =>
 *          "Login successful!"
 *        case Failure(message, _, _) =>
 *          s"Login failed: \$message"
 *        case Empty =>
 *          s"Unknown failure logging in."
 *      }
 *    }}}
 *  - For comparisons (e.g., in tests), use `==` and `===`:
 *    {{{
 *    loggedInUser must_== Full(mockUser)
 *    (loggedInUser === mockUser) must beTrue
 *    }}}
 */
sealed abstract class Box[+A] extends Product with Serializable{
  self =>
  /**
   * Returns `true` if this `Box` contains no value (i.e., it is `Empty` or
   * `Failure` or `ParamFailure`).
   */
  def isEmpty: Boolean

  /**
   * Returns true if the box contains a value.
   */
  def isDefined: Boolean = !isEmpty

  /**
   * The only time when you should be using this method is if the value is
   * guaranteed to be available based on a guard outside of the method. In these
   * cases, please provide that information in the justification `String`.
   * For example, `User.currentUser.openOrThrowException("This snippet is only
   * used on pages where the user is logged in")`. For tests, use `[[==]]` or
   * `[[===]]` instead. See the class documentation for more information.
   *
   * A valid justification for using this method should not be "I want my code
   * to fail fast when I call it."  Using exceptions in the core logic of your
   * application should be strongly discouraged.
   *
   * @param justification Justify why calling this method is okay and why it
   *        will not result in an exception being thrown. This serves both as
   *        mandatory documentation and as a very clear indication of what
   *        unexpected thing happened in the event you were wrong about the
   *        guard.
   *
   * @return The contents of the `Box` if it is `Full`.
   * @throws NullPointerException If you attempt to call it on an `EmptyBox`,
   *         with a message that includes the provided `justification`.
   */
  def openOrThrowException(justification: => String): A

  /**
   * Exists to avoid the implicit conversion from `Box` to `Option`. Opening a
   * `Box` unsafely should be done using `openOrThrowException`.
   *
   * This method '''always''' throws an exception.
   */
  final def get: DoNotCallThisMethod = {
    throw new Exception("Attempted to open a Box incorrectly. Please use openOrThrowException.")
  }

  /**
   * Return the value contained in this `Box` if it is full; otherwise return
   * the specified default. Equivalent to `Option`'s `[[scala.Option.getOrElse getOrElse]]`.
   */
  def openOr[B >: A](default: => B): B = default

  /**
   * Apply a function to the value contained in this `Box` if it exists and return
   * a `Full` containing the result. If this `Box` is not already `Full`, return
   * the unchanged box.
   *
   * @note This means that using `map` with a `Failure` will preserve the
   *       `Failure.`
   */
  def map[B](f: A => B): Box[B] = Empty

  /**
   * Apply a function returning a `Box` to the value contained in this `Box` if
   * it exists and return the resulting `Box`. If this `Box` is not already
   * `Full`, return this box unchanged.
   *
   * @note This means that using `flatMap` with a `Failure` will preserve the
   *       `Failure.`
   */
  def flatMap[B](f: A => Box[B]): Box[B] = Empty

  def flatten[B](implicit ev: A <:< Box[B]): Box[B] = this match {
    case Full(internal) => ev(internal)
    case f: Failure => f
    case Empty => Empty
  }


  /**
   * If this `Box` contains a value and it satisfies the specified `predicate`,
   * return the `Box` unchanged. Otherwise, return an `Empty`.
   */
  def filter(p: A => Boolean): Box[A] = this

  /**
   * Makes `Box` play better with Scala `for` comprehensions.
   */
  def withFilter(p: A => Boolean): WithFilter = new WithFilter(p)

  /**
   * Makes `Box` play better with Scala `for` comprehensions.
   */
  class WithFilter(p: A => Boolean) {
    def map[B](f: A => B): Box[B] = self.filter(p).map(f)
    def flatMap[B](f: A => Box[B]): Box[B] = self.filter(p).flatMap(f)
    def foreach[U](f: A => U): Unit = self.filter(p).foreach(f)
    def withFilter(q: A => Boolean): WithFilter =
      new WithFilter(x => p(x) && q(x))
  }

  /**
   * If this `Box` contains a value and it satisfies the specified `func`,
   * return `true`. Otherwise, return `false`.
   *
   * @return `true` if this Box does contain a value and it satisfies the
   *         predicate.
   */
  def exists(func: A => Boolean): Boolean = false

  /**
   * If this `Box` contains a value and it does not satisfy the specified
   * `func`, return `false`. Otherwise, return `true`.
   *
   * @return true If the `Box` is empty, or if its value satisfies the
   *         predicate.
   */
  def forall(func: A => Boolean): Boolean = true

  /**
   *
   * If this `Box` contains a value and it does '''not''' satisfy the specified
   * `f`, return the `Box` unchanged. Otherwise, return an `Empty`.
   */
  def filterNot(f: A => Boolean): Box[A] = filter(a => !f(a))

  /**
   * Perform a side effect by calling the specified function with the value
   * contained in this box. The function does not run if this `Box` is empty.
   */
  def foreach[U](f: A => U): Unit = {}

  /**
   * If this box is `Full` and contains an object of type `B`, returns a `Full`
   * of type `Box[B]`. Otherwise, returns `Empty`.
   *
   * This is basically a Java-friendly version of `[[asA]]`, which you should
   * prefer when using Scala.
   *
   * For example:
   * {{{
   * scala> Full("boom").isA(classOf[Int])
   * res0: net.liftweb.common.Box[Int] = Empty
   *
   * scala> Full(5).isA(classOf[Int])
   * res1: net.liftweb.common.Box[Int] = Full(5)
   * }}}
   */
  def isA[B](cls: Class[B]): Box[B] = Empty

  /**
   * Create a `Full` box containing the specified value if this box's value is
   * of type `B` and `Empty` otherwise.
   *
   * For example:
   * {{{
   * scala> Full("boom").asA[Int]
   * res0: net.liftweb.common.Box[Int] = Empty
   *
   * scala> Full(5).asA[Int]
   * res1: net.liftweb.common.Box[Int] = Full(5)
   * }}}
   */
  def asA[B](implicit m: Manifest[B]): Box[B] = Empty

  /**
   * Returns an `[[scala.collection.Iterator Iterator]]` over the value
   * contained in this `Box`, if any.
   */
  def elements: Iterator[A] = Iterator.empty

  /**
    * Return this Box if `Full`, or the specified alternative if it is
    * empty. Equivalent to `Option`'s `[[scala.Option.orElse orElse]]`.
    */
  def or[B >: A](alternative: => Box[B]): Box[B]

  /**
   * Get a `java.util.Iterator` from the Box.
   */
  def javaIterator[B >: A]: JavaIterator[B] = {
    val ar = new JavaArrayList[B]()
    foreach(v => ar.add(v))
    ar.iterator()
  }

  /**
   * Returns an `[[scala.collection.Iterator Iterator]]` over the value
   * contained in this `Box`, if any.
   *
   * Synonym for `[[elements]]`.
   */
  def iterator: Iterator[A] = this.elements

  /**
   * Returns a `[[scala.collection.immutable.List List]]` of one element if this
   * is Full, or an empty list if empty.
   */
  def toList: List[A] = Nil

  /**
   * Returns the contents of this box wrapped in `Some` if this is `Full`, or
   * `None` if this is empty (meaning an `Empty`, `Failure` or ParamFailure`).
   */
  def toOption: Option[A] = None

  /**
   * Transform an `Empty` to a `Failure` with the specified message. Otherwise
   * returns the same box.
   *
   * @note This means a `Failure` will also remain unchanged; see `?~!` to
   *       change these.
   *
   * @return A `Failure` with the message if this `Box` is `Empty`, this box
   *         otherwise.
   */
  def ?~(msg: => String): Box[A] = this

  /**
   * Transform an `Empty` or `Failure` to a `ParamFailure` with the specified
   * type-safe parameter.
   *
   * @param errorCode A value indicating the error.
   * @return A `ParamFailure` with the specified value, unless this is already a
   *         `ParamFailure` or a `Full`. If this is a `Failure`, the
   *         `ParamFailure` will preserve the message of the `Failure`.
   */
  def ~>[T](errorCode: => T): Box[A] = this

  /**
   * Alias for `[[?~]]`.
   */
  def failMsg(msg: => String): Box[A] = ?~(msg)

  /**
   * Chain the given `msg` as a `Failure` ahead of any failures this `Box` may
   * represent.
   *
   * If this is an `Empty`, this method behaves like `[[?~]]`. If it is a `Failure`,
   * however, this method returns a new `Failure` with the given `msg` and with its
   * `[[Failure.chain chain]]` set to this `Failure`.
   *
   * As with `[[?~]]`, if this is a `Full`, we return it unchanged.
   *
   * @return A `Failure` with the message if this `Box` is an `Empty` box. Chain
   *         this box to the new `Failure` if this is a `Failure`. The unchanged
   *         box if it is a `Full`.
   */
  def ?~!(msg: => String): Box[A] = ?~(msg)

  /**
   * Alias for `?~!`.
   */
  def compoundFailMsg(msg: => String): Box[A] = ?~!(msg)

  /**
   * If this `Box` contains a value and it satisfies the specified `predicate`,
   * return the `Box` unchanged. Otherwise, return a `Failure` with the given
   * `msg`.
   *
   * @see [[filter]]
   *
   * @return A `Failure` with the message if the box is empty or the predicate
   *         is not satisfied by the value contained in this Box.
   */
  def filterMsg(msg: String)(p: A => Boolean): Box[A] = filter(p) ?~ msg

  /**
   * This method calls the specified function with the specified `in` value and
   * the value contained in this `Box`. If this box is empty, returns the `in`
   * value directly.
   *
   * @return The result of the function or the `in` value.
   */
  def run[T](in: => T)(f: (T, A) => T) = in

  /**
   * Perform a side effect by passing this `Box` to the specified function and
   * return this `Box` unmodified. Similar to `foreach`, except that `foreach`
   * returns `Unit`, while this method allows chained use of the `Box`.
   *
   * @return This box.
   */
  def pass(f: Box[A] => Unit): Box[A] = {f(this) ; this}

  /**
   * Alias for `[[pass]]`.
   */
  def $(f: Box[A] => Unit): Box[A] = pass(f)

  /**
   * For `Full` and `Empty`, this has the expected behavior. Equality in terms
   * of Failure checks for equivalence of failure causes:
   * {{{
   * Failure("boom") == Failure("boom")
   * Failure("bam") != Failure("boom")
   * Failure("boom", Full(someException), Empty) != Failure("boom")
   * }}}
   *
   * For other values, determines equality based upon the contents of this `Box`
   * instead of the box itself. As a result, it is not symmetric. As an example:
   * {{{
   * val foo = "foo"
   * val boxedFoo = Full(foo)
   * foo == boxedFoo //is false
   * boxedFoo == foo //is true
   * }}}
   *
   * It is safest to use `===` explicitly when you're looking for this behavior,
   * and use `==` only for box-to-box comparisons:
   * {{{
   * Full("magic") == Full("magic")
   * Full("magic") != Full("another")
   * Full("magic") != Empty
   * Full("magic") != Failure("something's gone wrong")
   * }}}
   */
  override def equals(other: Any): Boolean = (this, other) match {
    case (Full(x), Full(y)) => x == y
    case (Full(x), y) => x == y
    case (x, y: AnyRef) => x eq y
    case _ => false
  }

  /**
   * Equivalent to `flatMap(f1).or(alternative)`.
   */
  def choice[B](f1: A => Box[B])(alternative: => Box[B]): Box[B] = this match {
    case Full(x) => f1(x)
    case _ => alternative
  }

  /**
   * Returns true if the value contained in this box is equal to the specified
   * value. This is the same thing that `==` does when it's handed a value that
   * isn't a `Box`, but using this is recommended because it's clearer that the
   * behavior will be different than the usual expectation.
   */
  def ===[B >: A](to: B): Boolean = false

  /**
   * Equivalent to `map(f).openOr(dflt)`.
   */
  def dmap[B](dflt: => B)(f: A => B): B = dflt


  /**
   * If the `Box` is `Full`, apply the transform function `f` on the value `v`;
   * otherwise, just return the value untransformed.
   *
   * The transform function is expected to be a function that will take the
   * value `v` and produce a function from the value in the box to a new value
   * of the same type as `v`.
   *
   * For example:
   * {{{
   * val myBox = Full(10)
   * myBox.fullXForm("No teddy bears left.")({ message =>
   *   { teddyBears: Int =>
   *     s"\$message Oh wait, there are \$teddyBears left!"
   *   }
   * })
   * }}}
   *
   * @tparam T The type of the initial value, default value, and transformed
   *         value.
   * @return If the `Box` is `Full`, the value once transformed by the function
   *         returned by `f`. Otherwise, the initial value `v`.
   */
  def fullXform[T](v: T)(f: T => A => T): T = v

  /**
   * An `[[scala.util.Either Either]]` that is a `Left` with the given argument
   * `left` if this is empty, or a `Right` with the boxed value if this is
   * `Full`.
   */
  def toRight[B](left: => B): Either[B, A] = Left(left)

  /**
   * An `[[scala.util.Either Either]]` that is a `Right` with the given argument
   * `right` if this is empty, or a `Left` with the boxed value if this is
   * `Full`.
   */
  def toLeft[B](right: => B): Either[A, B] = Right(right)

  /**
   * If the partial function is defined at the current Box's value, apply the
   * partial function.
   */
  final def collect[B](pf: PartialFunction[A, B]): Box[B] = flatMap { value =>
    Box(value)(pf)
  }

  /**
   * An alias for `collect`.
   *
   * Although this function is different for true collections, because `Box` is
   * really a collection of 1, the two functions are identical.
   */
  final def collectFirst[B](pf: PartialFunction[A, B]): Box[B] = {
    collect(pf)
  }

  /**
    * Transforms this box using the `transformFn`. If `transformFn` is defined for this box,
    * returns the result of applying `transformFn` to it. Otherwise, returns this box unchanged.
    *
    * If you want to change the content of a `Full` box, using `[[map]]` or `[[collect]]` might be better
    * suited to that purpose. If you want to convert an `Empty`, `Failure` or a `ParamFailure` into a
    * `Full` box, you should use `[[flip]]`.
    *
    * @example {{{
    *
    *  // Returns Full("alternative") because the partial function covers the case.
    *  Full("error") transform { case Full("error") => Full("alternative") }
    *
    *  // Returns Full(1), this Full box unchanged, because the partial function doesn't cover the case.
    *  Full(1) transform { case Full(2) => Failure("error") }
    *
    *  // Returns this Failure("another-error") unchanged because the partial function doesn't cover the case.
    *  Failure("another-error") transform { case Failure("error", Empty, Empty) => Full("alternative") }
    *
    *  // Returns Full("alternative") for an Empty box since `partialFn` is defined for Empty
    *  Empty transform { case Empty => Full("alternative") }
    *
    *  // Returns Empty because the partial function is not defined for Empty
    *  Empty transform { case Failure("error", Empty, Empty) => Full("alternative") }
    *
    *  }}}
    */
  def transform[B >: A](transformFn: PartialFunction[Box[A], Box[B]]): Box[B] = {
    transformFn.applyOrElse(this, (thisBox: Box[A]) => thisBox)
  }

  /**
    * Returns a `Full` box containing the results of applying `flipFn` to this box if it is a `Failure`,
    * `ParamFailure` or `Empty`. Returns `Empty` if this box is `Full`. In other words, it "flips" the
    * full/empty status of this Box.
    */
  def flip[B](flipFn: EmptyBox => B): Box[B] = this match {
    case e: EmptyBox => Full(flipFn(e))
    case _ => Empty
  }
}

/**
 * `Full` is a `[[Box]]` that contains a value.
 */
final case class Full[+A](value: A) extends Box[A] {
  def isEmpty: Boolean = false

  def openOrThrowException(justification: => String): A = value

  override def openOr[B >: A](default: => B): B = value

  override def or[B >: A](alternative: => Box[B]): Box[B] = this

  override def exists(func: A => Boolean): Boolean = func(value)

  override def forall(func: A => Boolean): Boolean = func(value)

  override def filter(p: A => Boolean): Box[A] = if (p(value)) this else Empty

  override def foreach[U](f: A => U): Unit = f(value)

  override def map[B](f: A => B): Box[B] = Full(f(value))

  override def flatMap[B](f: A => Box[B]): Box[B] = f(value)

  override def elements: Iterator[A] = Iterator(value)

  override def toList: List[A] = List(value)

  override def toOption: Option[A] = Some(value)

  override def run[T](in: => T)(f: (T, A) => T) = f(in, value)

  override def fullXform[T](v: T)(f: T => A => T): T = f(v)(value)


  override def toRight[B](left: => B): Either[B, A] = Right(value)

  override def toLeft[B](right: => B): Either[A, B] = Left(value)


  override def isA[B](clsOrg: Class[B]): Box[B] = value match {
    case value: AnyRef =>
      val cls = Box.primitiveMap.get(clsOrg) match {
        case Some(c) => c
        case _ => clsOrg
      }

      if (cls.isAssignableFrom(value.getClass)) Full(value.asInstanceOf[B])
      else Empty
    case _ => Empty
  }

  override def asA[B](implicit m: Manifest[B]): Box[B] = this.isA(m.runtimeClass).asInstanceOf[Box[B]]

  override def ===[B >: A](to: B): Boolean = value == to

  override def dmap[B](dflt: => B)(f: A => B): B = f(value)
}

/**
 * Singleton object representing a completely empty `Box` with no value or
 * failure information.
 */
case object Empty extends EmptyBox

/**
 * An `EmptyBox` is a `Box` containing no value. It can sometimes carry
 * additional failure information, as in `[[Failure]]` and `[[ParamFailure]]`.
 */
sealed abstract class EmptyBox extends Box[Nothing] with Serializable {

  def isEmpty: Boolean = true

  def openOrThrowException(justification: => String) =
  throw new NullPointerException("An Empty Box was opened.  The justification for allowing the openOrThrowException was "+justification)

  override def openOr[B >: Nothing](default: => B): B = default

  override def or[B >: Nothing](alternative: => Box[B]): Box[B] = alternative

  override def filter(p: Nothing => Boolean): Box[Nothing] = this

  override def ?~(msg: => String): Failure = Failure(msg, Empty, Empty)

  override def ?~!(msg: => String): Failure = Failure(msg, Empty, Empty)

  override def ~>[T](errorCode: => T): ParamFailure[T] = ParamFailure("", Empty, Empty, errorCode)
}

/**
 * Companion object used to simplify the creation of a simple `Failure` with
 * just a message.
 */
object Failure {
  def apply(msg: String) = new Failure(msg, Empty, Empty)
}

/**
 * A `Failure` is an `[[EmptyBox]]` with an additional failure message
 * explaining the reason for its being empty.  It can also optionally provide an
 * exception and/or a chain of previous `Failure`s that may have caused this
 * one.
 */
sealed case class Failure(msg: String, exception: Box[Throwable], chain: Box[Failure]) extends EmptyBox {
  type A = Nothing

  override def openOrThrowException(justification: => String) =
    throw new NullPointerException("An Failure Box was opened.  Failure Message: "+msg+
      ".  The justification for allowing the openOrThrowException was "+justification)  {
      override def getCause() = exception openOr null
    }

  override def map[B](f: A => B): Box[B] = this

  override def flatMap[B](f: A => Box[B]): Box[B] = this

  override def isA[B](cls: Class[B]): Box[B] = this

  override def asA[B](implicit m: Manifest[B]): Box[B] = this

  private def chainList: List[Failure] = chain match {
    case Full(f) => f :: f.chainList
    case _ => Nil
  }

  /**
   * Return a list of the exceptions that led to this `Failure`. First, unflattens
   * the list of causes of this `Failure`'s `exception`. Then, if this `Failure`
   * has a `chain`, walks down it and concatenates their `exceptionChain` to the
   * end of this one's.
   *
   * @return A single list of `Throwable`s from the most direct cause to the
   *         least direct cause of this `Failure`.
   */
  def exceptionChain: List[Throwable] = {
    import scala.collection.mutable.ListBuffer
    val ret = new ListBuffer[Throwable]()
    var e: Throwable = exception openOr null

    while (e ne null) {
      ret += e
      e = e.getCause
    }

    ret ++= chain.toList.flatMap(_.exceptionChain)
    ret.toList
  }

  /**
   * Gets the deepest exception cause, if any, which is ostensibly the root
   * cause of this `Failure`.
   */
  def rootExceptionCause: Box[Throwable] = {
    exceptionChain.lastOption
  }

  /**
   * Flatten the `Failure` chain to a List where this Failure is at the head.
   */
  def failureChain: List[Failure] =
    this :: chain.toList.flatMap(_.failureChain)

  /**
   * Reduce this `Failure`'s message and the messages of all chained failures a
   * to a single `String`. The resulting string links each step in the failure
   * chain with <-, and this `Failure`'s message is last.
   *
   * For example:
   * {{{
   * scala> Failure("It's all gone wrong.") ?~! "Something's gone wrong." ?~! "It's all sideways"
   * res0: net.liftweb.common.Failure = Failure(It's all sideways,Empty,
   *         Full(Failure(Something's gone wrong.,Empty,
   *           Full(Failure(It's all gone wrong.,Empty,Empty)))))
   * scala> res0.messageChain
   * res1: String = It's all sideways <- Something's gone wrong. <- It's all gone wrong.
   * }}}
   */
  def messageChain: String = (this :: chainList).map(_.msg).mkString(" <- ")

  override def equals(other: Any): Boolean = (this, other) match {
    case (Failure(x, y, z), Failure(x1, y1, z1)) => (x, y, z) == (x1, y1, z1)
    case (x, y: AnyRef) => x eq y
    case _ => false
  }

  override def ?~(msg: => String): Failure = this

  override def ?~!(msg: => String): Failure = Failure(msg, Empty, Full(this))

  override def ~>[T](errorCode: => T): ParamFailure[T] = ParamFailure(msg, exception, chain, errorCode)
}

/**
 * Companion object used to simplify the creation of simple `ParamFailure`s, as
 * well as allow pattern-matching on the `ParamFailure`.
 */
object ParamFailure {
  def apply[T](msg: String, exception: Box[Throwable], chain: Box[Failure], param: T) =
    new ParamFailure(msg, exception, chain, param)

  def apply[T](msg: String, param: T) = new ParamFailure(msg, Empty, Empty, param)

  def unapply(in: Box[_]): Option[(String, Box[Throwable], Box[Failure], Any)] = in match {
    case pf: ParamFailure[_] => Some((pf.msg, pf.exception, pf.chain, pf.param))
    case _ => None
  }
}

/**
 * A `ParamFailure` is a `[[Failure]]` with an additional type-safe parameter
 * that can allow an application to store other information related to the
 * failure.
 *
 * For example:
 * {{{
 * val loggedInUser =
 *   for {
 *     username ?~ "Missing username" ~> "error.missingUser"
 *     password ?~! "Missing password" ~> "error.missingPassword"
 *     user <- User.find("username" -> username)
 *     if User.checkPassword(password, user.password)
 *   } yield {
 *     user
 *   }
 *
 * loggedInUser match {
 *   case ParamFailure(message, _, _, i18nKey: String) =>
 *     tellUser(i18n(i18nKey))
 *   case Failure(message, _, _) =>
 *     tellUser(failureMessage)
 *   case Empty =>
 *     tellUser("Unknown login failure.")
 *   case _ =>
 *     tellUser("You're in!")
 * }
 * }}}
 */
final class ParamFailure[T](override val msg: String,
		            override val exception: Box[Throwable],
		            override val chain: Box[Failure], val param: T) extends
  Failure(msg, exception, chain) with Serializable{
    override def toString(): String = "ParamFailure("+msg+", "+exception+
    ", "+chain+", "+param+")"

    override def equals(that: Any): Boolean = that match {
      case ParamFailure(m, e, c, p) =>
        m == msg && e == exception && c == chain && p == param
      case _ => false
    }

    override def hashCode(): Int = super.hashCode() + (param match {
      case null => 0
      case x => x.hashCode()
    })

    override def ~>[T](errorCode: => T): ParamFailure[T] =
      ParamFailure(msg, exception, Full(this), errorCode)
  }

/**
 * A trait that a class can mix into itself to indicate that it can convert
 * itself into a `Box`.
 */
trait Boxable[T] {
  def asBox: Box[T]
}

/**
 * Sometimes it's convenient to access either a `[[Box]][T]` or a `T`.  If you
 * specify `BoxOrRaw[T]`, either a `T` or a `Box[T]` can be passed and the
 * "right thing" will happen, including `null`s being treated as `Empty`.
 */
sealed trait BoxOrRaw[T] {
  def box: Box[T]
}

/**
 * Companion object with implicit conversions to allow `BoxOrRaw[T]` to
 * masquerade as the appropriate types.
 */
object BoxOrRaw {
  implicit def rawToBoxOrRaw[T, Q <: T](r: Q): BoxOrRaw[T] =
    RawBoxOrRaw(r: T)

  implicit def boxToBoxOrRaw[T, Q](r: Box[Q])(implicit ev: Q => T): BoxOrRaw[T] = {
    BoxedBoxOrRaw(r.map(v => ev(v)))
  }

  implicit def optionToBoxOrRaw[T, Q](r: Option[Q])(implicit ev: Q => T): BoxOrRaw[T] = {
    BoxedBoxOrRaw(r.map(v => ev(v)))
  }

  implicit def borToBox[T](in: BoxOrRaw[T]): Box[T] = in.box
}

/**
 * The `[[BoxOrRaw]]` that represents a boxed value.
 */
final case class BoxedBoxOrRaw[T](box: Box[T]) extends BoxOrRaw[T]

/**
 * The `[[BoxOrRaw]]` that represents a raw value.
 */
final case class RawBoxOrRaw[T](raw: T) extends BoxOrRaw[T] {
  def box: Box[T] =
    if (raw.asInstanceOf[Object] ne null) Full(raw) else Empty
}

// vim: set ts=2 sw=2 et:
