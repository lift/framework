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
 * The bridge from Java to Scala Box
 */
class BoxJBridge {
  /**
   * Get the Box companion object
   */
  def box: BoxTrait = Box

  /**
   * Get the None singleton
   */
  def empty: EmptyBox = Empty
}

/**
 * The Box companion object provides methods to create a Box from:
 * <ul>
 *   <li>an Option</li>
 *   <li>a List</li>
 *   <li>any AnyRef object</li>
 * </ul>
 *
 * It also provides implicit methods to transform Option to Box, Box to Iterable, and Box to Option
 */
object Box extends BoxTrait {
  /**
   * Helper class to provide an easy way for converting Lists of Boxes[T] into
   * a Box of List[T].
  **/
  implicit class ListOfBoxes[T](val theListOfBoxes: List[Box[T]]) extends AnyVal {
    /**
     * Convert a List of Boxes into a single Box containting a List[T], where T is
     * the parameterized type of the Boxes.
     *
     * This method is useful for those cases where you have a lot of operations being
     * executed that all return some Box[T]. You want just a List[T] if all of those
     * operations succeeded, but you don't want to have Failures disappear if any were
     * present in the list.
     *
     * If all of the Boxes in the List are Full or Empty, we return a Full box containing
     * a List of all of the Full Box values that were present. If any of the Boxes contain
     * a Failure, a ParamFailure is returned, containing the original List[Box[T]] as the
     * param.
     *
     * It is worth noting that the size of the list in the resulting Box[List[T]] may not be equal
     * to the size of the List[Box[T]] that is fed as Empty values will disappear altogether in the
     * conversion.
     *
     * @param failureErrorMessage The string that should be placed in the message for the Failure.
     * @return A Full[List[T]] if no Failures were present. ParamFailure[List[Box[T]]] otherwise.
    **/
    def toSingleBox(failureErrorMessage: String): Box[List[T]] = {
      if (theListOfBoxes.exists(_.isInstanceOf[Failure])) {
        Failure(failureErrorMessage) ~> theListOfBoxes
      } else {
        Full(theListOfBoxes.flatten)
      }
    }
  }

}

/**
 * The Box companion object provides methods to create a Box from:
 * <ul>
 *   <li>an Option</li>
 *   <li>a List</li>
 *   <li>any AnyRef object</li>
 * </ul>
 *
 * It also provides implicit methods to transform Option to Box, Box to Iterable, and Box to Option
 */
sealed trait BoxTrait {
  val primativeMap: Map[Class[_], Class[_]] = Map(
    java.lang.Boolean.TYPE -> classOf[java.lang.Boolean],
    java.lang.Character.TYPE -> classOf[java.lang.Character],
    java.lang.Byte.TYPE -> classOf[java.lang.Byte],
    java.lang.Double.TYPE -> classOf[java.lang.Double],
    java.lang.Float.TYPE -> classOf[java.lang.Float],
    java.lang.Integer.TYPE -> classOf[java.lang.Integer],
    java.lang.Long.TYPE -> classOf[java.lang.Long],
    java.lang.Short.TYPE -> classOf[java.lang.Short])

  /**
   * Create a Box from the specified Option.
   * @return a Box created from an Option. Full(x) if the Option is Some(x) and Empty otherwise
   */
  def apply[T](in: Option[T]) = in match {
    case Some(x) => Full(x)
    case _ => Empty
  }

  /**
   * Create a Box from the specified Box, checking for null.
   * @return Full(x) if in is Full(x) and x is not null
   * Empty otherwise
   */
  def apply[T](in: Box[T]) = in match {
    case Full(x) => legacyNullTest(x)
    case x: EmptyBox => x
    case _ => Empty
  }

  /**
   * Transform a List with zero or one elements to a Box.
   * @return a Box object containing the head of a List. Full(x) if the List contains at least one element and Empty otherwise.
   */
  def apply[T](in: List[T]) = in match {
    case x :: _ => Full(x)
    case _ => Empty
  }

  /**
   * Apply the specified PartialFunction to the specified value and return the result
   * in a Full Box; if the pf is undefined at that point return Empty.
   * @param pf the partial function to use to transform the value
   * @param value the value to transform
   * @return a Full box containing the transformed value if pf.isDefinedAt(value); Empty otherwise
   */
  def apply[InType, OutType](pf: PartialFunction[InType, OutType])(value: InType): Box[OutType] =
  if (pf.isDefinedAt(value)) Full(pf(value)) else Empty

  /**
   * Apply the specified PartialFunction to the specified value and return the result
   * in a Full Box; if the pf is undefined at that point return Empty.
   * @param pf the partial function to use to transform the value
   * @param value the value to transform
   * @return a Full box containing the transformed value if pf.isDefinedAt(value); Empty otherwise
   */
  def apply[InType, OutType](value: InType)(pf: PartialFunction[InType, OutType]): Box[OutType] =
  if (pf.isDefinedAt(value)) Full(pf(value)) else Empty

  /**
   * This implicit transformation allows one to use a Box as an Iterable
   * @return List(in) if this Box is Full(in); Nil otherwise
   */
  implicit def box2Iterable[T](in: Box[T]): Iterable[T] = in.toList

  /**
   * This implicit transformation allows one to use an Option as a Box.
   * @return a Box object from an Option. Full(in) if the Option is Some(in); Empty otherwise
   */
  implicit def option2Box[T](in: Option[T]): Box[T] = Box(in)

  /**
   * This implicit transformation allows one to use a Box as an Option.
   * @return <code>Some(in)</code> if this Box is <code>Full(in)</code>; None otherwise
   */
  implicit def box2Option[T](in: Box[T]): Option[T] = in.toOption

  /**
   * This method allows one to encapsulate any object in a Box in a null-safe manner,
   * treating null values to Empty
   * @return <code>Full(in)</code> if <code>in</code> is not null; Empty otherwise
   */
  def legacyNullTest[T](in: T): Box[T] = in match {
    case null => Empty
    case _ => Full(in)
  }

  /**
   * Alias for legacyNullTest.
   * This method allows one to encapsulate any object in a Box in a null-safe manner,
   * returning Empty if the specified value is null.
   * @return Full(in) if <code>in</code> is not null Empty otherwise
   */
  def !![T](in: T): Box[T] = legacyNullTest(in)

  /**
   * Create a Full box containing the specified value if "in" is an instance
   * of the specified class, or Empty otherwise.
   */
  def isA[A, B](in: A, clz: Class[B]): Box[B] =
  (Box !! in).isA(clz)

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
   * Create a Full box containing the specified value if <code>in</code> is of
   * type <code>B</code>; Empty otherwise.
   */
  def asA[B](in: T forSome { type T })(implicit m: Manifest[B]): Box[B] =
  (Box !! in).asA[B]
}

/**
 * The Box class is a container which is able to declare if it is Full (containing a single non-null value) or EmptyBox. An EmptyBox, or empty, can be the Empty singleton, Failure or ParamFailure.
 * Failure and ParamFailure contain information about why the Box is empty including
 * exception information, chained Failures and a String.
 * It serves a similar purpose to the Option class from Scala standard library but adds several features:
 * <ul>
 *   <li> you can transform it to a Failure object if it is Empty (with the ?~ method)</li>
 *   <li> you can chain failure messages on Failure Boxes</li>
 *   <li> you "run" a function on your Box, with a default value: <code>Full(1).run("zero") { (x: String, y: Int) => y.toString }</code></li>
 *   <li> you can "pass" a Box to a function for side effects: <code>Full(1) $ { x: Box[Int] => println(x openOr 0) }</code></li>
 * </ul>
 */
sealed abstract class Box[+A] extends Product with Serializable{
  self =>
  /**
   * Returns true if this Box contains no value (is Empty or Failure or ParamFailure)
   * @return true if this Box contains no value
   */
  def isEmpty: Boolean

  /**
   * Returns true if the box contains a value.
   * @return true if this Box contains a value
   */
  def isDefined: Boolean = !isEmpty

  /**
   * If you grew up on Java, you're used to Exceptions as part of your program logic.
   * The Scala philosophy and the Lift philosophy is that exceptions are for exceptional
   * conditions such as failure of an external resource (e.g., your database goes offline)
   * rather than simply indicating that a parameter wasn't supplied or couldn't be parsed.
   *
   * Lift's Box and Scala's Option provide a mechanism for being explicit about a value
   * existing or not existing rather than relying on a reference being not-null.  However,
   * extracting a value from a Box should be done correctly.  Correctly can be (in order of use
   * in David Pollak's code): a for comprehension; using map, flatMap or foreach; or using pattern matching.
   *
   * The only times when you should be using this method are: the value is guaranteed to be available based
   * on a guard outside of the method using the Box or in tests.  For example,
   * User.currentUser.openOrThrowException("This snippet is used on pages where the user is logged in")
   *
   * A valid justification for using this method should not be "I want my code to fail fast when I call it."
   * Using exceptions in the core logic of your application should be strongly discouraged.
   *
   * This method replaces open_! because people used open_! and generally ignored the reason for the "!",
   * so we're making it more explicit that this method should not commonly be used and should be justified
   * when used.
   *
   * @param justification Justify why calling this method is okay and why it will not result in an Exception
   *
   * @return The contents of the Box if it has one or an exception if not
   */
  def openOrThrowException(justification: String): A

  /**
   * Exists to avoid the implicit conversion from Box to Option. Opening a Box
   * unsafely should be done using openOrThrowException.
   */
  final def get: Nothing = {
    throw new Exception("Attempted to open a Box incorrectly. Please use openOrThrowException.")
  }

  /**
   * Return the value contained in this Box if it is full; otherwise return the specified default
   * @return the value contained in this Box if it is full; otherwise return the specified default
   */
  def openOr[B >: A](default: => B): B = default

  /**
   * Apply a function to the value contained in this Box if it exists and return
   * a new Box containing the result, or empty otherwise.
   * @return the modified Box or empty
   */
  def map[B](f: A => B): Box[B] = Empty

  /**
   * Apply a function returning a Box to the value contained in this Box if it exists
   * and return the result, or empty otherwise.
   * @return the modified Box or empty
   */
  def flatMap[B](f: A => Box[B]): Box[B] = Empty

  /**
   * Return this Box if it contains a value satisfying the specified predicate; Empty otherwise
   * @return this Box if it contains a value satisfying the specified predicate; Empty otherwise
   */
  def filter(p: A => Boolean): Box[A] = this

  /**
   * Makes Box play better with Scala 2.8 for comprehensions
   */
  def withFilter(p: A => Boolean): WithFilter = new WithFilter(p)

  /**
   * Play NiceLike with the Scala 2.8 for comprehension
   */
  class WithFilter(p: A => Boolean) {
    def map[B](f: A => B): Box[B] = self.filter(p).map(f)
    def flatMap[B](f: A => Box[B]): Box[B] = self.filter(p).flatMap(f)
    def foreach[U](f: A => U): Unit = self.filter(p).foreach(f)
    def withFilter(q: A => Boolean): WithFilter =
      new WithFilter(x => p(x) && q(x))
  }

  /**
   * Determine whether this Box contains a value which satisfies the specified predicate
   * @return true if this Box does contain a value and it satisfies the predicate
   */
  def exists(func: A => Boolean): Boolean = false

  /**
   * Determine whether all Box values satisfy the predicate
   * @return true if the Box is empty, or if Box's value satisfies the predicate
   */
  def forall(func: A => Boolean): Boolean = true

  /**
   * Creates a Box if the current Box is Full and the value does not satisfy the predicate, f.
   *
   * @param f the predicate used to test value.
   *
   * @return a Box
   */
  def filterNot(f: A => Boolean): Box[A] = filter(a => !f(a))

  /**
   * Perform a side effect by calling the specified function
   * with the value contained in this box.
   */
  def foreach[U](f: A => U): Unit = {}

  /**
   * Return a Full[B] if the contents of this Box is an instance of the specified class,
  * otherwise return Empty
   */
  def isA[B](cls: Class[B]): Box[B] = Empty

  /**
   * Return a Full[B] if the contents of this Box is of type <code>B</code>, otherwise return Empty
   */
  def asA[B](implicit m: Manifest[B]): Box[B] = Empty

  /**
   * Return this Box if Full, or the specified alternative if this is empty
   */
  def or[B >: A](alternative: => Box[B]): Box[B] = alternative

  /**
   * Returns an Iterator over the value contained in this Box
   */
  def elements: Iterator[A] = Iterator.empty

  /**
   * Get a Java Iterator from the Box
   */
  def javaIterator[B >: A]: JavaIterator[B] = {
    val ar = new JavaArrayList[B]()
    foreach(v => ar.add(v))
    ar.iterator()
  }

  /**
   * Returns an Iterator over the value contained in this Box
   */
  def iterator: Iterator[A] = this.elements

  /**
   * Returns a List of one element if this is Full, or an empty list if empty.
   */
  def toList: List[A] = Nil

  /**
   * Returns the contents of this box in an Option if this is Full, or
   * None if this is a empty (Empty, Failure or ParamFailure)
   */
  def toOption: Option[A] = None

  /**
   * Transform an Empty to a Failure with the specified message.
   * @param msg the failure message
   * @return a Failure with the message if this Box is Empty
   */
  def ?~(msg: => String): Box[A] = this



  /**
   * Transform an Empty to a ParamFailure with the specified typesafe
   * parameter.
   * @param errorCode a value indicating the error
   * @return a ParamFailure with the specified value
   */
  def ~>[T](errorCode: => T): Box[A] = this

  /**
   * Alias for ?~
   */
  def failMsg(msg: => String): Box[A] = ?~(msg)

  /**
   * Transform an EmptyBox to a Failure with the specified message and chain
   * the new Failure to any previous Failure represented by this Box.
   * @param msg the failure message
   * @return a Failure with the message if this Box is an Empty Box. Chain the messages if it is already a Failure
   */
  def ?~!(msg: => String): Box[A] = ?~(msg)

  /**
   * Alias for ?~!
   */
  def compoundFailMsg(msg: => String): Box[A] = ?~!(msg)

  /**
   * Filter this box on the specified predicate, returning a Failure with the specified
   * message if the predicate is not satisfied.
   * @param msg the failure message
   * @param p a predicate
   * @return a Failure with the message if the predicate is not satisfied by the value contained in this Box
   */
  def filterMsg(msg: String)(p: A => Boolean): Box[A] = filter(p) ?~ msg

  /**
   * This method calls the specified function with the value contained in this Box
   * @return the result of the function or a default value
   */
  def run[T](in: => T)(f: (T, A) => T) = in

  /**
   * Perform a side effect by passing this Box to the specified function
   * and return this Box unmodified.
   * @return this Box
   */
  def pass(f: Box[A] => Unit): Box[A] = {f(this) ; this}

  /**
   * Alias for pass
   */
  def $(f: Box[A] => Unit): Box[A] = pass(f)

  /**
   * Determines equality based upon the contents of this Box instead of the box itself.
   * As a result, it is not symmetric. Which means that for
   *
   * <pre name="code" class="scala">
   *     val foo = "foo"
   *     val boxedFoo = Full(foo)
   *     foo == boxedFoo //is false
   *     boxedFoo == foo //is true
   * </pre>
   *
   * For Full and Empty, this has the expected behavior. Equality in terms of Failure
   * checks for equivalence of failure causes.
   */
  override def equals(other: Any): Boolean = (this, other) match {
    case (Full(x), Full(y)) => x == y
    case (Full(x), y) => x == y
    case (x, y: AnyRef) => x eq y
    case _ => false
  }

  /**
   * Apply the function f1 to the contents of this Box if available; if this
   * is empty return the specified alternative.
   */
  def choice[B](f1: A => Box[B])(alternative: => Box[B]): Box[B] = this match {
    case Full(x) => f1(x)
    case _ => alternative
  }

  /**
   * Returns true if the value contained in this box is equal to the specified value.
   */
  def ===[B >: A](to: B): Boolean = false

  /**
   * Equivalent to map(f).openOr(Full(dflt))
   */
  def dmap[B](dflt: => B)(f: A => B): B = dflt


  /**
   * If the Box is Full, apply the transform function on the
   * value, otherwise just return the value untransformed
   *
   * @param v the value
   * @param f the transformation function
   * @tparam T the type of the value
   * @return the value or the transformed value is the Box is Full
   */
  def fullXform[T](v: T)(f: T => A => T): T = v

  /**
   * An <code>Either</code> that is a <code>Left</code> with the given argument
   * <code>left</code> if this is empty, or a <code>Right</code> if this
   * Full with the Box's value.
   */
  def toRight[B](left: => B): Either[B, A] = Left(left)

  /**
   * An <code>Either</code> that is a <code>Right</code> with the given
   * argument
   * <code>right</code> if this is empty, or a <code>Left</code> if this is
   * Fill with the Box's value
   */
  def toLeft[B](right: => B): Either[A, B] = Right(right)


  /**
   * If the partial function is defined at the current Box's value
   * apply the partial function.
   */
  final def collect[B](pf: PartialFunction[A, B]): Box[B] = {
    flatMap(value =>
    if (pf.isDefinedAt(value)) Full(pf(value))
    else Empty)
  }

}

/**
 * Full is a Box containing a value.
 */
final case class Full[+A](value: A) extends Box[A]{

  def isEmpty: Boolean = false



  /**
   * If you grew up on Java, you're used to Exceptions as part of your program logic.
   * The Scala philosophy and the Lift philosophy is that exceptions are for exceptional
   * conditions such as failure of an external resource (e.g., your database goes offline)
   * rather than simply indicating that a parameter wasn't supplied or couldn't be parsed.
   *
   * Lift's Box and Scala's Option provide a mechanism for being explicit about a value
   * existing or not existing rather than relying on a reference being not-null.  However,
   * extracting a value from a Box should be done correctly.  Correctly can be (in order of use
   * in David Pollak's code): a for comprehension; using map, flatMap or foreach; or using pattern matching.
   *
   * The only times when you should be using this method are: the value is guaranteed to be available based
   * on a guard outside of the method using the Box or in tests.  For example,
   * User.currentUser.openOrThrowException("This snippet is used on pages where the user is logged in")
   *
   * A valid justification for using this method should not be "I want my code to fail fast when I call it."
   * Using exceptions in the core logic of your application should be strongly discouraged.
   *
   * This method replaces open_! because people used open_! and generally ignored the reason for the "!",
   * so we're making it more explicit that this method should not commonly be used and should be justified
   * when used.
   *
   * @param justification Justify why calling this method is okay and why it will not result in an Exception
   *
   * @return The contents of the Box if it has one or an exception if not
   */
  def openOrThrowException(justification: String): A = value


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

  /**
   * If the Box is Full, apply the transform function on the
   * value, otherwise just return the value untransformed
   *
   * @param v the value
   * @param f the transformation function
   * @tparam T the type of the value
   * @return the value or the transformed value is the Box is Full
   */
  override def fullXform[T](v: T)(f: T => A => T): T = f(v)(value)


  /**
   * An <code>Either</code> that is a <code>Left</code> with the given argument
   * <code>left</code> if this is empty, or a <code>Right</code> if this
   * Full with the Box's value.
   */
  override def toRight[B](left: => B): Either[B, A] = Right(value)

  /**
   * An <code>Either</code> that is a <code>Right</code> with the given
   * argument
   * <code>right</code> if this is empty, or a <code>Left</code> if this is
   * Fill with the Box's value
   */
  override def toLeft[B](right: => B): Either[A, B] = Left(value)


  override def isA[B](clsOrg: Class[B]): Box[B] = value match {
    case value: AnyRef =>
      val cls = Box.primativeMap.get(clsOrg) match {
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
 * Singleton object representing an Empty Box
 */
case object Empty extends EmptyBox

/**
 * The EmptyBox is a Box containing no value.
 */
sealed abstract class EmptyBox extends Box[Nothing] with Serializable {

  def isEmpty: Boolean = true


  /**
   * If you grew up on Java, you're used to Exceptions as part of your program logic.
   * The Scala philosophy and the Lift philosophy is that exceptions are for exceptional
   * conditions such as failure of an external resource (e.g., your database goes offline)
   * rather than simply indicating that a parameter wasn't supplied or couldn't be parsed.
   *
   * Lift's Box and Scala's Option provide a mechanism for being explicit about a value
   * existing or not existing rather than relying on a reference being not-null.  However,
   * extracting a value from a Box should be done correctly.  Correctly can be (in order of use
   * in David Pollak's code): a for comprehension; using map, flatMap or foreach; or using pattern matching.
   *
   * The only times when you should be using this method are: the value is guaranteed to be available based
   * on a guard outside of the method using the Box or in tests.  For example,
   * User.currentUser.openOrThrowException("This snippet is used on pages where the user is logged in")
   *
   * A valid justification for using this method should not be "I want my code to fail fast when I call it."
   * Using exceptions in the core logic of your application should be strongly discouraged.
   *
   * This method replaces open_! because people used open_! and generally ignored the reason for the "!",
   * so we're making it more explicit that this method should not commonly be used and should be justified
   * when used.
   *
   * @param justification Justify why calling this method is okay and why it will not result in an Exception
   *
   * @return The contents of the Box if it has one or an exception if not
   */
  def openOrThrowException(justification: String) =
  throw new NullPointerException("An Empty Box was opened.  The justification for allowing the openOrThrowException was "+justification)



  override def openOr[B >: Nothing](default: => B): B = default

  override def or[B >: Nothing](alternative: => Box[B]): Box[B] = alternative

  override def filter(p: Nothing => Boolean): Box[Nothing] = this

  override def ?~(msg: => String): Failure = Failure(msg, Empty, Empty)

  override def ?~!(msg: => String): Failure = Failure(msg, Empty, Empty)

  override def ~>[T](errorCode: => T): ParamFailure[T] =
    ParamFailure("", Empty, Empty, errorCode)
}

/**
 * Companion object used to simplify the creation of a simple Failure.
 */
object Failure {
  def apply(msg: String) = new Failure(msg, Empty, Empty)
}

/**
 * A Failure is an EmptyBox with an additional failure message explaining the reason for its being empty.
 * It can also optionally provide an exception or a chain of causes represented as a list of other Failure objects
 */
sealed case class Failure(msg: String, exception: Box[Throwable], chain: Box[Failure]) extends EmptyBox{
  type A = Nothing


  /**
   * If you grew up on Java, you're used to Exceptions as part of your program logic.
   * The Scala philosophy and the Lift philosophy is that exceptions are for exceptional
   * conditions such as failure of an external resource (e.g., your database goes offline)
   * rather than simply indicating that a parameter wasn't supplied or couldn't be parsed.
   *
   * Lift's Box and Scala's Option provide a mechanism for being explicit about a value
   * existing or not existing rather than relying on a reference being not-null.  However,
   * extracting a value from a Box should be done correctly.  Correctly can be (in order of use
   * in David Pollak's code): a for comprehension; using map, flatMap or foreach; or using pattern matching.
   *
   * The only times when you should be using this method are: the value is guaranteed to be available based
   * on a guard outside of the method using the Box or in tests.  For example,
   * User.currentUser.openOrThrowException("This snippet is used on pages where the user is logged in")
   *
   * A valid justification for using this method should not be "I want my code to fail fast when I call it."
   * Using exceptions in the core logic of your application should be strongly discouraged.
   *
   * This method replaces open_! because people used open_! and generally ignored the reason for the "!",
   * so we're making it more explicit that this method should not commonly be used and should be justified
   * when used.
   *
   * @param justification Justify why calling this method is okay and why it will not result in an Exception
   *
   * @return The contents of the Box if it has one or an exception if not
   */
  override def openOrThrowException(justification: String) =
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
   * Get the exception chain along with the exception chain of any
   * chained failures
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
   * Gets the deepest exception cause
   */
  def rootExceptionCause: Box[Throwable] = {
    exceptionChain.lastOption
  }

  /**
   * Flatten the Failure chain to a List where this
   * Failure is at the head
   */
  def failureChain: List[Failure] = 
    this :: chain.toList.flatMap(_.failureChain)

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
 * A ParamFailure is a Failure with an additional typesafe parameter that can
 * allow an application to store other information related to the failure.
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
  }

/**
 * A trait that a class can mix into itself to convert itself into a Box
 */
trait Boxable[T] {
  def asBox: Box[T]
}

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
 * Sometimes it's convenient to access either a Box[T]
 * or a T.  If you specify BoxOrRaw[T], the
 * either a T or a Box[T] can be passed and the "right thing"
 * will happen
 */
sealed trait BoxOrRaw[T] {
  def box: Box[T]
}

/**
 * The companion object that has helpful conversions
 */
object BoxOrRaw {
  /**
   * Convert a T to a BoxOrRaw[T]
   */
  implicit def rawToBoxOrRaw[T, Q <: T](r: Q): BoxOrRaw[T] =
    RawBoxOrRaw(r: T)

  /**
   * Convert a Box[T] to a BoxOrRaw[T]
   */
  implicit def boxToBoxOrRaw[T, Q <% T](r: Box[Q]): BoxOrRaw[T] = {
    BoxedBoxOrRaw(r.map(v => v: T))
  }

  /**
   * Convert an Option[T] to a BoxOrRaw[T]
   */
  implicit def optionToBoxOrRaw[T, Q <% T](r: Option[Q]): BoxOrRaw[T] = {
    BoxedBoxOrRaw(r.map(v => v: T))
  }

  /**
   * Convert a BoxOrRaw[T] to a Box[T]
   */
  implicit def borToBox[T](in: BoxOrRaw[T]): Box[T] = in.box
}

/**
 * The Boxed up BoxOrRaw
 */
final case class BoxedBoxOrRaw[T](box: Box[T]) extends BoxOrRaw[T]

/**
 * The raw version of BoxOrRaw
 */
final case class RawBoxOrRaw[T](raw: T) extends BoxOrRaw[T] {
  def box: Box[T] = 
    if (raw.asInstanceOf[Object] ne null) Full(raw) else Empty
}

// vim: set ts=2 sw=2 et:
