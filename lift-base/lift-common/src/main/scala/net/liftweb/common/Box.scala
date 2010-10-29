/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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

package net.liftweb {
package common {

import _root_.scala.reflect.Manifest

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
object Box {
  /**
   * Create a Box from the specified Option.
   * @return a Box created from an Option. Full(x) if the Option is Some(x) and Empty otherwise
   */
  def apply[T](in: Option[T]) = in match {
    case Some(x) => Full(x)
    case _ => Empty
  }

  /**
   * Create a Box from the specified Option.
   * @return a Box created from a Box. Full(x) if the Box is Full(x) and
   * not null
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
   * This method allows one to encapsulate any object in a Box in a null-safe manner,
   * treating null values to Empty.  This is a parallel method to
   * the Scala Option's apply method.
   * 
   * @return <code>Full(in)</code> if <code>in</code> is not null; Empty otherwise
   */
  def apply[T](in: T): Box[T] = legacyNullTest(in)

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

  /**
   * Create a Full box containing the specified value if <code>in</code> is of
   * type <code>B</code>; Empty otherwise.
   */
  def asA[B](in: T forSome {type T})(implicit m: Manifest[B]): Box[B] =
  (Box !! in).asA[B]
}

/**
 * The Box class is a container which is able to declare if it is Full (containing a single non-null value) or Empty.
 * It serves the same purpose as the Option class from Scala standard library but adds several features:
 * <ul>
 *   <li> you can transform it to a Failure object if it is Empty (with the ?~ method)</li>
 *   <li> you can chain failure messages on Failure Boxes</li>
 *   <li> you "run" a function on your Box, with a default value: <code>Full(1).run("zero") { (x: String, y: Int) => y.toString }</code></li>
 *   <li> you can "pass" a Box to a function for side effects: <code>Full(1) $ { x: Box[Int] => println(x openOr 0) }</code></li>
 * </ul>
 */
@serializable
sealed abstract class Box[+A] extends Product {
  self =>
  /**
   * Returns true if this Box contains no value (is Empty or Failure)
   * @return true if this Box contains no value
   */
  def isEmpty: Boolean

  /**
   * Returns true if the box contains a value.
   * @return true if this Box contains a value
   */
  def isDefined: Boolean = !isEmpty

  /**
   * Return the value contained in this Box if it is full; throw an exception otherwise
   * @return the value contained in this Box if it is full; throw an exception otherwise
   */
  def open_! : A

  /**
   * Return the value contained in this Box if it is full; otherwise return the specified default
   * @return the value contained in this Box if it is full; otherwise return the specified default
   */
  def openOr[B >: A](default: => B): B = default

  /**
   * Apply a function to the value contained in this Box if it exists and return
   * a new Box containing the result, or Empty otherwise.
   * @return the modified Box or Empty
   */
  def map[B](f: A => B): Box[B] = Empty

  /**
   * Apply a function returning a Box to the value contained in this Box if it exists
   * and return the result, or Empty otherwise.
   * @return the modified Box or Empty
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
   * @return true if this Box's value satisfies the specified predicate
   */
  def exists(func: A => Boolean): Boolean = false

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
   * If the partial function is defined at the current Box's value
   * apply the partial function.
   */
  def collect[B](pf: PartialFunction[A, B]): Box[B] 

  /**
   * Return a Full[B] if the contents of this Box is of type <code>B</code>, otherwise return Empty
   */
  def asA[B](implicit m: Manifest[B]): Box[B] = Empty

  /**
   * Return this Box if Full, or the specified alternative if this is Empty
   */
  def or[B >: A](alternative: => Box[B]): Box[B] = alternative

  /**
   * Returns an Iterator over the value contained in this Box
   */
  def elements: Iterator[A] = Iterator.empty

  /**
   * Returns an Iterator over the value contained in this Box
   */
  def iterator: Iterator[A] = this.elements

  /**
   * Returns a List of one element if this is Full, or an empty list if Empty.
   */
  def toList: List[A] = Nil

  /**
   * Returns the contents of this box in an Option if this is Full, or
   * None if this is a failure or Empty.
   */
  def toOption: Option[A] = None

  /**
   * Transform an Empty to a Failure with the specified message.
   * @param msg the failure message
   * @return a Failure with the message if this Box is Empty
   */
  def ?~(msg: String): Box[A] = this

  /**
   * Transform an Empty to a ParamFailure with the specified typesafe
   * parameter.
   * @param errorCode a value indicating the error
   * @return a ParamFailure with the specified value
   */
  def ~>[T](errorCode: T): Box[A] = this

  /**
   * Alias for ?~
   */
  def failMsg(msg: String): Box[A] = ?~(msg)

  /**
   * Transform an Empty to a Failure with the specified message and chain
   * the new Failure to any previous Failure represented by this Box.
   * @param msg the failure message
   * @return a Failure with the message if this Box is an Empty Box. Chain the messages if it is already a Failure
   */
  def ?~!(msg: String): Box[A] = ?~(msg)

  /**
   * Alias for ?~!
   */
  def compoundFailMsg(msg: String): Box[A] = ?~!(msg)

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
  def run[T](in: T)(f: (T, A) => T) = in

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
   * is Empty return the specified alternative.
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
   * Equivalent to map(f).or(Full(dflt)).open_!
   */
  def dmap[B](dflt: => B)(f: A => B): B = dflt

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
}

/**
 * Full is a Box containing a value.
 */
@serializable
final case class Full[+A](value: A) extends Box[A] {

  def isEmpty: Boolean = false

  def open_! : A = value

  override def openOr[B >: A](default: => B): B = value

  override def or[B >: A](alternative: => Box[B]): Box[B] = this

  override def exists(func: A => Boolean): Boolean = func(value)

  override def filter(p: A => Boolean): Box[A] = if (p(value)) this else Empty

  override def foreach[U](f: A => U): Unit = f(value)

  override def map[B](f: A => B): Box[B] = Full(f(value))

  override def flatMap[B](f: A => Box[B]): Box[B] = f(value)

  override def elements: Iterator[A] = Iterator.fromValues(value)

  override def toList: List[A] = List(value)

  override def toOption: Option[A] = Some(value)

  override def run[T](in: T)(f: (T, A) => T) = f(in, value)

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


  override def isA[B](cls: Class[B]): Box[B] = value match {
    case value: AnyRef =>
      if (cls.isAssignableFrom(value.getClass)) Full(value.asInstanceOf[B])
      else Empty
    case _ => Empty
  }

  override def asA[B](implicit m: Manifest[B]): Box[B] = this.isA(m.erasure).asInstanceOf[Box[B]]

  override def ===[B >: A](to: B): Boolean = value == to

  override def dmap[B](dflt: => B)(f: A => B): B = f(value)

  /**
   * If the partial function is defined at the current Box's value
   * apply the partial function.
   */
  final def collect[B](pf: PartialFunction[A, B]): Box[B] = {
    if (pf.isDefinedAt(value)) Full(pf(value))
    else Empty
  }
}

/**
 * Singleton object representing an Empty Box
 */
@serializable
case object Empty extends EmptyBox

/**
 * The EmptyBox is a Box containing no value.
 */
@serializable
sealed abstract class EmptyBox extends Box[Nothing] {

  def isEmpty: Boolean = true

  def open_!  = throw new NullPointerException("Trying to open an empty Box")

  override def openOr[B >: Nothing](default: => B): B = default

  override def or[B >: Nothing](alternative: => Box[B]): Box[B] = alternative

  override def filter(p: Nothing => Boolean): Box[Nothing] = this

  override def ?~(msg: String): Failure = Failure(msg, Empty, Empty)

  override def ?~!(msg: String): Failure = Failure(msg, Empty, Empty)

  override def ~>[T](errorCode: T): ParamFailure[T] = 
    ParamFailure("", Empty, Empty, errorCode)


  /**
   * If the partial function is defined at the current Box's value
   * apply the partial function.
   */
  final override def collect[B](pf: PartialFunction[Nothing, B]): Box[B] = this
}

/**
 * Companion object used to simplify the creation of a simple Failure.
 */
object Failure {
  def apply(msg: String) = new Failure(msg, Empty, Empty)
}

/**
 * A Failure is an Empty with an additional failure message explaining the reason for its being empty.
 * It can also optionally provide an exception or a chain of causes represented as a list of other Failure objects
 */
@serializable
sealed case class Failure(msg: String, exception: Box[Throwable], chain: Box[Failure]) extends EmptyBox {
  type A = Nothing

  override def open_! = throw new NullPointerException("Trying to open a Failure Box: " + msg) {
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

  def messageChain: String = (this :: chainList).map(_.msg).mkString(" <- ")

  override def equals(other: Any): Boolean = (this, other) match {
    case (Failure(x, y, z), Failure(x1, y1, z1)) => (x, y, z) == (x1, y1, z1)
    case (x, y: AnyRef) => x eq y
    case _ => false
  }

  override def ?~(msg: String): Failure = this

  override def ?~!(msg: String): Failure = Failure(msg, Empty, Full(this))

  override def ~>[T](errorCode: T): ParamFailure[T] = ParamFailure(msg, exception, chain, errorCode)
}

/**
 * A ParamFailure is a Failure with an additional typesafe parameter that can
 * allow an application to store other information related to the failure.
 */
@serializable
final class ParamFailure[T](override val msg: String,
		            override val exception: Box[Throwable],
		            override val chain: Box[Failure], val param: T) extends
  Failure(msg, exception, chain) {
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

}
}
// vim: set ts=2 sw=2 et:
