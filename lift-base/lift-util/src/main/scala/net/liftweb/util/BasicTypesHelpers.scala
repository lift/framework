/*
 * Copyright 2006-2010 WorldWide Conferencing, LLC
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
package util {

import _root_.java.io.{InputStream, ByteArrayOutputStream, ByteArrayInputStream, Reader, File, FileInputStream, BufferedReader, InputStreamReader}
import _root_.scala.xml._
import common._

/**
 * This object adds functionality to Scala standard types.
 */
object BasicTypesHelpers extends BasicTypesHelpers with StringHelpers with ControlHelpers

/**
 * This trait adds functionality to Scala standard types
 */
trait BasicTypesHelpers { self: StringHelpers with ControlHelpers =>

  /**
   * Allows an implicit transform from a Boolean to a Boolean2, allowing expressions such as:
   * <code>(1 == 2) ? "a" | "b"</code> (This expression will return "b")
   * @param b the predicate to be tested by the ternary operator.
   */
  implicit def boolean2(b: => Boolean) = new Boolean2(b)

  /**
   * This decorator class adds a ternary operator to a Boolean value
   * @param b the predicate to be tested by the ternary operator.
   */
  class Boolean2(b: => Boolean) {
    /**
     * Ternary operator.
     * @returns a BooleanSome containing the specified value
     * if the decorated boolean is true, or a BooleanNone otherwise.
     */
    def ? [A](first: => A): BooleanOption[A] = {
      if (b) BooleanSome(() => first)
      else BooleanNone
    }

    /**
     * Class for return values from the Boolean2 ternary operator.
     * This class provides the "|" operator that can be used to
     * specify a default value (i.e. the RHS of the "or")
     */
    sealed abstract class BooleanOption[+A] {
      def |[B >: A](default: => B): B
    }

    /**
     * The value returned by the ternary operator if the predicate is true.
     */
    case class BooleanSome[+A](x: () => A) extends BooleanOption[A] {
      def |[B >: A](default: => B): B = x()
    }

    /**
     * The value returned by the ternary operator if the predicate is false.
     */
    case object BooleanNone extends BooleanOption[Nothing] {
      def |[B](default: => B): B  = default
    }
  }

  /**
   * Implicit transformation from a Boolean expression to an OptionalCons object so
   * that an element can be added to a list if the expression is true
   */
  implicit def toOptiCons(expr: => Boolean): OptionalCons = new OptionalCons(expr)

  /**
   * promote a partial function such that we can invoke the guard method
   * to wrap the guarded partial function with a guard
   */
  implicit def pfToGuardable[A](in: PartialFunction[A, _]):
                         PartialFunctionWrapper[A] =
                           new PartialFunctionWrapper[A](in)

  /**
   * Convert any object to an "equivalent" Boolean depending on its value
   */
  def toBoolean(in: Any): Boolean = {
    in match {
      case null => false
      case b : Boolean => b
      case i: Int => i != 0
      case lo: Long => lo != 0
      case n : Number => n.intValue != 0
      case s : String => {
        val sl = s.toLowerCase
        if (sl.length == 0) false
        else {
          if (sl.charAt(0) == 't') true
          else if (sl == "yes") true
          else toInt(s) != 0
        }
      }
      case None => false
      case Empty | Failure(_, _, _) => false
      case Full(n) => toBoolean(n)
      case Some(n) => toBoolean(n)
      case x :: xs => toBoolean(x)
      case o => toBoolean(o.toString)
    }
  }

  /**
   * A helper that will convert the String to a Boolean if it's
   * t, true, yes, 1, f, false, no, or 0
   */
  def asBoolean(in: String): Box[Boolean] = AsBoolean.unapply(in)

/**
* A helpful Boolean extractor
*/
object AsBoolean {
  def unapply(in: String): Option[Boolean] =
  if (null eq in) None else
  in.toLowerCase match {
    case "t" | "true" | "yes" | "1" => Full(true)
    case "f" | "false" | "no" | "0" => Full(false)
    case _ => None
  }
}

  /**
   * Safely convert the specified String to an Int.
   */
  def asInt(in: String): Box[Int] = tryo{in.trim.toInt}

/**
* A helpful Int extractor
*/
object AsInt {
  def unapply(in: String): Option[Int] = asInt(in)
}

  /**
   * Safely convert the specified String to an Int.
   */
  def asDouble(in: String): Box[Double] = tryo{in.trim.toDouble}

/**
* A helpful Int extractor
*/
object AsDouble {
  def unapply(in: String): Option[Double] = asDouble(in)
}

  /**
   * Safely convert the specified String to a Long.
   */
  def asLong(in: String): Box[Long] = tryo(in.toLong)


/**
* A helpful Long extractor
*/
object AsLong {
  def unapply(in: String): Option[Long] = asLong(in)
}


/**
   * Convert any object to an "equivalent" Long depending on its value
   */
  def asLong(in: Any): Box[Long] = {
    in match {
      case null => Empty
      case i: Int => Full(i.toLong)
      case n: Long => Full(n)
      case d: _root_.java.util.Date => Full(d.getTime)
      case n : Number => Full(n.longValue)
      case (n: Number) :: _ => Full(n.longValue)
      case Some(n) => asLong(n)
      case Full(n) => asLong(n)
      case None | Empty | Failure(_, _, _) => Empty
      case s: String => asLong(s)
      case x :: xs => asLong(x)
      case o => asLong(o.toString)
    }
  }

  /**
   * Convert any object to an "equivalent" Int depending on its value
   */
  def toInt(in: Any): Int = {
    in match {
      case null => 0
      case n: Int => n
      case lo: Long => lo.toInt
      case n : Number => n.intValue
      case (n: Number) :: _ => n.intValue
      case Some(n) => toInt(n)
      case Full(n) => toInt(n)
      case None | Empty | Failure(_, _, _) => 0
      case s: String => parseNumber(s).toInt
      case d: _root_.java.util.Date => (d.getTime / 1000L).toInt
      case x :: xs => toInt(x)
      case o => toInt(o.toString)
    }
  }

  /**
   * Convert any object to an "equivalent" Long depending on its value
   */
  def toLong(in: Any): Long = {
    in match {
      case null => 0L
      case i: Int => i
      case n: Long => n
      case d: _root_.java.util.Date => d.getTime
      case n : Number => n.longValue
      case (n: Number) :: _ => n.longValue
      case Some(n) => toLong(n)
      case Full(n) => toLong(n)
      case None | Empty | Failure(_, _, _) => 0L
      case s: String => parseNumber(s)
      case x :: xs => toLong(x)
      case o => toLong(o.toString)
    }
  }

  /**
   * Convert any InputStream to a ByteArrayInputStream
   */
  def toByteArrayInputStream(in: InputStream) = {
    val ba = new Array[Byte](4096)
    val bos = new ByteArrayOutputStream
    var len = 0
    while (len >= 0) {
      len = in.read(ba)
      if (len > 0) {
        bos.write(ba, 0, len)
      }
    }
    new ByteArrayInputStream(bos.toByteArray)
  }

  /**
   * Compare two arrays of Byte for byte equality.
   * @return true if two Byte arrays don't contain the same bytes
   */
  def notEq(a: Array[Byte], b: Array[Byte]) = !isEq(a,b)

  /**
   * Compare two arrays of Byte for byte equality.
   * @return true if two Byte arrays contain the same bytes
   */
  def isEq(a: Array[Byte], b: Array[Byte]) = {
    def eq(a: Array[Byte], b: Array[Byte], pos: Int, len: Int): Boolean = {
      if (pos == len) true
      else if (a(pos) != b(pos)) false
      else eq(a , b, pos + 1, len)
    }
    a.length == b.length && eq(a, b, 0, a.length)
  }
}

/**
 * Optional cons that implements the expression: <code>expr ?> value ::: List</code>
 * @param expr the predicate to evaluate
 */
final class OptionalCons(expr: => Boolean) {
  /**
   * Return the specified value in a single-element list if the predicate
   * evaluates to true.
   */
  def ?>[T](f: => T): List[T] = if (expr) List(f) else Nil
}

/**
 * The helper class that facilitates wrapping of one PartialFunction
 * around another
 */
final class PartialFunctionWrapper[A](around: PartialFunction[A, _]) {
  /**
   * Allows you to put a guard around a partial function
   * such that the around's isDefinedMethod must return true
   * before the other's isDefinedAt method is tested
   */
  def guard[B](other: PartialFunction[A, B]): PartialFunction[A,B] =
    new PartialFunction[A, B] {
      def isDefinedAt(a: A) = around.isDefinedAt(a) && other.isDefinedAt(a)
      def apply(a: A): B = other.apply(a)
    }
    
}


}
}
