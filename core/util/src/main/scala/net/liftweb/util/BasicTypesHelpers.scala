/*
 * Copyright 2006-2011 WorldWide Conferencing, LLC
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
package util

import java.io.{InputStream, ByteArrayOutputStream, ByteArrayInputStream, Reader, File, FileInputStream, BufferedReader}
import scala.xml._
import common._

/**
 * Generics on the JVM have an issues with Type Erasure. Basically, Generic types (e.g.,
 * Function1[String, Int] and Function1[Double, Bool]) look like the same type to the JVM
 * so that methods cannot be overloaded with generic types. This "marker" trait is used as
 * a work-around to the issue.  The marker is implicitly passed as a parameter to some overloaded
 * methods.  If you see this as an implicit parameter to an overloaded method, just ignore it.
 */
trait AvoidTypeErasureIssues1

/**
 * The companion object that does the implicit vending of AvoidTypeErasureIssues1
 */
object AvoidTypeErasureIssues1 {
  /**
   * Automagically vend a AvoidTypeErasureIssues1
   */
  implicit val vendOne: AvoidTypeErasureIssues1 = new AvoidTypeErasureIssues1 {}
}

/**
 * This object adds functionality to Scala standard types.
 */
object BasicTypesHelpers extends BasicTypesHelpers with StringHelpers with ControlHelpers

/**
 * This trait adds functionality to Scala standard types
 */
trait BasicTypesHelpers { self: StringHelpers with ControlHelpers =>
  /**
   * This decorator class adds a ternary operator to a Boolean value
   * @param b the predicate to be tested by the ternary operator.
   */
  implicit class Boolean2(b: => Boolean) {
    /**
     * Ternary operator.
     * @return a BooleanSome containing the specified value
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
   * Compare two NodeSeq and return true if they are equal, even if
   * attribute order of Elems is different
   */
  def compareXml(left: NodeSeq, right: NodeSeq): Boolean = {
    val ls: Seq[Node] = left.toSeq
    val rs: Seq[Node] = right.toSeq
    if (ls.length == rs.length) {
     ls.zip(rs).foldLeft(true){case (b, (l, r)) => b && compareNode(l, r)}
    } else {
      false
    }
  }

  /**
   * Compare two Elems
   */
  def compareElem(left: Elem, right: Elem): Boolean =
    compareXml(left.child, right.child) &&
  left.label == right.label &&
  (((null eq left.prefix) && (null eq right.prefix)) || left.prefix == right.prefix) &&
    left.scope == right.scope &&
    compareMetaData(left.attributes.toList, right.attributes.toList)

  private def findFilter(m: MetaData, lst: List[MetaData]): Box[List[MetaData]] = {
    var found = false
    val ret = lst.filter {
      case PrefixedAttribute(pre, label, value, _) if !found =>
        m match {
          case PrefixedAttribute(p2, l2, v2, _) if p2 == pre && l2 == label && v2.text == value.text =>
            found = true
            false
          case _ => true
        }
      case UnprefixedAttribute(label, value, _) if !found =>
        m match {
          case UnprefixedAttribute(l2, v2, _) if l2 == label && v2.text == value.text =>
            found = true
            false
          case _ => true
        }
      case _ => true
    }
    if (found) Full(ret) else Empty
  }

  /**
   * Compare the metadata of two attributes
   */
    def compareMetaData(left: List[MetaData], right: List[MetaData]): Boolean =
    (left, right) match {
      case (Nil, Nil) => true
      case (_, Nil) => false
      case (Nil, _) => false
      case (attr :: rl, right) => findFilter(attr, right) match {
        case Full(rr) => compareMetaData(rl, rr)
        case _ => false
      }
      case _ => false
    }

  /**
   * Comparse two XML nodes
   */
  def compareNode(left: Node, right: Node): Boolean = {
    (left, right) match {
      case (Group(gl), Group(gr)) => compareXml(gl, gr)
      case (el: Elem, er: Elem) => compareElem(el, er)
      case (Unparsed(tl), Unparsed(tr)) => tl == tr
      case (Text(tl), Text(tr)) => tl == tr

      case (el: EntityRef, er: EntityRef) => el === er
      case (Comment(cl), Comment(cr)) => cl == cr
      case (PCData(dl), PCData(dr)) => dl == dr
      case (pl: ProcInstr, pr: ProcInstr) => pl === pr
      case (a, b) => a.toString == b.toString
    }

  }

  /**
   * Optional cons that implements the expression: <code>expr ?> value ::: List</code>
   * @param expr the predicate to evaluate
   */
  final implicit class OptionalCons(expr: => Boolean) {
    /**
     * Return the specified value in a single-element list if the predicate
     * evaluates to true.
     */
    def ?>[T](f: => T): List[T] = if (expr) List(f) else Nil
  }

  /**
   * A helper class that facilitates wrapping of one PartialFunction
   * around another
   */
  final implicit class PartialFunctionWrapper[A](around: PartialFunction[A, _]) {
    /**
     * Allows you to put a guard around a partial function
     * such that the around's isDefinedAt method must return true
     * before the other's isDefinedAt method is tested
     */
    def guard[B](other: PartialFunction[A, B]): PartialFunction[A,B] =
      new PartialFunction[A, B] {
        def isDefinedAt(a: A) = around.isDefinedAt(a) && other.isDefinedAt(a)
        def apply(a: A): B = other.apply(a)
      }
  }

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
      case s : String =>  asBoolean(s) openOr false
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
    case "t" | "true"  | "yes" | "1" | "on"  => Full(true)
    case "f" | "false" | "no"  | "0" | "off" => Full(false)
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
   * Safely convert the specified String to a Double.
   */
  def asDouble(in: String): Box[Double] = tryo{in.trim.toDouble}

/**
* A helpful Double extractor
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
      case d: java.util.Date => Full(d.getTime)
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
      case d: java.util.Date => (d.getTime / 1000L).toInt
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
      case d: java.util.Date => d.getTime
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

