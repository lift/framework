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

import common._

object ListHelpers extends ListHelpers

/**
 * The ListHelpers trait provides useful functions which can be applied to Lists.<p/>
 */
trait ListHelpers {

  /**
   * Returns a Full can with the first element x of the list in
   * for which f(x) evaluates to true. If f(x) evaluates to false
   * for every x, then an Empty can is returned.
   *
   * @param in  a list of elements to which f can be applied
   * @param f   a function that can be applied to elements of in
   *
   * @return a Box containing the found element (or Empty if not found)
   */
  def first_?[B](in: Seq[B])(f: => B => Boolean): Box[B] =
    Box(in.find(f))

  /**
   * Returns the first application of f to an element of in that
   * results in a Full can. If f applied to an element of in results
   * in an Empty can, then f will be applied to the rest of the
   * elements of in until a Full can results. If the list runs out
   * then an Empty can is returned.
   *
   * @param in  a list of elements to which f can be applied
   * @param f   a function that can be applied to elements of in
   *
   * @return a Box containing the first Full can or Empty if f never returns a Full can
   */
  def first[B, C](in: Seq[B])(_f: B => Box[C]): Box[C] = {
    val f: B => Iterable[C] = _f andThen Box.box2Iterable[C]
    // We use toStream here to avoid multiple execution of "f" for each element access (Issue #596)
    Box(in.toStream.flatMap(f).headOption)
  }

  /**
   * This class add a case insensitive get to a List of Pairs of String, as if it was a Map
   */
  class ListMapish(val theList: Seq[(String, String)]) {
    /**
     * Return a Box containing the second element of the first pair having key as the first element
     * The comparison is made ignoring the case of the keys
     *
     * @param key the string to find
     *
     * @return a Full can containing the found value or Empty
     */
    def ciGet(swhat: String): Box[String] = {
      val what = swhat.toLowerCase
      def tGet(in: Seq[(String, String)]): Box[String] =
        in match {
          case Nil => Empty
          case x :: xs if (x._1.toLowerCase == what) => Full(x._2)
          case x :: xs => tGet(xs)
        }
      tGet(theList)
    }
  }

  /** adds the ciGet method to a List of Pairs of Strings */
  implicit def listToListMapish(in: Seq[(String, String)]): ListMapish = new ListMapish(in)

  /**
   * Convert a java.util.Enumeration to a List[T]
   */
  def enumToList[T](enum: _root_.java.util.Enumeration[T]): List[T] = {
    if (enum.hasMoreElements) {
      val next = enum.nextElement
      next :: enumToList(enum)
    } else Nil
  }

  /**
   * Convert a java.util.Enumeration to a List[String] using the toString method on each element
   */
  def enumToStringList[C](enum: _root_.java.util.Enumeration[C]): List[String] =
    if (enum.hasMoreElements) enum.nextElement.toString :: enumToStringList(enum) else Nil

  /**
   * Return the first element of a List or a default value if the list is empty
   */
  def head[T](l: Seq[T], deft: => T) = l.headOption.getOrElse(deft)

  /**
   * Return a list containing the element f if the expression is true
   */
  def listIf[T](expr: Boolean)(f: => T): List[T] = if (expr) List(f) else Nil

  /**
   * Given an incoming list, return a set of lists that is the original list rotated through all its positions
   *
   * @param in the list to rotate
   *
   * @return all the rotations of the list
   */
  def rotateList[T](in: Seq[T]): List[List[T]] = {
    def doIt(in: List[T], cnt: Int): List[List[T]] = ((in, cnt): @unchecked) match {
      case (_, 0) => Nil
      case (x :: xs, cnt) => in :: doIt(xs ::: List(x), cnt - 1)
    }
    doIt(in.toList, in.length)
  }

  /**
   * Given a list, return all the permutations of the list.
   *
   * @param in -- the list
   *
   * @return all the permutations of the list
   */
  def permuteList[T](in: Seq[T]): List[List[T]] = (in.toList: @unchecked) match {
    case Nil => Nil
    case x :: Nil => List(List(x))
    case xs => rotateList(xs).flatMap(x => (x: @unchecked) match {case x :: xs => permuteList(xs).map(x :: _) case _ => Nil})
  }

  /**
   * Given a list, return all the permutations including the removal of items (does not return a Nil list unless in is Nil).
   *
   * @param in the list to permute
   *
   * @return all the permutations of the list including sublists, sorted in longest to shortest
   */
  def permuteWithSublists[T](in: Seq[T]): List[List[T]] = {
    def internal(in: List[T]): List[List[T]] = in match {
      case Nil => Nil
      case x :: Nil => List(List(x))
      case xs => val rot = rotateList(xs)
      val ret = rot.flatMap(z => (z: @unchecked) match {case x :: xs => permuteList(xs).map(x :: _)})
      ret ::: rot.map(z => (z: @unchecked) match {case x :: xs => xs}).flatMap(internal(_))
    }
    internal(in.toList).removeDuplicates.sortWith(_.length > _.length)
  }

  /** Add utility methods to Lists */
  implicit def toSuperList[T](in: List[T]): SuperList[T] = new SuperList(in)

  /** Add utility methods to Lists */
  class SuperList[T](val what: List[T]) {
    /** permute the elements of a list */
    def permute = permuteList(what)

    /** return all the permuations of a list */
    def rotate = rotateList(what)

    /** return all the permuations of a list, including its sublists */
    def permuteAll = permuteWithSublists(what)

    /** return the first element of a list or a default element of the same type */
    def headOr(other: => T): T = head(what, other)

    /** return the list if not empty or another list */
    def or(other: => List[T]): List[T] = if (!what.isEmpty) what else other

    /** return a string with all elements toString values appended */
    def str: String = what.mkString("")

    /** return all elements separated by a comma */
    def comma: String = what.mkString(", ")

    /** alias for mkString */
    def join(str: String) = what.mkString(str)

    /** return true if not empty */
    def ? : Boolean = !what.isEmpty

    /** return a new list where the element at position pos is replaced with another element */
    def replace(pos: Int, withWhat: T): List[T] = {
      def repl(pos: Int, withWhat: T, rest: List[T]): List[T] = rest match {
        case Nil => Nil
        case x :: xs if pos <= 0 => withWhat :: xs
        case x :: xs => x :: repl(pos - 1, withWhat, xs)
      }
      repl(pos, withWhat, what)
    }
  }
}

}
}
