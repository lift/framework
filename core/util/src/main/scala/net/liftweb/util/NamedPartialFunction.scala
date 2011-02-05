/*
 * Copyright 2008-2010 WorldWide Conferencing, LLC
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

/**
 * This trait is used to represent a PartialFunction with additional
 * associated metadata, a name that allows the NamedPartialFunction
 * to be looked up dynamically.
 */
trait NamedPartialFunction[-A, +B] extends PartialFunction[A, B] {
  def functionName: String
}

/**
 * This class is the base implementation of the NamedPartialFunction trait.
 */
class NamedPF[-A, +B](name: String, f: PartialFunction[A, B]) extends NamedPartialFunction[A, B] {
  override def isDefinedAt(x: A): Boolean = f.isDefinedAt(x)
  override def apply(x: A): B = f(x)
  val functionName = name
}

object NamedPF {
  /**
   * Curried constructor for NamedPF
   */
  def apply[A, B](name: String)(f: PartialFunction[A,B]):
  NamedPartialFunction[A,B] = new NamedPF(name, f)

  /**
   * Find the first partial function in the specified sequence that
   * is defined at the given value.
   *
   * @param value the value to use to test each PartialFunction
   * @param lst the sequence to search for a PartialFunction defined at <code>value</code>
   * @return a Full Box containing the PartialFunction if found,
   * or Empty othewise.
   */
  def find[A, B](value: A, lst: Seq[PartialFunction[A, B]]):
  Box[PartialFunction[A, B]] = lst.find(_.isDefinedAt(value))

  /**
   * Determine whether any PartialFunction in the specified sequence
   * is defined at the specified value.
   *
   * @param value the value to use to test each PartialFunction
   * @param lst the sequence to search for a PartialFunction defined at <code>value</code>
   * @return whether such a PartialFunction is found
   */
  def isDefinedAt[A, B](value: A, lst: Seq[PartialFunction[A, B]]): Boolean =
  find(value, lst).isDefined

  /**
   * Find the first PartialFunction in the specified sequence that is defined
   * at the specified value, apply it to that value and return the result
   * or throw a MatchError on failure to find such a function.
   *
   * @param value the value to use to test each PartialFunction
   * @param lst the sequence to search for a PartialFunction defined at <code>value</code>
   * @return the result of applying any such PartialFunction to the specified value.
   * @throws MatchError on failure to find such a PartialFunction
   */
  def apply[A, B](value: A, lst: Seq[PartialFunction[A, B]]): B =
  find(value, lst) match {
    case Full(pf) => pf.apply(value)
    case _ => throw new MatchError(value)
  }

  /**
   * Find the first PartialFunction in the specified sequence that is defined
   * at the specified value, apply it to that value and return the result
   * in a Full Box if found; return Empty otherwise
   *
   * @param value the value to use to test each PartialFunction
   * @param lst the sequence to search for a PartialFunction defined at <code>value</code>
   * @return a Full Box containing the result of applying the first PartialFunction which is
   * defined at the specified value to that value, or Empty if no such PartialFunction is found
   */
  def applyBox[A, B](value: A, lst: Seq[PartialFunction[A, B]]): Box[B] =
  find(value, lst).map(_.apply(value))
}

}
}
