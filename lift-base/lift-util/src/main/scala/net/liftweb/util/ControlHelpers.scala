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

object ControlHelpers extends ControlHelpers with ClassHelpers

/**
 * Control helpers provide alternate ways to catch exceptions and ignore them as necessary
 */
trait ControlHelpers extends ClassHelpers {

  /**
   * Wraps a "try" block around the function f. If f throws
   * an exception with its class in the 'ignore' list or if 'ignore' is
   * null or an empty list, ignore the exception and return None.
   *
   * @param ignore - a list of exception classes to ignore. A thrown exception will be ignored if it is assignable from one of
   * the exception classes in the list
   * @param onError - an optional callback function that will use the thrown exception as a parameter
   * @param f - the block of code to evaluate
   * @return <ul>
   *   <li>Full(result of the evaluation of f) if f doesn't throw any exception
   *   <li>a Failure if f throws an exception
   *   <li>Empty if the exception class is in the ignore list
   *   </ul>
   */
  def tryo[T](ignore: List[Class[_]], onError: Box[Throwable => Unit])(f: => T): Box[T] = {
    try {
      Full(f)
    } catch {
      case c if ignore.exists(_.isAssignableFrom(c.getClass)) => onError.foreach(_(c)); Empty
      case c if (ignore == null || ignore.isEmpty) => onError.foreach(_(c)); Failure(c.getMessage, Full(c), Empty)
    }
  }

  /**
   * Wraps a "try" block around the function f. If f throws
   * an exception with its class in the 'ignore' list or if 'ignore' is
   * null or an empty list, ignore the exception and return None.
   *
   * @param handler - A partial function that handles exceptions
   * @param f - the block of code to evaluate
   * @return <ul>
   *   <li>Full(result of the evaluation of f) if f doesn't throw any exception
   *   <li>a Failure if f throws an exception
   *   </ul>
   */
  def tryo[T](handler: PartialFunction[Throwable, T], f: => T): Box[T] = {
    try {
      Full(f)
    } catch {
      case t if handler.isDefinedAt(t) => Full(handler(t))
      case e => Failure(e.getMessage, Full(e), Empty)
    }
  }

  /**
   * Wraps a "try" block around the function f
   * @param f - the block of code to evaluate
   * @return <ul>
   *   <li>Full(result of the evaluation of f) if f doesn't throw any exception
   *   <li>a Failure if f throws an exception
   *   </ul>
   */
  def tryo[T](f: => T): Box[T] = tryo(Nil, Empty)(f)


  /**
   * Wraps a "try" block around the function f and trigger a callback function if an exception is thrown
   * @param onError - an optional callback function that will use the thrown exception as a parameter
   * @param f - the block of code to evaluate
   * @return <ul>
   *   <li>Full(result of the evaluation of f) if f doesn't throw any exception
   *   <li>a Failure if f throws an exception
   *   </ul>
   */
  def tryo[T](onError: Throwable => Unit)(f: => T): Box[T] = tryo(Nil, Full(onError))(f)

  /**
   * Wraps a "try" block around the function f
   * @param ignore - a list of exception classes to ignore. A thrown exception will be ignored if it is assignable from one of
   * the exception classes in the list
   * @param f - the block of code to evaluate
   * @return <ul>
   *   <li>Full(result of the evaluation of f) if f doesn't throw any exception
   *   <li>a Failure if f throws an exception
   *   <li>Empty if the exception class is in the ignore list
   *   </ul>
   */
  def tryo[T](ignore: List[Class[_]])(f: => T): Box[T] = tryo(ignore, Empty)(f)

  /**
   * Wraps a "try" block around the function f. Takes only one Class of exception to ignore
   * @param ignore - a single exception classes to ignore. A thrown exception will be ignored if it is assignable from this class.
   * @param f - the block of code to evaluate
   * @return <ul>
   *   <li>Full(result of the evaluation of f) if f doesn't throw any exception
   *   <li>a Failure if f throws an exception
   *   <li>Empty if the exception class is in the ignore list
   *   </ul>
   */
  def tryo[T](ignore: Class[_])(f: => T): Box[T] = tryo(List(ignore), Empty)(f)


}

}
}
