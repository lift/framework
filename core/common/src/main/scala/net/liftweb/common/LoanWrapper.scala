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
package common {

/**
 * This trait defines the principle contract for function objects that
 * wrap the processing of HTTP requests by Lift while utilizing the preestablished
 * request-local scope.
 */
trait CommonLoanWrapper {
  /**
   * Implementations of this method may either call f to continue processing
   * the wrapped call as normal, or may ignore f to entirely replace the
   * wrapped call with a custom implementation
   * @param f the delegate which provides processing by the underlying framework
   */
  def apply[T](f: => T): T
}

object CommonLoanWrapper {
  /**
  * If you have a List of LoanWrappers, apply them and then the functions
  */
  def apply[T, LWT <: CommonLoanWrapper](lst: List[LWT])(f: => T): T = lst match {
    case Nil => f
    case x :: xs => x.apply(this.apply(xs)(f))
  }
}

}
}
