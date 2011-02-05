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

/**
 * This is a decorator for a ThreadLocal variable that provides
 * convenience methods to transform the variable to a Box and execute
 * functions in a "scope" wherein the variable may hold a different value.
 */
class ThreadGlobal[T]
{
  private val threadLocal = new ThreadLocal[T]

  /**
   * Returns the current value of this variable.
   */
  def value: T = threadLocal.get

  /**
   * Returns a Box containing the value of this ThreadGlobal
   * in a null-safe fashion.
   */
  def box: Box[T] = Box !! value

  /**
   * Sets the value of this ThreadGlobal.
   * @param v the value to set.
   */
  def set(v: T): ThreadGlobal[T] = {
    threadLocal.set(v)
    this
  }

  /**
   * Alias for <code>set(v: T)</code>
   * @param v the value to set.
   */
  def apply(v: T) = set(v)

  /**
   * Sets this ThreadGlobal's contents to the specified value,
   * executes the specified function, and then restores the ThreadGlobal
   * to its earlier value. This effectively creates a scope within
   * the execution of the current thread for the execution of the specified
   * function.
   *
   * @param x the value to temporarily set in this ThreadGlobal
   * @param f the function to execute
   */
  def doWith[R](x: T)(f : => R) : R = {
    val original = value
    try {
      threadLocal.set(x)
      f
    } finally {
      threadLocal.set(original)
    }
  }
}

trait DynoVar[T] {
  private val threadLocal = new ThreadLocal[T]
  // threadLocal.set(Empty)

  def is: Box[T] = Box !! threadLocal.get

  def get = is

  def set(v: T): this.type = {
    threadLocal.set(v)
    this
  }

  def run[S](x: T)(f: => S): S = {
    val original = threadLocal.get
    try {
      threadLocal.set(x)
      f
    } finally {
      threadLocal.set(original)
    }
  }
}

}
}
