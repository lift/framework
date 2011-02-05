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
package util {

import common._

/**
 * Companion object for FatLaxy.
 */
object FatLazy {
  /**
   * Create a new FatLazy.
   */
  def apply[T](f: => T) = new FatLazy(f)

  // implicit def fromLazy[T](in: Lazy[T]): T = in.get
}

/**
 * A class that does lazy evaluation
 *
 * @param f -- a function that evaluates to the default value of the instance
 */
class FatLazy[T](f: => T) {
  private var value: Box[T] = Empty

  /**
   * Get the value of the instance.  If it's not yet been set, call f to calculate it
   *
   * @return the value of the instance
   */
  def get: T = synchronized {
    value match {
      case Full(v) => v
      case _ => value = Full(f)
      value.open_!
    }
  }

  /**
   * Test whether the value of this class has been set or initialized from the default.
   */
  def defined_? = synchronized {
    value != None
  }

  /**
   * Set the instance to a new value and return that value
   *
   * @param v - the new value of the instance
   *
   * @return v
   */
  def set(v: T): T = synchronized {
    value = Full(v)
    v
  }

  /**
   * Copy the value of the specified FatLazy into this FatLazy
   */
  def setFrom(other: FatLazy[T]): Unit = synchronized {
    value = other.value
  }

  /**
   * and the lazy() = foo style of assignment
   */
  def update(v: T): Unit = set(v)

  /**
   * Reset the value of this FatLazy to the default (which will be lazily determined
   * on retrieval.)
   */
  def reset = synchronized {value = Empty}

  /**
   * Determine whether the value of this FatLazy has been determined.
   */
  def calculated_? = synchronized {value.isDefined}

  // implicit def fromLazy[T](in: Lazy[T]): T = in.get
}

/**
 * Sometimes, you want to do pattern matching against a lazy value.  Why?
 * Because, there may be parts of the pattern that must be evaluated first
 * and if they evaluate successfully, you then want to test another part of
 * the pattern. Thus, the LZ pattern match.
 */
object LZ {
  def apply[T](f: => T): LZ[T] = new LZ(f)
  def unapply[T](in: LZ[T]): Option[T] = Some(in.get)

 // implicit def lazyToT[T](in: LazyMatcher[T]): T = in.get
}

/**
 * LZ encapsulates a lazy value.
 *
 * @param f - a value to be evaluated lazily
 */
class LZ[T](f: => T) {
  lazy val get = f
  override def toString = "LZ("+get+")"
}

object ThreadLazy {
  def apply[T](f: => T) = new ThreadLazy(f)

  implicit def what[T](in: ThreadLazy[T]): T = in.get
}

/**
 * A thread-local lazy value that provides a means to evaluate
 * a function in a lazily-evaluated scope.
 *
 * @param theFunc the lazily-evaluated expression for which to
 * cache the result in thread-local scope.
 */
class ThreadLazy[TheType](theFunc: => TheType) extends LoanWrapper {
  private val calced = new ThreadGlobal[Boolean]
  private val value = new ThreadGlobal[TheType]

  /**
   * Save the current cached lazy value, if any, evaluate the specified
   * function and then restore the previous value to the cache. The effect
   * of this function is to essentially perform a reset of this lazy value
   * to being unevaluated prior to function evaluation.
   */
  def apply[T](f: => T): T = {
    val old = value.value
    calced.set(false)
    try {
      f
    } finally {
      calced.set(false)
      value.set(old)
    }
  }

  /**
   * Reset the lazy value so that it will be recalculated from the default expression
   * on the next retrieval.
   */
  def reset(): Unit = calced.set(false)

  /**
   * Return the value, evaluating the default expression if necessary.
   */
  def get: TheType = {
    if (calced.value) value.value
    else {
      value.set(theFunc)
      calced.set(true)
      value.value
    }
  }
}

}
}
