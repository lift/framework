/*
 * Copyright 2009-2011 WorldWide Conferencing, LLC
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

import scala.language.implicitConversions

trait ValueHolder {
  type ValueType

  /**
   * get the value
   */
  def get: ValueType
}

/**
 * A value that can be set
 */
trait Settable extends ValueHolder {
  def set(in: ValueType): ValueType

  /**
   * Perform an atomic update of this Settable.
   * The current value is passed to the function and the ValueHolder
   * is set to the result of the function.  This is enclosed in the
   * performAtomicOperation method which will, by default, synchronize
   * this instance
   */
  def atomicUpdate(f: ValueType => ValueType): ValueType =
    performAtomicOperation(set(f(get)))

  /**
   * Perform an atomic operation on the Settable. By default
   * synchronizes the instance, but it could use other mechanisms
   */
  def performAtomicOperation[T](f: => T): T = synchronized {
    f
  }
}

trait SettableValueHolder extends Settable

trait PValueHolder[T] extends ValueHolder {
 type ValueType = T
}

object PValueHolder {
  implicit def tToVHT[T](in: T): PValueHolder[T] = new PValueHolder[T] {def get = in; def is = get}
  def apply[T](in: T) = tToVHT(in)
}

object ValueHolder {
  implicit def tToVHT[T](in: T): ValueHolder = new PValueHolder[T] {def get = in; def is = get}
  def apply[T](in: T) = tToVHT(in)
}

trait PSettableValueHolder[T] extends PValueHolder[T] with SettableValueHolder

/**
 * Kinda like a JavaBean property.  It's something that can
 * be set and retrieved
 */
trait LiftValue[T] extends PSettableValueHolder[T] {
  def is: T = get
}

