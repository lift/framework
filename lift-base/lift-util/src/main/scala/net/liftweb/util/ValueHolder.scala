/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
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

trait ValueHolder {
  type ValueType

  /**
   * Get the value.  Use get.
   *
   * @deprecated
   */
  def is: ValueType

  /**
   * An alternative way to get the value
   */
  def get: ValueType
}

/**
 * A value that can be set
 */
trait Settable extends ValueHolder {
  def set(in: ValueType): ValueType
}

trait SettableValueHolder extends Settable

trait PValueHolder[T] extends ValueHolder {
 type ValueType = T
}

object PValueHolder {
  implicit def tToVHT[T](in: T): PValueHolder[T] = new PValueHolder[T] {def is = in; def get = is}
  def apply[T](in: T) = tToVHT(in)
}

object ValueHolder {
  implicit def tToVHT[T](in: T): ValueHolder = new PValueHolder[T] {def is = in; def get = is}
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

}
}
