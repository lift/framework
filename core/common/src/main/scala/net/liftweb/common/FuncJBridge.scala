/*
 * Copyright 2011 WorldWide Conferencing, LLC
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
package common

import scala.language.implicitConversions

object FuncJBridge extends FuncJBridge

/**
 * Bridge from Java functions to Scala functions
 */
class FuncJBridge {
  /**
   * Lift the Java Func0 to a Scala Function0
   */
  implicit def lift[Z](f: Func0[Z]): Function0[Z] = new Function0[Z] {
    def apply(): Z = f.apply()
  }

  /**
   * Drop from Scala function to Java function
   */
  implicit def drop[Z](f: Function0[Z]): Func0[Z] = new Func0[Z] {
    def apply(): Z = f.apply()
  }

  /**
   * Lift the Java Func1 to a Scala Function1
   */
  implicit def lift[A, Z](f: Func1[A, Z]): Function1[A, Z] = new Function1[A, Z] {
    def apply(a: A): Z = f.apply(a)
  }

  /**
   * Lift the Java Func2 to a Scala Function2
   */
  implicit def lift[A, B, Z](f: Func2[A, B, Z]): Function2[A, B, Z] = new Function2[A, B, Z] {
    def apply(a: A, b: B): Z = f.apply(a, b)
  }

  /**
   * Lift the Java Func3 to a Scala Function3
   */
  implicit def lift[A, B, C, Z](f: Func3[A, B, C, Z]): Function3[A, B, C, Z] = new Function3[A, B, C, Z] {
    def apply(a: A, b: B, c: C): Z = f.apply(a, b, c)
  }

  /**
   * Lift the Java Func4 to a Scala Function4
   */
  implicit def lift[A, B, C, D, Z](f: Func4[A, B, C, D, Z]): Function4[A, B, C, D, Z] = new Function4[A, B, C, D, Z] {
    def apply(a: A, b: B, c: C, d: D): Z = f.apply(a, b, c, d)
  }

  /**
   * Lift the Java Callable to a Scala Function0
   */
  implicit def lift[Z](f: java.util.concurrent.Callable[Z]): Function0[Z] = new Function0[Z] {
    def apply(): Z = f.call()
  }
}
