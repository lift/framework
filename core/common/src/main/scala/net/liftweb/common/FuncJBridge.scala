/*
 * Copyright 2011-2026 Lift Committers and Contributors
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
 * Bridges from Java functions to Scala functions.
 *
 * The implicits defined here allow Scala code to interact seamlessly between
 * the Java function-like interfaces and the Scala function interfaces for
 * various function arities.
 *
 * In particular, there is a pair of implicits for each arity of function from 0
 * to 4. There is one implicit (called `lift`) from the Java function type to
 * the corresponding Scala function type and one (called `drop`) from the Scala
 * function type to the corresponding Java function type.
 */
class FuncJBridge {
  implicit def lift[Z](f: Func0[Z]): Function0[Z] = new Function0[Z] {
    def apply(): Z = f.apply()
  }

  implicit def drop[Z](f: Function0[Z]): Func0[Z] = new Func0[Z] {
    def apply(): Z = f.apply()
  }

  implicit def lift[A, Z](f: Func1[A, Z]): Function1[A, Z] = new Function1[A, Z] {
    def apply(a: A): Z = f.apply(a)
  }

  implicit def lift[A, B, Z](f: Func2[A, B, Z]): Function2[A, B, Z] = new Function2[A, B, Z] {
    def apply(a: A, b: B): Z = f.apply(a, b)
  }

  implicit def lift[A, B, C, Z](f: Func3[A, B, C, Z]): Function3[A, B, C, Z] = new Function3[A, B, C, Z] {
    def apply(a: A, b: B, c: C): Z = f.apply(a, b, c)
  }

  implicit def lift[A, B, C, D, Z](f: Func4[A, B, C, D, Z]): Function4[A, B, C, D, Z] = new Function4[A, B, C, D, Z] {
    def apply(a: A, b: B, c: C, d: D): Z = f.apply(a, b, c, d)
  }

  implicit def lift[Z](f: java.util.concurrent.Callable[Z]): Function0[Z] = new Function0[Z] {
    def apply(): Z = f.call()
  }
}
