/*
 * Copyright 2010 WorldWide Conferencing, LLC
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

import _root_.org.specs._
import _root_.net.liftweb.common.Box._
import _root_.org.specs.runner._
import _root_.org.specs.Sugar._

class HListSpecTest extends Runner(HListSpec) with JUnit with Console
object HListSpec extends Specification {
  "An HList" should {
    "Must get types right" in {
      import HList._

      val x = 1 :+: "Foo" :+: HNil

      val head: Int = x.head
      val head2: String = x.tail.head

      x.head must_== 1
      x.tail.head must_== "Foo"
    }

    
  }
}

}
}
