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
      import HLists._

      val x = 1 :+: "Foo" :+: HNil

      val head: Int = x.head
      val head2: String = x.tail.head

      x.head must_== 1
      x.tail.head must_== "Foo"
    }

    
  }

  "A combinable box" should {
    "build a box with a failure must be a failure" in {
      import CombinableBox._

      val x = Full("a") :&: Full(1) :&: Empty

      // result in a failure
      x match {
        case Left(_) => true must_== true
      }
    }

    "build a box with all Full must match" in {
      import CombinableBox._
      import HLists._

      val x = Full("a") :&: Full(1) :&: Full(List(1,2,3))

      // result in a failure
      x match {
        case Right(a :+: one :+: lst :+:HNil) => {
          // val a2: Int = a  fails... not type safe

          val as: String = a
          val onei: Int = one
          val lstl: List[Int] = lst

          true must_== true
        }
      }
    }

    "Use in for comprehension" in {
      import CombinableBox._
      import HLists._

      val res = for {
        a :+: one :+: lst :+: _ <- 
        (Full("a") ?~ "Yak" :&: Full(1) :&: Full(List(1,2,3))) ?~! "Dude"
      } yield a.length * one * lst.foldLeft(1)(_ * _)

      res must_== Full(6)
    }


  }
}

}
}
