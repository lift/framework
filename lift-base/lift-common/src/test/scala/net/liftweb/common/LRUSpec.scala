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
import _root_.org.specs.ScalaCheck
import _root_.org.scalacheck.Gen._
import _root_.org.scalacheck._
import _root_.org.scalacheck.Arbitrary._
import _root_.org.scalacheck.Prop.{forAll}


class LRUTest extends Runner(LRUUnit) with JUnit
object LRUUnit extends Specification {
  "LRU" can {
    "never grow beyond a certain size" in {
      val lru = new LRUMap[Int, Int](10)
      for (i <- 1 to 20) lru(i) = i

      lru.size mustBe 10
    }

    "Has the last 10 elements" in {
      val lru = new LRUMap[Int, Int](10)
      for (i <- 1 to 20) lru(i) = i

      lru.size mustBe 10
      for (i <- 11 to 20) lru(i) must_== i
    }


    "Expire elements to func" in {
      var expCnt = 0
      val lru = new LRUMap[Int, Int](10, Empty, (k, v) => {expCnt += 1; k must_== v; k must be > 0; v must be < 11})
      for (i <- 1 to 20) lru(i) = i

      lru.size mustBe 10
      expCnt must_== 10
      for (i <- 11 to 20) lru(i) must_== i
    }

    "Don't expire recently accessed" in {
      var expCnt = 0
      val lru = new LRUMap[Int, Int](10, Empty, (k, v) => {expCnt += 1; k must_== v; k must be > 0})
      for (i <- 1 to 20) {
        for (q <- 1 to 10) lru.get(q)
        lru(i) = i
      }

      lru.size mustBe 10
      for (i <- 2 to 10) lru(i) must_== i
      lru(20) must_== 20
    }

  }
}

}
}
