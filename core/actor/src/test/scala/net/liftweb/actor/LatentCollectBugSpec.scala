/*
 * Copyright 2009-2026 Lift Committers and Contributors
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
package actor

import org.specs2.mutable.Specification

import common._

/**
 * Validates Bug 3: LAFuture.collect over-allocates its accumulator
 * (`for (i <- 0 to len)` at LAFuture.scala:390 creates len+1 slots) and uses
 * `insert` rather than `update` at the success site, so collected values are
 * ordered by completion order rather than by input order.
 *
 * This example is expected to FAIL while the bug is present.
 */
class LatentCollectBugSpec extends Specification {
  "LAFuture.collect Specification".title

  // A scheduler that runs callbacks inline on the calling thread, so that the
  // order of satisfy() calls deterministically drives collect()'s accumulation.
  private val inline = new LAScheduler {
    def execute(f: () => Unit): Unit = f()
  }

  "LAFuture.collect" should {
    "preserve input order regardless of completion order" in {
      val f1 = new LAFuture[String](inline)
      val f2 = new LAFuture[String](inline)
      val f3 = new LAFuture[String](inline)

      val collected = LAFuture.collect(f1, f2, f3)

      // Complete out of order: middle, then first, then last.
      f2.satisfy("b")
      f1.satisfy("a")
      f3.satisfy("c")

      collected.get(5000L) must_== Full(List("a", "b", "c"))
    }
  }
}
