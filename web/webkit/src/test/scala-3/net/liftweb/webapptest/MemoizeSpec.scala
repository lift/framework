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
package webapptest

import org.specs2.mutable.Specification

import common._
import util._
import http._

object SessionInfo {
  lazy val session1 = new LiftSession("/", Helpers.randomString(20), Empty)
  lazy val session2 = new LiftSession("/", Helpers.randomString(20), Empty)

  object sessionMemo extends SessionMemoize[Int, Int]
  object requestMemo extends RequestMemoize[Int, Int]
}


/**
 * System under specification for Memoize.
 */
class MemoizeSpec extends Specification  {
  "Memoize Specification".title
  sequential

  import SessionInfo._

  "Memoize" should {
    "Session memo should default to empty" in {
      S.init(Full(Req.nil), session1) {
        sessionMemo.get(3) must be_== (Empty)
      }
    }

    "Session memo should be settable" in {
      S.init(Full(Req.nil), session1) {
        sessionMemo.get(3, 8) must be_== (8)

        sessionMemo.get(3) must be_== (Full(8))
      }
    }

    "Session memo should survive across calls" in {
      S.init(Full(Req.nil), session1) {
        sessionMemo.get(3) must be_== (Full(8))
      }
    }

    "Session memo should not float across sessions" in {
      S.init(Full(Req.nil), session2) {
        sessionMemo.get(3) must be_== (Empty)
      }
    }

    "Request memo should work in the same request" in {
      S.init(Full(Req.nil), session1) {
        requestMemo(3) must be_== (Empty)
        requestMemo(3, 44) must be_== (44)
        requestMemo(3) must be_== (Full(44))
      }
    }

    "Request memo should not span requests" in {
      S.init(Full(Req.nil), session1) {
        requestMemo(3) must be_== (Empty)
      }
    }

  }
}
