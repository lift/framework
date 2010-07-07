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
package webapptest {

import _root_.org.specs._
import _root_.org.specs.runner.JUnit3
import _root_.org.specs.runner.ConsoleRunner
import _root_.net.sourceforge.jwebunit.junit.WebTester
import _root_.net.liftweb._
import http._
import common._
import _root_.net.liftweb.util._

class MemoizeTest extends JUnit3(MemoizeSpec)
object MemoizeRunner extends ConsoleRunner(MemoizeSpec)

object SessionInfo {
  lazy val session1 = new LiftSession("/", Helpers.randomString(20), Empty)
  lazy val session2 = new LiftSession("/", Helpers.randomString(20), Empty)

  object sessionMemo extends SessionMemoize[Int, Int]
  object requestMemo extends RequestMemoize[Int, Int]
}

object MemoizeSpec extends Specification {
  import SessionInfo._


  "Memoize" should {
    "Session memo should default to empty" >> {
      S.initIfUninitted(session1) {
        sessionMemo.get(3) must_== Empty
      }
    }

    "Session memo should be settable" >> {
      S.initIfUninitted(session1) {
        sessionMemo.get(3, 8) must_== 8

        sessionMemo.get(3) must_== Full(8)
      }
    }

    "Session memo should survive across calls" >> {
      S.initIfUninitted(session1) {
        sessionMemo.get(3) must_== Full(8)
      }
    }

    "Session memo should not float across sessions" >> {
      S.initIfUninitted(session2) {
        sessionMemo.get(3) must_== Empty
      }
    }

    "Request memo should work in the same request" >> {
      S.initIfUninitted(session1) {
        requestMemo(3) must_== Empty
        requestMemo(3, 44) must_== Full(44)
        requestMemo(3) must_== Full(44)
      }
    }

    "Request memo should not span requests" >> {
      S.initIfUninitted(session1) {
        requestMemo(3) must_== Empty
      }
    }

  }
}

}
}
