/*
 * Copyright 2011-2014 WorldWide Conferencing, LLC
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
package db

import org.specs2.mutable.Specification
import org.specs2.mock.Mockito
import org.mockito.Matchers._

import net.liftweb.common._
import net.liftweb.db._
import net.liftweb.util.DefaultConnectionIdentifier
import net.liftweb.util.ControlHelpers._

import java.sql._

class DBSpec extends Specification with Mockito {
  sequential

  trait CommitFunc {
    def f(success: Boolean): Unit
  }

  def  dBVendor(connection: Connection) = new ProtoDBVendor {
    def createOne = {
      connection.createStatement returns mock[PreparedStatement]
      Full(connection)
    }
  }

  "eager buildLoanWrapper" should {
    "call postTransaction functions with true if transaction is committed" in {
      val m = mock[CommitFunc]
      val activeConnection = mock[Connection]
      DB.defineConnectionManager(DefaultConnectionIdentifier, dBVendor(activeConnection))

      DB.buildLoanWrapper(true) {
        DB.appendPostTransaction(DefaultConnectionIdentifier, m.f _)
        DB.currentConnection.map{c => DB.exec(c, "stuff") {dummy => }}
      }
      there was one(activeConnection).commit
      there was one(m).f(true)
    }

    "call postTransaction functions with false if transaction is rolled back" in {
      val m = mock[CommitFunc]
      val activeConnection = mock[Connection]
      DB.defineConnectionManager(DefaultConnectionIdentifier, dBVendor(activeConnection))

      val lw = DB.buildLoanWrapper(true)

      tryo(lw.apply {
        DB.appendPostTransaction(DefaultConnectionIdentifier, m.f _)
        DB.currentConnection.map{c => DB.exec(c, "stuff") {dummy => }}
        throw new RuntimeException("oh no")
        42
      })
      there was one(activeConnection).rollback
      there was one(m).f(false)
    }
  }

  "lazy buildLoanWrapper" should {
    "call postTransaction functions with true if transaction is committed" in {
      val m = mock[CommitFunc]
      val activeConnection = mock[Connection]
      DB.defineConnectionManager(DefaultConnectionIdentifier, dBVendor(activeConnection))

      DB.buildLoanWrapper(false) {
        DB.use(DefaultConnectionIdentifier) {c =>
          DB.appendPostTransaction(DefaultConnectionIdentifier, m.f _)
          DB.exec(c, "stuff") {
            dummy =>
          }
        }
        DB.use(DefaultConnectionIdentifier) {c =>
          DB.exec(c, "more stuff") { dummy => }
        }
      }
      there was one(activeConnection).commit
      there was one(m).f(true)
    }

    "call postTransaction functions with false if transaction is rolled back" in {
      val m = mock[CommitFunc]
      val activeConnection = mock[Connection]
      DB.defineConnectionManager(DefaultConnectionIdentifier, dBVendor(activeConnection))

      val lw = DB.buildLoanWrapper(false)

      tryo(lw.apply {
        DB.use(DefaultConnectionIdentifier) {c =>
          DB.exec(c, "more stuff") { dummy => }
        }
        DB.use(DefaultConnectionIdentifier) {c =>
          DB.appendPostTransaction (m.f _)
          DB.exec(c, "stuff") {dummy => throw new RuntimeException("oh no")}
        }
        42
      })
      there was one(activeConnection).rollback
      there was one(m).f(false)
    }
  }

  "DB.use" should {
    "call postTransaction functions with true if transaction is committed" in {
      val m = mock[CommitFunc]
      val activeConnection = mock[Connection]
      DB.defineConnectionManager(DefaultConnectionIdentifier, dBVendor(activeConnection))

      DB.use(DefaultConnectionIdentifier) {c =>
        DB.appendPostTransaction(DefaultConnectionIdentifier, m.f _)
        DB.exec(c, "stuff") {dummy => }
      }

      there was one(activeConnection).commit
      there was one(m).f(true)
    }

    "call postTransaction functions with false if transaction is rolled back" in {
      val m = mock[CommitFunc]
      val activeConnection = mock[Connection]
      DB.defineConnectionManager(DefaultConnectionIdentifier, dBVendor(activeConnection))

      tryo(DB.use(DefaultConnectionIdentifier) {c =>
        DB.appendPostTransaction(DefaultConnectionIdentifier, m.f _)
        DB.exec(c, "stuff") {dummy => throw new RuntimeException("Oh no")}
        42
      })

      there was one(activeConnection).rollback
      there was one(m).f(false)
      success
    }
  }

  "appendPostTransaction" should {
    "throw if called outside tx context" in {
      DB.appendPostTransaction {committed => ()}  must throwA[IllegalStateException]
    }
  }

  "DB.rollback" should {
    "call postTransaction functions with false" in {
      val m = mock[CommitFunc]
      val activeConnection = mock[Connection]
      DB.defineConnectionManager(DefaultConnectionIdentifier, dBVendor(activeConnection))

      tryo(DB.use(DefaultConnectionIdentifier) {c =>
        DB.appendPostTransaction(DefaultConnectionIdentifier, m.f _)
        DB.rollback(DefaultConnectionIdentifier)
        42
      })

      there was one(activeConnection).rollback
      there was one(m).f(false)
    }
  }
}
