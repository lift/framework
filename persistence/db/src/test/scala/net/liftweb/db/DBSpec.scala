package net.liftweb
package db

import org.specs.Specification
import org.specs.runner._
import org.specs.mock.Mockito
import org.mockito.Matchers._

import net.liftweb.common._
import net.liftweb.db._
import net.liftweb.util.ControlHelpers._

import java.sql._

class DBSpec extends Specification with Mockito {
  trait CommitFunc {
    def f(success: Boolean): Unit
  }

  var activeConnection: Connection = _

  def  dBVendor = new ProtoDBVendor {
    def createOne = {
      val connection = mock[Connection]
      connection.createStatement returns mock[PreparedStatement]
      activeConnection = connection
      Full(connection)
    }
  }

  DB.defineConnectionManager(DefaultConnectionIdentifier, dBVendor)

  "eager buildLoanWrapper" should {
    "call postTransaction functions with true if transaction is committed" in {
      val m = mock[CommitFunc]

      DB.buildLoanWrapper(true) {
        DB.appendPostTransaction(DefaultConnectionIdentifier, m.f _)
        DB.currentConnection.map{c => DB.exec(c, "stuff") {dummy => }}
      }
      there was one(activeConnection).commit
      there was one(m).f(true)
    }

    "call postCommit functions with false if transaction is rolledback" in {
      val m = mock[CommitFunc]

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

    "call postCommit functions with false if transaction is rolledback" in {
      val m = mock[CommitFunc]

      val lw = DB.buildLoanWrapper(false)

      tryo(lw.apply {
        DB.use(DefaultConnectionIdentifier) {c =>
          DB.exec(c, "more stuff") { dummy => }
        }
        DB.use(DefaultConnectionIdentifier) {c =>
          DB.appendPostTransaction(DefaultConnectionIdentifier, m.f _)
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

      DB.use(DefaultConnectionIdentifier) {c =>
        DB.appendPostTransaction(DefaultConnectionIdentifier, m.f _)
        DB.exec(c, "stuff") {dummy => }
      }

      there was one(activeConnection).commit
      there was one(m).f(true)
    }

    "call postTransaction functions with false if transaction is committed" in {
      val m = mock[CommitFunc]

      tryo(DB.use(DefaultConnectionIdentifier) {c =>
        DB.appendPostTransaction(DefaultConnectionIdentifier, m.f _)
        DB.exec(c, "stuff") {dummy => throw new RuntimeException("Oh no")}
        42
      })

      there was one(activeConnection).rollback
      there was one(m).f(false)
    }
  }

  "appendPostTransaction" should {
    "throw if called outside tx context" in {
      DB.appendPostTransaction(DefaultConnectionIdentifier, d => ())  must throwA[IllegalStateException]
    }
  }
}
