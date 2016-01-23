package net.liftweb.http

import net.liftweb.actor.LAFuture
import net.liftweb.common.{ Failure, Full, Empty }
import org.specs2.matcher.ThrownMessages
import org.specs2.mutable.Specification

import LAFutureWithSession._

object LAFutureWithSessionSpec extends Specification with ThrownMessages {
  object TestVar1 extends SessionVar[String]("Uninitialized1")
  object TestVar2 extends SessionVar[String]("Uninitialized2")

  "An LAFutureWithSession" should {
    "fail if session is not available" in {
      val actual: LAFuture[String] = new LAFuture[String]().withCurrentSession

      actual.get(0) must beEqualTo(Failure("LiftSession not available in this thread context", Empty, Empty))
    }
  }
}
