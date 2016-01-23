package net.liftweb.http

import net.liftweb.actor.{LAScheduler, LAFuture}
import net.liftweb.common.{ Failure, Full, Empty }
import org.specs2.matcher.ThrownMessages
import org.specs2.mutable.Specification

import LAFutureWithSession._

class LAFutureWithSessionSpec extends Specification with ThrownMessages {
  sequential
  val timeout = 20000L
  LAScheduler

  object TestVar1 extends SessionVar[String]("Uninitialized1")
  object TestVar2 extends SessionVar[String]("Uninitialized2")

  "An LAFutureWithSession" should {
    val futureSpecScheduler = new LAScheduler {
      override def execute(f: () => Unit): Unit = f()
    }

    "fail if session is not available" in {
      val future = LAFuture(() => "something", futureSpecScheduler).withCurrentSession

      future.get(timeout) shouldEqual Failure("LiftSession not available in this thread context", Empty, Empty)
    }

    "succeed with the original value if in a session" in {
      val session = new LiftSession("Test Session", "", Empty)

      S.initIfUninitted(session) {
        val future = LAFuture(() => "something", futureSpecScheduler).withCurrentSession

        future.get(timeout) shouldEqual Full("something")
      }
    }

    "have access to session variables in onComplete()" in {
      implicit val session = new LiftSession("Test Session", "", Empty)

      val future = LAFuture(() => "onComplete", futureSpecScheduler).withImplicitSession
      future.onComplete {
        case Full(s) => TestVar1.set(s)
        case _ => fail("The future should have completed!")
      }

      S.initIfUninitted(session) { TestVar1.get must eventually(beEqualTo("onComplete")) }
    }

  }
}