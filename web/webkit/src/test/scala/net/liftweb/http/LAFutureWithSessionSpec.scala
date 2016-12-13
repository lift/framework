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
      val future = LAFuture.build("something", futureSpecScheduler).withCurrentSession

      future.get(timeout) shouldEqual Failure("LiftSession not available in this thread context", Empty, Empty)
    }

    "succeed with the original value if in a session" in {
      val session = new LiftSession("Test Session", "", Empty)

      S.initIfUninitted(session) {
        val future = LAFuture.build("something", futureSpecScheduler).withCurrentSession

        future.get(timeout) shouldEqual Full("something")
      }
    }

    "have access to session variables in onComplete()" in {
      implicit val session = new LiftSession("Test Session", "", Empty)

      val future = LAFuture.build("onComplete", futureSpecScheduler).withImplicitSession
      future.onComplete {
        case Full(s) => TestVar1.set(s)
        case _ => fail("The future should have completed!")
      }

      S.initIfUninitted(session) { TestVar1.is must eventually(beEqualTo("onComplete")) }
    }

    "have access to session variables in onFail()" in {
      implicit val session = new LiftSession("Test Session", "", Empty)

      val future = new LAFuture(futureSpecScheduler).withImplicitSession
      future.onFail {
        case Failure(msg, Full(ex), Empty) => TestVar1.set(ex.getMessage)
        case _ => fail("The future should have failed!")
      }
      future.fail(new Exception("fail"))

      S.initIfUninitted(session) { TestVar1.is must eventually(beEqualTo("fail")) }
    }

    "have access to session variables in onSuccess()" in {
      implicit val session = new LiftSession("Test Session", "", Empty)

      val future = new LAFuture[String](futureSpecScheduler).withImplicitSession
      future.onSuccess(TestVar1.apply(_))
      future.satisfy("success!!")

      S.initIfUninitted(session) { TestVar1.is must eventually(beEqualTo("success!!")) }
    }

    "have access to session variables in chains of filter()" in {
      val session = new LiftSession("Test Session", "", Empty)

      S.initIfUninitted(session) {
        val future = LAFuture.build("something in it", futureSpecScheduler).withCurrentSession
        TestVar1("something")
        TestVar2("in it")

        val filtered = future
          .filter(_.contains(TestVar1.is))
          .filter(_.contains(TestVar2.is))

        filtered.get(timeout) must beEqualTo(Full("something in it"))
      }
    }

    "have access to session variables in chains of withFilter()" in {
      val session = new LiftSession("Test Session", "", Empty)

      S.initIfUninitted(session) {
        val future = LAFuture.build("something in it", futureSpecScheduler).withCurrentSession
        TestVar1("something")
        TestVar2("in it")

        val filtered = future
          .withFilter(_.contains(TestVar1.is))
          .withFilter(_.contains(TestVar2.is))

        filtered.get(timeout) must beEqualTo(Full("something in it"))
      }
    }

    "have access to session variables in chains of flatMap()" in {
      val session = new LiftSession("Test Session", "", Empty)

      S.initIfUninitted(session) {
        val future = LAFuture.build("this", futureSpecScheduler).withCurrentSession
        TestVar1(" and ")
        TestVar2("that")

        val fm1 = future.flatMap { s =>
          val f = new LAFuture[String](futureSpecScheduler)
          f.satisfy(s + TestVar1.is)
          f
        }
        val fm2 = fm1.flatMap { s =>
          val f = new LAFuture[String](futureSpecScheduler)
          f.satisfy(s + TestVar2.is)
          f
        }

        fm2.get(timeout) must beEqualTo(Full("this and that"))
      }
    }

    "have access to session variables in chains of map()" in {
      val session = new LiftSession("Test Session", "", Empty)

      S.initIfUninitted(session) {
        val future = LAFuture.build("this", futureSpecScheduler).withCurrentSession
        TestVar1(" and ")
        TestVar2("that")

        val mapped = future.map(_ + TestVar1.is).map(_ + TestVar2.is)

        mapped.get(timeout) must beEqualTo(Full("this and that"))
      }
    }

    "have access to session variables in foreach()" in {
      val session = new LiftSession("Test Session", "", Empty)

      S.initIfUninitted(session) {
        val future = LAFuture.build("stuff", futureSpecScheduler).withCurrentSession
        future.foreach(TestVar1.apply(_))

        TestVar1.is must eventually(beEqualTo("stuff"))
      }
    }

    "have the same scheduler as the original LAFuture when no session is available" in {
      val future = LAFuture.build("stuff", futureSpecScheduler).withCurrentSession

      future.scheduler must beEqualTo(futureSpecScheduler)
    }

    "have the same scheduler as the original LAFuture when a session is available" in {
      val session = new LiftSession("Test Session", "", Empty)

      S.initIfUninitted(session) {
        val future = LAFuture.build("stuff", futureSpecScheduler).withCurrentSession

        future.scheduler must beEqualTo(futureSpecScheduler)
      }
    }
  }
}