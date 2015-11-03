package net.liftweb.http

import net.liftweb.common.Empty
import scala.concurrent._
import org.specs2.mutable.Specification
import FutureWithSession._

import scala.util.{Try, Success}

object FutureWithSessionSpec extends Specification {

  object TestVar1 extends SessionVar[String]("test1")
  object TestVar2 extends SessionVar[String]("test2")

  "A FutureWithSession" should {
    "fail if session is not available" in {
      val actual:Future[String] = Future("something").withSession

      // TODO: Why the hell does this not work??
//      val expected:Option[Try[String]] = Some(Failure(new Exception))
//      actual.value must eventually(beEqualTo(expected))

      actual.value.isDefined must eventually(beTrue)
      actual.value.get must beFailedTry.withThrowable[Exception]("LiftSession not available in this thread context")
    }

    "succeed with the original value if in a session" in {
      val session = new LiftSession("Test Session", "", Empty)

      S.initIfUninitted(session) {
        val actual:Future[String] = Future("something").withSession
        val expected:Option[Try[String]] = Some(Success("something"))

        actual.value must eventually(beEqualTo(expected))
      }
    }

    "have access to session variables in map() chains" in {
      val session = new LiftSession("Test Session", "", Empty)

      val actual:Future[String] = S.initIfUninitted(session) {
        TestVar1("map1")
        TestVar2("map2")
        Future("something").withSession
          .map(_+"-"+TestVar1.get)
          .map(_+"-"+TestVar2.get)
      }
      val expected:Option[Try[String]] = Some(Success("something-map1-map2"))

      actual.value must eventually(beEqualTo(expected))
    }

    "have access to session variables in flatMap() chains" in {
      val session = new LiftSession("Test Session", "", Empty)

      val actual:Future[String] = S.initIfUninitted(session) {
        TestVar1("map1")
        TestVar2("map2")
        Future("something").withSession
          .flatMap{in => val out = in+"-"+TestVar1.get; Future(out)}
          .flatMap{in => val out = in+"-"+TestVar2.get; Future(out)}
      }
      val expected:Option[Try[String]] = Some(Success("something-map1-map2"))

      actual.value must eventually(beEqualTo(expected))
    }
  }
}
