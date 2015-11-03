package net.liftweb.http

import net.liftweb.common.Empty
import scala.concurrent._
import org.specs2.mutable.Specification
import FutureWithSession._

import scala.util.{Try, Success}

object FutureWithSessionSpec extends Specification {

  object TestVar extends SessionVar[String]("test")

  "A FutureWithSession" should {
    "fail if session is not available" in {
      val actual = Future("something").withSession

      // TODO: Why the hell does this not work??
//      val expected:Option[Try[String]] = Some(Failure(new Exception))
//      actual.value must eventually(beEqualTo(expected))

      actual.value.isDefined must eventually(beTrue)
      actual.value.get must beFailedTry.withThrowable[Exception]("LiftSession not available in this thread context")
    }

    "succeed with the original value if in a session" in {
      val session = new LiftSession("Test Session", "", Empty)

      S.initIfUninitted(session) {
        val actual = Future("something").withSession
        val expected:Option[Try[String]] = Some(Success("something"))

        actual.value must eventually(beEqualTo(expected))
      }
    }
  }
}
