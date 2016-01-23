package net.liftweb.http

import net.liftweb.common.Empty
import org.specs2.matcher.ThrownMessages
import scala.concurrent._
import org.specs2.mutable.Specification
import FutureWithSession._

import scala.util.{Try, Success}

object FutureWithSessionSpec extends Specification with ThrownMessages {

  object TestVar1 extends SessionVar[String]("Uninitialized1")
  object TestVar2 extends SessionVar[String]("Uninitialized2")

  "A FutureWithSession" should {
    "fail if session is not available" in {
      val actual:Future[String] = Future("something").withCurrentSession

      actual.value must eventually(beLike (PartialFunction[Option[Try[String]],org.specs2.matcher.MatchResult[_]]{
        case Some(result) =>
          result must beFailedTry.withThrowable[Exception]("LiftSession not available in this thread context")
        case None => fail("actual.value is a None but should be Some(...)")
      }))
    }

    "succeed with the original value if in a session" in {
      val session = new LiftSession("Test Session", "", Empty)

      S.initIfUninitted(session) {
        val actual:Future[String] = Future("something").withCurrentSession
        val expected:Option[Try[String]] = Some(Success("something"))

        actual.value must eventually(beEqualTo(expected))
      }
    }

    "have access to session variables in onComplete()" in {
      implicit val session = new LiftSession("Test Session", "", Empty)

      val future:Future[String] = Future("onComplete").withImplicitSession
      future.onComplete { 
        case Success(s) => TestVar1.set(s) 
        case _ => // Just to avoid the compiler warning
      }

      S.initIfUninitted(session) { TestVar1.get must eventually(beEqualTo("onComplete")) }
    }

    "have access to session variables in andThen() chains" in {
      val session = new LiftSession("Test Session", "", Empty)

      val future:Future[String] = S.initIfUninitted(session) {
        TestVar1("val1")
        TestVar2("val2")
        Future("new value").withCurrentSession
      }
      val actual:Future[String] = future
        .andThen { case Success(str) => TestVar1(str) }
        .andThen { case Success(str) => TestVar2(str) }

      val expected:Option[Try[String]] = Some(Success("new value"))

      S.initIfUninitted(session) {
        TestVar1.get must eventually(beEqualTo("new value"))
        TestVar2.get must eventually(beEqualTo("new value"))
        actual.value must eventually(beEqualTo(expected))
      }
    }

    "have access to session variables in collect() chains" in {
      val session = new LiftSession("Test Session", "", Empty)

      val future:Future[String] = S.initIfUninitted(session) {
        TestVar1("collect1")
        TestVar2("collect2")
        Future("something").withCurrentSession
      }
      val actual:Future[String] = future
        .collect { case s:String => s+"-"+TestVar1.get }
        .collect { case s:String => s+"-"+TestVar2.get }
      val expected:Option[Try[String]] = Some(Success("something-collect1-collect2"))

      actual.value must eventually(beEqualTo(expected))
    }

    "have access to session variables in failed projection" in {
      val session = new LiftSession("Test Session", "", Empty)

      val future:Future[String] = S.initIfUninitted(session) {
        TestVar1("fail")
        Future.failed[String](new Exception("failure")).withCurrentSession
      }
      val actual:Future[String] = future.failed
        .collect { case e:Exception => e.getMessage+"-"+TestVar1.get }
      val expected:Option[Try[String]] = Some(Success("failure-fail"))

      actual.value must eventually(beEqualTo(expected))
    }

    "have access to session variables after calling fallbackTo()" in {
      val session = new LiftSession("Test Session", "", Empty)

      val future:Future[String] = S.initIfUninitted(session) {
        TestVar1("fallbackTo")
        Future.failed[String](new Exception("failure")).withCurrentSession
      }
      val actual:Future[String] = future.fallbackTo(Future("success"))
        .map(_+"-"+TestVar1.get)
      val expected:Option[Try[String]] = Some(Success("success-fallbackTo"))

      actual.value must eventually(beEqualTo(expected))
    }

    "have access to session variables in filter() chains" in {
      val session = new LiftSession("Test Session", "", Empty)

      val future:Future[String] = S.initIfUninitted(session) {
        TestVar1("map1")
        TestVar2("map")
        Future("has map1").withCurrentSession
      }
      val actual:Future[String] = future
        .filter(_.contains(TestVar1.get))
        .filter(_.contains(TestVar2.get))

      val expected:Option[Try[String]] = Some(Success("has map1"))

      actual.value must eventually(beEqualTo(expected))
    }

    "have access to session variables in flatMap() chains" in {
      val session = new LiftSession("Test Session", "", Empty)

      val future:Future[String] = S.initIfUninitted(session) {
        TestVar1("map1")
        TestVar2("map2")
        Future("something").withCurrentSession
      }
      val actual:Future[String] = future
        .flatMap{in => val out = in+"-"+TestVar1.get; Future(out)}
        .flatMap{in => val out = in+"-"+TestVar2.get; Future(out)}
      val expected:Option[Try[String]] = Some(Success("something-map1-map2"))

      actual.value must eventually(beEqualTo(expected))
    }

    "have access to session variables in foreach()" in {
      implicit val session = new LiftSession("Test Session", "", Empty)

      val future:Future[String] = Future("foreach").withImplicitSession
      future.foreach(TestVar1.set)

      S.initIfUninitted(session) { TestVar1.get } must eventually(beEqualTo("foreach"))
    }

    "have access to session variables in map() chains" in {
      val session = new LiftSession("Test Session", "", Empty)

      val future:Future[String] = S.initIfUninitted(session) {
        TestVar1("map1")
        TestVar2("map2")
        Future("something").withCurrentSession
      }
      val actual:Future[String] = future
        .map(_+"-"+TestVar1.get)
        .map(_+"-"+TestVar2.get)
      val expected:Option[Try[String]] = Some(Success("something-map1-map2"))

      actual.value must eventually(beEqualTo(expected))
    }

    "yield another FutureWithSession with mapTo()" in {
      val session = new LiftSession("Test Session", "", Empty)

      val future:Future[Object] = S.initIfUninitted(session) {
        TestVar1("mapTo")
        Future("something").withCurrentSession
      }
      val actual:Future[String] = future
        .mapTo[String]
        .map(_+"-"+TestVar1.get)
      val expected:Option[Try[String]] = Some(Success("something-mapTo"))

      actual.value must eventually(beEqualTo(expected))
    }

    "have access to session variables in onFailure()" in {
      implicit val session = new LiftSession("Test Session", "", Empty)

      val future:Future[String] = Future.failed[String](new Exception("failure")).withImplicitSession
      future.onFailure { case e:Exception => TestVar1.set(e.getMessage) }

      S.initIfUninitted(session) { TestVar1.get } must eventually(beEqualTo("failure"))
    }

    "have access to session variables in onSuccess()" in {
      implicit val session = new LiftSession("Test Session", "", Empty)

      val future:Future[String] = Future("onSuccess").withImplicitSession
      future.onSuccess { case s => TestVar1.set(s) }

      S.initIfUninitted(session) { TestVar1.get } must eventually(beEqualTo("onSuccess"))
    }

    "have access to session variables with recover()" in {
      val session = new LiftSession("Test Session", "", Empty)

      val future:Future[String] = S.initIfUninitted(session) {
        TestVar1("recover1")
        TestVar2("recover2")
        Future.failed[String](new Exception("failure")).withCurrentSession
      }
      val actual:Future[String] = future
        .recover { case e:Throwable => e.getMessage+"-"+TestVar1.get }
        .map(_+"-"+TestVar2.get)
      val expected:Option[Try[String]] = Some(Success("failure-recover1-recover2"))

      actual.value must eventually(beEqualTo(expected))
    }

    "have access to session variables with recoverWith()" in {
      val session = new LiftSession("Test Session", "", Empty)

      val future:Future[String] = S.initIfUninitted(session) {
        TestVar1("recover1")
        TestVar2("recover2")
        Future.failed[String](new Exception("failure")).withCurrentSession
      }
      val actual:Future[String] = future
        .recoverWith { case e:Throwable => val out = e.getMessage+"-"+TestVar1.get; Future(out) }
        .map(_+"-"+TestVar2.get)
      val expected:Option[Try[String]] = Some(Success("failure-recover1-recover2"))

      actual.value must eventually(beEqualTo(expected))
    }

    "have access to session variables with transform()" in {
      val session = new LiftSession("Test Session", "", Empty)

      val future:Future[String] = S.initIfUninitted(session) {
        TestVar1("transform1")
        TestVar2("transform2")
        Future("orig").withCurrentSession
      }
      val actual:Future[String] = future
        .transform( s => throw new Exception(TestVar1.get), identity[Throwable] )
        .transform( identity[String], t => new Exception(t.getMessage+"-"+TestVar2.get) )
        .recover { case e:Exception => e.getMessage }

      val expected:Option[Try[String]] = Some(Success("transform1-transform2"))

      actual.value must eventually(beEqualTo(expected))
    }

    "yield another FutureWithSession with zip()" in {
      val session = new LiftSession("Test Session", "", Empty)

      val future:Future[String] = S.initIfUninitted(session) {
        TestVar1("zip")
        Future("one").withCurrentSession
      }
      val actual:Future[String] = future
        .zip(Future("two"))
        .collect { case (one, two) => one+"-"+TestVar1.get+"-"+two }

      val expected:Option[Try[String]] = Some(Success("one-zip-two"))

      actual.value must eventually(beEqualTo(expected))
    }
  }
}
