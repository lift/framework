package net.liftweb.http

import net.liftweb.common.Empty
import net.liftweb.mockweb.WebSpec
import org.specs2.matcher.ThrownMessages

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class FutureWithSessionSpec extends WebSpec with ThrownMessages {

  sequential

  object SessionVar1 extends SessionVar[String]("Uninitialized1")
  object SessionVar2 extends SessionVar[String]("Uninitialized2")

  object ReqVar1 extends RequestVar[String]("Uninitialized1")
  object ReqVar2 extends RequestVar[String]("Uninitialized2")

  "FutureWithSession" should {

    "fail if session is not available" in {
      val future = FutureWithSession.withCurrentSession("kaboom")

      future.value must eventually(beSome(beFailedTry[String].withThrowable[IllegalStateException](
        "LiftSession not available in this thread context"
      )))
    }

    "succeed with original value if session is available" withSFor "/" in {
      val future = FutureWithSession.withCurrentSession("works!")

      future.value must eventually(beEqualTo(Some(Success("works!"))))
    }

    "have access to session variables in Future task" withSFor "/" in {
      SessionVar1("dzien dobry")

      val future = FutureWithSession.withCurrentSession(SessionVar1.is)

      future.value must eventually(beEqualTo(Some(Success("dzien dobry"))))
    }

    "have access to request variables in Future task" withSFor "/" in {
      ReqVar1("guten tag")

      val future = FutureWithSession.withCurrentSession(ReqVar1.is)

      future.value must eventually(beEqualTo(Some(Success("guten tag"))))
    }

    "have access to session variables in onComplete()" withSFor "/" in {
      // workaround for a possible race condition in SessionVar
      // https://groups.google.com/forum/#!topic/liftweb/V1pWy14Wl3A
      SessionVar1.is

      val future = FutureWithSession.withCurrentSession("thorgal")
      future.onComplete {
        case Success(v) => SessionVar1(v)
        case Failure(reason) => ko("Future execution failed: " + reason)
      }

      SessionVar1.is must eventually(beEqualTo("thorgal"))
    }

    "have access to request variables in onComplete()" withSFor "/" in {
      // workaround for a possible race condition in AnyVarTrait
      // https://groups.google.com/forum/#!topic/liftweb/V1pWy14Wl3A
      ReqVar1.is

      val future = FutureWithSession.withCurrentSession("thor")
      future.onComplete {
        case Success(v) => ReqVar1(v)
        case Failure(reason) => ko("Future execution failed: " + reason)
      }

      ReqVar1.is must eventually(beEqualTo("thor"))
    }

    "have access to session variables in chains of map()" withSFor "/" in {
      SessionVar1("b")
      SessionVar2("c")

      val future = FutureWithSession.withCurrentSession("a")
      val mapped = future.map(_ + SessionVar1.is).map(_ + SessionVar2.is)

      mapped.value must eventually(beEqualTo(Some(Success("abc"))))
    }

    "have access to request variables in chains of map()" withSFor "/" in {
      ReqVar1("b")
      ReqVar2("c")

      val future = FutureWithSession.withCurrentSession("a")
      val mapped = future.map(_ + ReqVar1.is).map(_ + ReqVar2.is)

      mapped.value must eventually(beEqualTo(Some(Success("abc"))))
    }

    "have access to session variables in chains of flatMap()" withSFor "/" in {
      SessionVar1("e")
      SessionVar2("f")

      val future = FutureWithSession.withCurrentSession("d")
      val mapped = future
        .flatMap { s => val out = s + SessionVar1.is; Future(out) }
        .flatMap { s => val out = s + SessionVar2.is; Future(out) }

      mapped.value must eventually(beEqualTo(Some(Success("def"))))
    }

    "have access to request variables in chains of flatMap()" withSFor "/" in {
      ReqVar1("e")
      ReqVar2("f")

      val future = FutureWithSession.withCurrentSession("d")
      val mapped = future
        .flatMap { s => val out = s + ReqVar1.is; Future(out) }
        .flatMap { s => val out = s + ReqVar2.is; Future(out) }

      mapped.value must eventually(beEqualTo(Some(Success("def"))))
    }

    "have access to session variables in chains of andThen()" withSFor "/" in {
      // workaround for a possible race condition in AnyVarTrait
      // https://groups.google.com/forum/#!topic/liftweb/V1pWy14Wl3A
      SessionVar1.is
      SessionVar2.is

      val future = FutureWithSession.withCurrentSession("rambo")
        .andThen { case Success(v) => SessionVar1(v) }
        .andThen { case Success(v) => SessionVar2(v) }

      SessionVar1.is must eventually(beEqualTo("rambo"))
      SessionVar2.is must eventually(beEqualTo("rambo"))
      future.value must eventually(beEqualTo(Some(Success("rambo"))))
    }

    "have access to request variables in chains of andThen()" withSFor "/" in {
      // workaround for a possible race condition in AnyVarTrait
      // https://groups.google.com/forum/#!topic/liftweb/V1pWy14Wl3A
      ReqVar1.is
      ReqVar2.is

      val future = FutureWithSession.withCurrentSession("conan")
        .andThen { case Success(v) => ReqVar1(v) }
        .andThen { case Success(v) => ReqVar2(v) }

      ReqVar1.is must eventually(beEqualTo("conan"))
      ReqVar2.is must eventually(beEqualTo("conan"))
      future.value must eventually(beEqualTo(Some(Success("conan"))))
    }

    "have access to session variables in failed projection" withSFor "/" in {
      SessionVar1("on purpose")

      val future = FutureWithSession.withCurrentSession(throw new Exception("failed")).failed.collect {
        case e: Exception => e.getMessage + " " + SessionVar1.is
      }

      future.value must eventually(beEqualTo(Some(Success("failed on purpose"))))
    }

    "have access to request variables in failed projection" withSFor "/" in {
      ReqVar1("on purpose")

      val future = FutureWithSession.withCurrentSession(throw new Exception("failed")).failed.collect {
        case e: Exception => e.getMessage + " " + ReqVar1.is
      }

      future.value must eventually(beEqualTo(Some(Success("failed on purpose"))))
    }

    "have access to session variables in fallbackTo() result" withSFor "/" in {
      SessionVar1("result")

      val future = FutureWithSession.withCurrentSession(throw new Exception("failed"))
        .fallbackTo(Future("fallback")).map(_ + " " + SessionVar1.is)

      future.value must eventually(beEqualTo(Some(Success("fallback result"))))
    }

    "have access to request variables in fallbackTo() result" withSFor "/" in {
      ReqVar1("result")

      val future = FutureWithSession.withCurrentSession(throw new Exception("failed"))
        .fallbackTo(Future("fallback")).map(_ + " " + ReqVar1.is)

      future.value must eventually(beEqualTo(Some(Success("fallback result"))))
    }

    "have access to session variables with recover()" withSFor "/" in {
      SessionVar1("g")
      SessionVar2("h")

      val future = FutureWithSession.withCurrentSession(throw new Exception("failed"))
        .recover { case e: Exception => e.getMessage + " " + SessionVar1.is }
        .map(_ + SessionVar2.is)

      future.value must eventually(beEqualTo(Some(Success("failed gh"))))
    }

    "have access to request variables with recover()" withSFor "/" in {
      ReqVar1("g")
      ReqVar2("h")

      val future = FutureWithSession.withCurrentSession(throw new Exception("failed"))
        .recover { case e: Exception => e.getMessage + " " + ReqVar1.is }
        .map(_ + ReqVar2.is)

      future.value must eventually(beEqualTo(Some(Success("failed gh"))))
    }

    "have access to session variables with recoverWith()" withSFor "/" in {
      SessionVar1("i")
      SessionVar2("j")

      val future = FutureWithSession.withCurrentSession(throw new Exception("failed"))
        .recoverWith { case e: Exception => val out = e.getMessage + " " + SessionVar1.is; Future(out) }
        .map(_ + SessionVar2.is)

      future.value must eventually(beEqualTo(Some(Success("failed ij"))))
    }

    "have access to request variables with recoverWith()" withSFor "/" in {
      ReqVar1("k")
      ReqVar2("l")

      val future = FutureWithSession.withCurrentSession(throw new Exception("failed"))
        .recoverWith { case e: Exception => val out = e.getMessage + " " + ReqVar1.is; Future(out) }
        .map(_ + ReqVar2.is)

      future.value must eventually(beEqualTo(Some(Success("failed kl"))))
    }

    "have access to session variables with transform()" withSFor "/" in {
      SessionVar1("john")
      SessionVar2("rambo")

      val future = FutureWithSession.withCurrentSession("something")
        .transform(s => throw new Exception(SessionVar1.is), identity[Throwable])
        .transform(identity[String], t => new Exception(t.getMessage + " " + SessionVar2.is))
        .recover { case e: Exception => e.getMessage }

      future.value must eventually(beEqualTo(Some(Success("john rambo"))))
    }

    "have access to request variables with transform()" withSFor "/" in {
      ReqVar1("chuck")
      ReqVar2("norris")

      val future = FutureWithSession.withCurrentSession("something")
        .transform(s => throw new Exception(ReqVar1.is), identity[Throwable])
        .transform(identity[String], t => new Exception(t.getMessage + " " + ReqVar2.is))
        .recover { case e: Exception => e.getMessage }

      future.value must eventually(beEqualTo(Some(Success("chuck norris"))))
    }

    "yield another session aware future with zip()" withSFor "/" in {
      ReqVar1("a")
      SessionVar1("hero")

      val future = FutureWithSession.withCurrentSession("gotham")
        .zip(Future("needs"))
        .collect { case (one, two) => one + two + ReqVar1.is + SessionVar1.is }

      future.value must eventually(beEqualTo(Some(Success("gothamneedsahero"))))
    }

    "not leak out initial session between threads with their own sessions" in {
      val session1 = new LiftSession("Test session 1", "", Empty)
      val session2 = new LiftSession("Test session 2", "", Empty)
      val session3 = new LiftSession("Test session 3", "", Empty)

      S.initIfUninitted(session1)(SessionVar1("one"))
      S.initIfUninitted(session2)(SessionVar1("two"))
      S.initIfUninitted(session3)(SessionVar1("three"))

      val future = S.initIfUninitted(session1)(FutureWithSession.withCurrentSession("zero"))

      S.initIfUninitted(session2) {
        val mapped = future.map(v => SessionVar1.is)
        mapped.value must eventually(beEqualTo(Some(Success("two"))))
      }

      S.initIfUninitted(session3) {
        val mapped = future.map(v => SessionVar1.is)
        mapped.value must eventually(beEqualTo(Some(Success("three"))))
      }

      S.initIfUninitted(session1) {
        val mapped = future.map(v => SessionVar1.is)
        mapped.value must eventually(beEqualTo(Some(Success("one"))))
      }
    }
  }
}
