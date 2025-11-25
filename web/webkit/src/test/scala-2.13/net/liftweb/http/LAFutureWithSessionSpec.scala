package net.liftweb.http

import net.liftweb.actor.LAFuture
import net.liftweb.common.{Box, Empty, Failure, Full}
import net.liftweb.mockweb.WebSpec
import org.specs2.matcher.ThrownMessages

class LAFutureWithSessionSpec extends WebSpec with ThrownMessages {

  sequential

  object SessionVar1 extends SessionVar[String]("Uninitialized1")
  object SessionVar2 extends SessionVar[String]("Uninitialized2")

  object ReqVar1 extends RequestVar[String]("Uninitialized1")
  object ReqVar2 extends RequestVar[String]("Uninitialized2")

  val timeout = 10000L

  "LAFutureWithSession" should {

    "fail if session is not available" in {
      val future = LAFutureWithSession.withCurrentSession("kaboom")

      future.get(timeout) mustEqual Failure("LiftSession not available in this thread context", Empty, Empty)
    }

    "succeed with original value if session is available" withSFor "/" in {
      val future = LAFutureWithSession.withCurrentSession("works!")

      future.get(timeout) mustEqual Full("works!")
    }

    "have access to session variables in LAFuture task" withSFor "/" in {
      SessionVar1("dzien dobry")

      val future = LAFutureWithSession.withCurrentSession(SessionVar1.is)

      future.get(timeout) mustEqual Full("dzien dobry")
    }

    "have access to request variables in LAFuture task" withSFor "/" in {
      ReqVar1("guten tag")

      val future = LAFutureWithSession.withCurrentSession(ReqVar1.is)

      future.get(timeout) mustEqual Full("guten tag")
    }

    "have access to session variables in onComplete()" withSFor "/" in {
      // workaround for a possible race condition in AnyVarTrait
      // https://groups.google.com/forum/#!topic/liftweb/V1pWy14Wl3A
      SessionVar1.is

      val future = LAFutureWithSession.withCurrentSession {
        Thread.sleep(Long.MaxValue)
        "292 billion years"
      }

      future.onComplete {
        case Full(v) => SessionVar1(v)
        case problem => ko("Future computation failed: " + problem)
      }

      future.satisfy("thorgal")

      SessionVar1.is must eventually(beEqualTo("thorgal"))
    }

    "have access to request variables in onComplete()" withSFor "/" in {
      // workaround for a possible race condition in AnyVarTrait
      // https://groups.google.com/forum/#!topic/liftweb/V1pWy14Wl3A
      ReqVar1.is

      val future = LAFutureWithSession.withCurrentSession {
        Thread.sleep(Long.MaxValue)
        "292 billion years"
      }

      future.onComplete {
        case Full(v) => ReqVar1(v)
        case problem => ko("Future computation failed: " + problem)
      }

      future.satisfy("thor")

      ReqVar1.is must eventually(beEqualTo("thor"))
    }

    "have access to session variables in onFail()" withSFor "/" in {
      // workaround for a possible race condition in SessionVar
      // https://groups.google.com/forum/#!topic/liftweb/V1pWy14Wl3A
      SessionVar1.is

      val future = LAFutureWithSession.withCurrentSession {
        Thread.sleep(Long.MaxValue)
        "292 billion years"
      }

      future.onFail {
        case f: Failure => SessionVar1(f.msg)
        case _ => fail("The Future should have failed")
      }

      future.fail(new Exception("kaboom!"))

      SessionVar1.is must eventually(beEqualTo("kaboom!"))
    }

    "have access to request variables in onFail()" withSFor "/" in {
      // workaround for a possible race condition in AnyVarTrait
      // https://groups.google.com/forum/#!topic/liftweb/V1pWy14Wl3A
      ReqVar1.is

      val future = LAFutureWithSession.withCurrentSession {
        Thread.sleep(Long.MaxValue)
        "292 billion years"
      }

      future.onFail {
        case f: Failure => ReqVar1(f.msg)
        case _ => fail("The Future should have failed")
      }

      future.fail(new Exception("nope!"))

      ReqVar1.is must eventually(beEqualTo("nope!"))
    }

    "have access to session variables in onSuccess()" withSFor "/" in {
      // workaround for a possible race condition in AnyVarTrait
      // https://groups.google.com/forum/#!topic/liftweb/V1pWy14Wl3A
      SessionVar1.is

      val future = LAFutureWithSession.withCurrentSession {
        Thread.sleep(Long.MaxValue)
        "292 billion years"
      }

      future.onSuccess(SessionVar1(_))

      future.satisfy("done")

      SessionVar1.is must eventually(beEqualTo("done"))
    }

    "have access to request variables in onSuccess()" withSFor "/" in {
      // workaround for a possible race condition in AnyVarTrait
      // https://groups.google.com/forum/#!topic/liftweb/V1pWy14Wl3A
      ReqVar1.is

      val future = LAFutureWithSession.withCurrentSession {
        Thread.sleep(Long.MaxValue)
        "292 billion years"
      }

      future.onSuccess(ReqVar1(_))

      future.satisfy("my preciousss")

      ReqVar1.is must eventually(beEqualTo("my preciousss"))
    }

    "have access to session variables in chains of filter()" withSFor "/" in {
      SessionVar1("see")
      SessionVar2("me")

      val future = LAFutureWithSession.withCurrentSession("they see me rollin")
      val filtered = future
        .filter(_.contains(SessionVar1.is))
        .filter(_.contains(SessionVar2.is))

      filtered.get(timeout) must eventually(===(Full("they see me rollin"): Box[String]))
    }

    "have access to request variables in chains of filter()" withSFor "/" in {
      ReqVar1("see")
      ReqVar2("me")

      val future = LAFutureWithSession.withCurrentSession("they see me rollin")
      val filtered = future
        .filter(_.contains(ReqVar1.is))
        .filter(_.contains(ReqVar2.is))

      filtered.get(timeout) must eventually(===(Full("they see me rollin"): Box[String]))
    }

    "have access to session variables in chains of withFilter()" withSFor "/" in {
      SessionVar1("come")
      SessionVar2("prey")

      val future = LAFutureWithSession.withCurrentSession("do not come between the nazgul and his prey")
      val filtered = future
        .withFilter(_.contains(SessionVar1.is))
        .withFilter(_.contains(SessionVar2.is))

      filtered.get(timeout) must eventually(===(Full("do not come between the nazgul and his prey"): Box[String]))
    }

    "have access to request variables in chains of withFilter()" withSFor "/" in {
      ReqVar1("hurt")
      ReqVar2("precious")

      val future = LAFutureWithSession.withCurrentSession("mustn't go that way, mustn't hurt the precious!")
      val filtered = future
        .withFilter(_.contains(ReqVar1.is))
        .withFilter(_.contains(ReqVar2.is))

      filtered.get(timeout) must eventually(===(Full("mustn't go that way, mustn't hurt the precious!"): Box[String]))
    }

    "have access to session variables in chains of map()" withSFor "/" in {
      SessionVar1("b")
      SessionVar2("c")

      val future = LAFutureWithSession.withCurrentSession("a")
      val mapped = future.map(_ + SessionVar1.is).map(_ + SessionVar2.is)

      mapped.get(timeout) mustEqual Full("abc")
    }

    "have access to request variables in chains of map()" withSFor "/" in {
      ReqVar1("b")
      ReqVar2("c")

      val future = LAFutureWithSession.withCurrentSession("a")
      val mapped = future.map(_ + ReqVar1.is).map(_ + ReqVar2.is)

      mapped.get(timeout) mustEqual Full("abc")
    }

    "have access to session variables in chains of flatMap()" withSFor "/" in {
      SessionVar1("e")
      SessionVar2("f")

      val future = LAFutureWithSession.withCurrentSession("d")
      val mapped = future
        .flatMap { s =>
          val out = s + SessionVar1.is
          LAFuture.build(out)
        }
        .flatMap { s =>
          val out = s + SessionVar2.is
          LAFuture.build(out)
        }

      mapped.get(timeout) mustEqual Full("def")
    }

    "have access to request variables in chains of flatMap()" withSFor "/" in {
      ReqVar1("e")
      ReqVar2("f")

      val future = LAFutureWithSession.withCurrentSession("d")
      val mapped = future
        .flatMap { s =>
          val out = s + ReqVar1.is
          LAFuture.build(out)
        }
        .flatMap { s =>
          val out = s + ReqVar2.is
          LAFuture.build(out)
        }

      mapped.get(timeout) mustEqual Full("def")
    }

    "have access to session variables in foreach()" withSFor "/" in {
      // workaround for a possible race condition in AnyVarTrait
      // https://groups.google.com/forum/#!topic/liftweb/V1pWy14Wl3A
      SessionVar1.is

      val future = LAFutureWithSession.withCurrentSession("cookie")
      future.foreach(SessionVar1(_))

      SessionVar1.is must eventually(beEqualTo("cookie"))
    }

    "have access to request variables in foreach()" withSFor "/" in {
      // workaround for a possible race condition in AnyVarTrait
      // https://groups.google.com/forum/#!topic/liftweb/V1pWy14Wl3A
      ReqVar1.is

      val future = LAFutureWithSession.withCurrentSession("monster")
      future.foreach(ReqVar1(_))

      ReqVar1.is must eventually(beEqualTo("monster"))
    }

    "not leak out initial session between threads with their own sessions" in {
      val session1 = new LiftSession("Test session 1", "", Empty)
      val session2 = new LiftSession("Test session 2", "", Empty)
      val session3 = new LiftSession("Test session 3", "", Empty)

      S.initIfUninitted(session1)(SessionVar1("one"))
      S.initIfUninitted(session2)(SessionVar1("two"))
      S.initIfUninitted(session3)(SessionVar1("three"))

      val future = S.initIfUninitted(session1)(LAFutureWithSession.withCurrentSession("zero"))

      S.initIfUninitted(session2) {
        future.map(v => SessionVar1.is).get(timeout) must eventually(===(Full("two"): Box[String]))
      }

      S.initIfUninitted(session3) {
        future.map(v => SessionVar1.is).get(timeout) must eventually(===(Full("three"): Box[String]))
      }

      S.initIfUninitted(session1) {
        future.map(v => SessionVar1.is).get(timeout) must eventually(===(Full("one"): Box[String]))
      }
    }
  }
}
