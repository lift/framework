package net.liftweb.actor

import net.liftweb.common.{Failure, Box}
import org.specs2.mutable.Specification
import java.util.concurrent.atomic.AtomicBoolean

class LAFutureSpec extends Specification {
  sequential
  val timeout = 20000L
  LAScheduler

  "LAFuture" should {
    val futureSpecScheduler = new LAScheduler {
      override def execute(f: ()=>Unit): Unit = f()
    }

    "map to failing future if transforming function throws an Exception" in {
      val future = LAFuture(() => 1, futureSpecScheduler)
      def tranformThrowingException(input: Int) = {
        throw new Exception("fail")
      }

      val transformedFuture = future.map(tranformThrowingException)

      var notifiedAboutFailure: Boolean = false
      transformedFuture.onFail { _ =>
        notifiedAboutFailure = true
      }

      transformedFuture.get(timeout)
      notifiedAboutFailure shouldEqual true
    }

    "flatMap to failing future if transforming function throws an Exception" in {
      val future = LAFuture(() => 1, futureSpecScheduler)
      def tranformThrowingException(input: Int): LAFuture[Int] = {
        throw new Exception("fail")
      }

      val transformedFuture = future.flatMap(tranformThrowingException)

      var notifiedAboutFailure: Boolean = false
      transformedFuture.onFail { _ =>
        notifiedAboutFailure = true
      }

      transformedFuture.get(timeout)
      notifiedAboutFailure shouldEqual true
    }

    "return original Failure after timeout" in {
      val future = new LAFuture()
      val givenFailure = Failure("fooFailure")
      LAScheduler.execute { () =>
        Thread.sleep(500)
        future.fail(givenFailure)
      }

      val result = future.get(timeout)

      result shouldEqual givenFailure
    }

    "collect one future result" in {
      val givenOneResult = 123
      val one = LAFuture(() => givenOneResult)
      LAFuture.collect(one).get(timeout) shouldEqual List(givenOneResult)
    }

    "collect more future results in correct order" in {
      val givenOneResult = 123
      val givenTwoResult = 234
      val one = LAFuture(() => givenOneResult)
      val two = LAFuture(() => givenTwoResult)
      LAFuture.collect(one, two).get(timeout) shouldEqual List(givenOneResult, givenTwoResult)
    }

    "collect empty list immediately" in {
      val collectResult = LAFuture.collect(Nil: _*)
      collectResult.isSatisfied shouldEqual true
      collectResult.get(timeout) shouldEqual Nil
    }

    "collectAll empty list immediately" in {
      val collectResult = LAFuture.collectAll(Nil : _*)
      collectResult.isSatisfied shouldEqual true
      collectResult.get(timeout) shouldEqual Nil
    }
  }

}
