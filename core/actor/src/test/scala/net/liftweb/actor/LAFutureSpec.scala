package net.liftweb.actor

import net.liftweb.common.{Box, Failure, Full}
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

    "when collecting results with LAFuture.collect" in {
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

      "report a failed LAFuture as a failure for the overall future" in {
        val one: LAFuture[Int] = new LAFuture
        val two: LAFuture[Int] = LAFuture(() => 5)

        one.fail(Failure("boom boom boom!"))

        val collectResult = LAFuture.collect(one, two)
        collectResult.get(timeout) shouldEqual Failure("boom boom boom!")
      }
    }

    "when collecting Boxed results with collectAll" in {
      "collectAll collects an EmptyBox immediately" in {
        val one: LAFuture[Box[Int]] = LAFuture(() => { Failure("whoops"): Box[Int] })
        val two: LAFuture[Box[Int]] = LAFuture(() => { Thread.sleep(10000); Full(1) })

        val collectResult = LAFuture.collectAll(one, two)
        collectResult.get(5000) shouldEqual Failure("whoops")
      }

      "collectAll collects a set of Fulls" in {
        val one: LAFuture[Box[Int]] = LAFuture(() => Full(1): Box[Int])
        val two: LAFuture[Box[Int]] = LAFuture(() => Full(2): Box[Int])
        val three: LAFuture[Box[Int]] = LAFuture(() => Full(3): Box[Int])

        val collectResult = LAFuture.collectAll(one, two, three)
        collectResult.get(timeout) should beLike {
          case Full(Full(results)) =>
            results should contain(allOf(1, 2, 3))
        }
      }

      "collectAll reports a failed LAFuture as a failure for the overall future" in {
        val one: LAFuture[Box[Int]] = new LAFuture
        val two: LAFuture[Box[Int]] = LAFuture(() => Full(5): Box[Int])

        one.fail(Failure("boom boom boom!"))

        val collectResult = LAFuture.collectAll(one, two)
        collectResult.get(timeout) shouldEqual Failure("boom boom boom!")
      }

      "collectAll empty list immediately" in {
        val collectResult = LAFuture.collectAll(Nil : _*)
        collectResult.isSatisfied shouldEqual true
        collectResult.get(timeout) shouldEqual Nil
      }

      "report a failed LAFuture as a failure for the overall future" in {
        val one: LAFuture[Box[Int]] = new LAFuture
        val two: LAFuture[Box[Int]] = LAFuture(() => Full(5): Box[Int])

        one.fail(Failure("boom boom boom!"))

        val collectResult = LAFuture.collectAll(one, two)
        collectResult.get(timeout) shouldEqual Failure("boom boom boom!")
      }
    }
  }

}
