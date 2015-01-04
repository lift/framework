package net.liftweb.actor

import org.specs2.mutable.Specification

class LAFutureSpec extends Specification {
  val timeout = 1000L

  "LAFuture" should {

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
      LAFuture.collect(one, two).get shouldEqual List(givenOneResult, givenTwoResult)
    }

    "collect empty list immediately" in {
      val collectResult = LAFuture.collect(Nil: _*)
      collectResult.isSatisfied shouldEqual true
      collectResult.get shouldEqual Nil
    }

    "collectAll empty list immediately" in {
      val collectResult = LAFuture.collectAll(Nil : _*)
      collectResult.isSatisfied shouldEqual true
      collectResult.get shouldEqual Nil
    }
  }

}
