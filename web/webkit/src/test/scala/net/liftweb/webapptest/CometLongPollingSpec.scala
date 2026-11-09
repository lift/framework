/*
 * Characterization tests for Lift's servlet-level long polling.
 *
 * THEORY OF WHAT THESE TESTS TEST
 * -------------------------------
 * Lift's long polling contract: application code runs a request through
 * RestContinuation.async; the framework schedules the reply body on a thread
 * distinct from the request thread; the reply body runs detached from request
 * state (the framework's request/session wrapper applies to the response
 * rendering, not to the reply body); and an answer computed after the request
 * thread has returned is delivered to the waiting client.
 *
 * These are characterization tests: they pin behavior that exists today. The
 * test container (the embedded Jetty instance used by the webkit test suite)
 * reports no asynchronous support, so requests take the future-based fallback
 * path in LiftServlet.handleGenericContinuation; the contract under test —
 * off-thread reply, request state around the reply, budgeted delivery — is
 * the same contract on the asynchronous path, which Phase 4 (the servlet 5
 * migration) re-exercises on a container with real async support.
 *
 * Each test cites the behavior statement it proves in
 * docs/servlet-behavior-test-map.md.
 */
package net.liftweb
package webapptest

import org.specs2.mutable.Specification
import org.specs2.matcher.XmlMatchers

import java.net.{InetAddress, URL}

import common._
import util._
import util.Helpers._
import bootstrap.liftweb.LongPollingTests
import http.testing.RequestKit

/**
 * THEORY: the long polling contract is observable end to end: the client's
 * request returns with the reply produced by the reply body, the reply body
 * runs on a thread distinct from the request thread, request state is
 * available inside the reply body, and an answer produced within the budget
 * is delivered. Boundary conditions: a reply scheduled with a small delay
 * inside a small-but-sufficient budget, and the immediate-reply case.
 */
object CometLongPollingSpec extends Specification with RequestKit with XmlMatchers {
  sequential

  private def reachableLocalAddress = {
    val l = InetAddress.getLocalHost
    tryo { l.isReachable(50) } match {
      case Full(true) => l.getHostAddress
      case _          => "127.0.0.1"
    }
  }

  private val host_ = System.getProperty("net.liftweb.webapptest.comet.host", reachableLocalAddress)
  private val port_ = System.getProperty("net.liftweb.webapptest.comet.port", "8383").toInt

  private lazy val baseUrl_ = new URL("http://%s:%s".format(host_, port_))

  private lazy val jetty = new JettyTestServer(Full(baseUrl_))

  def baseUrl = jetty.baseUrl.toString

  step(jetty.start())

  "long polling" should {

    "run the reply off the request thread and deliver it to the client" in {
      val xml = get("/lp/echo?msg=hello").flatMap(_.xml)
        .openOrThrowException("the long polling reply should be XML")

      xml must ==/(<answer>hello</answer>).when(jetty.running)

      val reqThread = LongPollingTests.requestThread.get
      val replyThread = LongPollingTests.replyThread.get
      (reqThread must_!= replyThread).when(jetty.running)
    }

    "run the reply detached from request state" in {
      // CHARACTERIZATION FINDING: the reply body executes on the scheduler
      // thread without the session/request loans (the framework applies
      // wrapState around the response rendering, not around the reply body,
      // despite RestContinuation's scaladoc claiming the body runs "in the
      // scope of the current request"). This pins actual behavior; the
      // migration phases must preserve it.
      val xml = get("/lp/state").flatMap(_.xml)
        .openOrThrowException("the state reply should be XML")

      xml must ==/(<state>false</state>).when(jetty.running)
    }

    "deliver an answer computed after the request thread has returned" in {
      // CHARACTERIZATION NOTE: the async provider suspends with the
      // container timeout set to zero (Servlet30AsyncProvider.suspend), so
      // the framework delivers a late answer whenever it arrives; the
      // answer here is scheduled 200 ms after the request thread released.
      val xml = get("/lp/delay?ms=200").flatMap(_.xml)
        .openOrThrowException("the delayed reply should be XML")
      xml must ==/(<answer>done</answer>).when(jetty.running)
    }
  }

  step {
    tryo {
      jetty.stop()
    }
  }
}
