/*
 * Characterization tests for the async suspension failsafe.
 *
 * THEORY OF WHAT THESE TESTS TEST
 * -------------------------------
 * Per-request suspension support is a per-request property: the provider
 * meta's class-level support is necessary but insufficient — a container
 * request that reports asynchronous processing unsupported makes
 * HTTPRequestServlet.suspendResumeSupport_? false, which selects the bounded
 * fallback path in LiftServlet rather than suspension.
 *
 * Budget characterization (pinned here end-to-end): on an async-capable
 * container, an answer computed beyond the configured budget
 * (LiftRules.cometRequestTimeout) is still delivered, and the client waits
 * for it. The code sets the container async timeout to zero on suspend, and
 * (verified during this phase's mutation checks) honoring that timeout does
 * not change the delivery: the container's timeout dispatch re-enters the
 * framework, which re-suspends the request until the answer arrives. The
 * budget expiry fallback (request completes without a Lift response) applies
 * on containers without async support; this container is async-capable, so
 * that boundary is characterized at the unit level by the per-request test
 * above.
 *
 * These tests cite the behavior statements they prove in
 * docs/servlet-behavior-test-map.md.
 */
package net.liftweb
package webapptest

import org.specs2.mutable.Specification
import org.specs2.matcher.XmlMatchers

import java.net.{InetAddress, URL}

import common._
import http._
import util.Helpers._
import mocks.{MockHttpServletRequest, MockServletContext}
import http.testing.RequestKit

object AsyncProviderFailsafeSpec extends Specification with RequestKit with XmlMatchers {
  sequential

  private def reachableLocalAddress = {
    val l = InetAddress.getLocalHost
    tryo { l.isReachable(50) } match {
      case Full(true) => l.getHostAddress
      case _          => "127.0.0.1"
    }
  }

  private val host_ = System.getProperty("net.liftweb.webapptest.failsafe.host", reachableLocalAddress)
  private val port_ = System.getProperty("net.liftweb.webapptest.failsafe.port", "8686").toInt

  private lazy val baseUrl_ = new URL("http://%s:%s".format(host_, port_))

  private lazy val jetty = new JettyTestServer(Full(baseUrl_))

  def baseUrl = jetty.baseUrl.toString

  step(jetty.start())

  "async suspension failsafe" should {

    "report no per-request support when the container request disallows async" in {
      val raw = new MockHttpServletRequest
      // MockHttpServletRequest reports isAsyncSupported() = false
      val adapter = new http.provider.servlet.HTTPRequestServlet(
        raw,
        new http.provider.HTTPProvider {
          def context: http.provider.HTTPContext =
            new http.provider.servlet.HTTPServletContext(new MockServletContext("target/test1-1.0-SNAPSHOT"))
        })

      adapter.suspendResumeSupport_? must beFalse
    }

    // CHARACTERIZATION FINDING: a budget-exceeded answer is still delivered
    // on an async-capable container and the client waits for it (verified in
    // the mutation checks: honoring the container timeout changes nothing —
    // the timeout dispatch re-suspends until the answer arrives).
    // Pinned here with a 1 s budget and a 10 s answer.
    "deliver a budget-exceeded answer on an async-capable container" in {
      val previous = LiftRules.cometRequestTimeout
      try {
        LiftRules.cometRequestTimeout = Full(1) // 1 second budget
        // the answer is scheduled 10 s out, far beyond the budget
        val xml = get("/lp/delay?ms=10000").flatMap(_.xml)
          .openOrThrowException("the delayed reply should be XML")
        xml must ==/(<answer>done</answer>).when(jetty.running)
      } finally {
        LiftRules.cometRequestTimeout = previous
      }
    }
  }

  step {
    tryo {
      jetty.stop()
    }
  }
}
