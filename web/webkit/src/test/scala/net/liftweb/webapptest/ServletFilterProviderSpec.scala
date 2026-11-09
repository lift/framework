/*
 * Characterization test for the Lift servlet filter boot path.
 *
 * THEORY OF WHAT THIS TEST TESTS
 * ------------------------------
 * ServletFilterProvider is the boot path used by annotation-less deployments:
 * the container creates the filter, init captures the ServletContext and
 * boots Lift, and afterwards requests dispatched through the filter are
 * served by the running LiftServlet. Proving this requires a real container:
 * the filter lifecycle and the servlet dispatch are container behavior.
 *
 * The test starts the embedded Jetty instance with the test web application
 * (whose web.xml declares LiftFilter), and issues a request for the static
 * template index.html, which Lift's filter handles and renders.
 *
 * This test cites the behavior statement it proves in
 * docs/servlet-behavior-test-map.md.
 */
package net.liftweb
package webapptest

import org.specs2.mutable.Specification

import java.net.{InetAddress, URL}

import common._
import util.Helpers._
import http.testing.RequestKit

object ServletFilterProviderSpec extends Specification with RequestKit {
  sequential

  private def reachableLocalAddress = {
    val l = InetAddress.getLocalHost
    tryo { l.isReachable(50) } match {
      case Full(true) => l.getHostAddress
      case _          => "127.0.0.1"
    }
  }

  private val host_ = System.getProperty("net.liftweb.webapptest.filter.host", reachableLocalAddress)
  private val port_ = System.getProperty("net.liftweb.webapptest.filter.port", "8484").toInt

  private lazy val baseUrl_ = new URL("http://%s:%s".format(host_, port_))

  private lazy val jetty = new JettyTestServer(Full(baseUrl_))

  def baseUrl = jetty.baseUrl.toString

  step(jetty.start())

  "the lift servlet filter" should {

    "boot lift and serve a subsequent request" in {
      // the request goes through the filter: Lift boots at filter init and
      // the filter dispatches the request to the LiftServlet, which renders
      // the requested template
      val resp = get("/index.html")

      resp.map(_.code) must_== Full(200)
      val body = resp.flatMap(_.body)
        .openOrThrowException("index.html should be served with a body")
      body.length must be_>(0)
    }
  }

  step {
    tryo {
      jetty.stop()
    }
  }
}
