/*
 * Characterization test for Lift's asynchronous provider selection.
 *
 * THEORY OF WHAT THIS TEST TESTS
 * ------------------------------
 * LiftRules selects the asynchronous provider for long polling at boot: the
 * first registered AsyncProviderMeta whose suspendResumeSupport_? is true
 * supplies the provider function. On a JVM with the servlet API present, the
 * class-level check of the standard Servlet 3.0 provider succeeds, so the
 * standard provider is the selection: LiftRules.asyncProviderMeta is Full,
 * its class-level suspendResumeSupport_? is true, and its providerFunction
 * vends a Servlet30AsyncProvider for a request.
 *
 * Boundary note: LiftRules.asyncProviderMeta is a lazy val — once evaluated,
 * the selection is cached for the JVM, and LiftRules is a singleton object
 * shared by all tests in this suite. The "selection list empty" boundary is
 * therefore observable only before the first evaluation and cannot be pinned
 * in-process; this test pins the selection that the running system actually
 * uses, which is the behavior the later migration phases must preserve.
 *
 * This test cites the behavior statements it proves in
 * docs/servlet-behavior-test-map.md.
 */
package net.liftweb
package webapptest

import org.specs2.mutable.Specification

import common._
import mocks.{MockHttpServletRequest, MockServletContext}
import util.Helpers._
import http._
import http.provider.servlet.containers.Servlet30AsyncProvider

object AsyncProviderSelectionSpec extends Specification {
  sequential

  "async provider selection" should {
    "select the standard servlet 3.0 provider" in {
      val meta = LiftRules.asyncProviderMeta

      (meta.isDefined && meta.map(_.suspendResumeSupport_?).openOr(false)) must beTrue
    }

    "vend a standard servlet 3.0 provider from the selected meta" in {
      val providerFnBox = LiftRules.theServletAsyncProvider
      (providerFnBox.isDefined &&
        providerFnBox.map(f => f(newRequest).getClass == classOf[Servlet30AsyncProvider]).openOr(false)) must beTrue
    }

    "register only the standard servlet 3.0 provider" in {
      // Phase 2: the legacy Jetty reflection providers are gone; the
      // selection list contains only the standard Servlet 3.0 provider
      LiftRules.asyncProviderMetaList.size must_== 1
      LiftRules.asyncProviderMetaList.head must_== Servlet30AsyncProvider
    }
  }

  /**
   * Build a request the provider function can wrap. The mock request stands
   * in for a container request; the provider object itself only holds the
   * request until suspend/resume is called, which this test does call.
   */
  private def newRequest: http.provider.HTTPRequest = {
    val servletReq = new MockHttpServletRequest
    new http.provider.servlet.HTTPRequestServlet(servletReq, testProvider)
  }

  /**
   * A minimal HTTPProvider whose context is an empty mock servlet context.
   * The adapter only uses the provider for snapshots and resume callbacks,
   * which this test reaches.
   */
  private object testProvider extends http.provider.HTTPProvider {
    def context: http.provider.HTTPContext =
      new http.provider.servlet.HTTPServletContext(new MockServletContext("target/test1-1.0-SNAPSHOT"))
  }
}
