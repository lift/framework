/*
 * Characterization tests for HTTPRequestServlet, the Lift adapter over the
 * container's HttpServletRequest.
 *
 * THEORY OF WHAT THESE TESTS TEST
 * -------------------------------
 * HTTPRequestServlet exposes the container request's facts — URI, query
 * string, headers, cookies, remote address, content type — as Lift's
 * container-neutral HTTPRequest. The adapter must present exactly what the
 * raw servlet request presents: a property over arbitrary header sets,
 * because any header the client sends must be visible through the adapter.
 *
 * Boundary conditions: an absent header reads as empty through the adapter;
 * duplicate cookie names are each visible; a request with cookies carries
 * them through (the adapter first touches the session so the container
 * captures the session cookie, then lists the request's cookies).
 *
 * The raw request is the testkit's MockHttpServletRequest, which implements
 * the servlet interfaces with plain fields; the adapter must agree with
 * those fields.
 *
 * This test cites the behavior statement it proves in
 * docs/servlet-behavior-test-map.md.
 */
package net.liftweb
package webapptest

import org.specs2.Specification
import org.specs2.ScalaCheck
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import java.util.Locale

import common.{Full, Box}
import mocks.{MockHttpServletRequest, MockServletContext}
import http.provider.servlet.HTTPRequestServlet

object HTTPRequestServletSpec extends Specification with ScalaCheck {
  def is = s2"""

  HTTPRequestServlet request facts

  match the raw servlet request for arbitrary header sets  ${headerFacts}
  expose uri, query string, remote address, and content type
    for a plain request  ${plainFacts}
  expose cookies including a duplicate cookie name  ${cookieFacts}

  """

  private def adapterOn(req: MockHttpServletRequest): HTTPRequestServlet =
    new HTTPRequestServlet(req, testProvider)

  private object testProvider extends http.provider.HTTPProvider {
    def context: http.provider.HTTPContext =
      new http.provider.servlet.HTTPServletContext(new MockServletContext("target/test1-1.0-SNAPSHOT"))
  }

  // Property: for an arbitrary set of headers, each header is visible
  // through the adapter with exactly the values the raw request holds.
  private def headerFacts = {
    org.scalacheck.Prop.forAll(Gen.listOf(for {
      name <- Gen.oneOf("Accept", "X-Test", "User-Agent", "Referer", "X-Lift")
      value <- Gen.alphaNumStr
    } yield (name, value))) { (headers: List[(String, String)]) =>
      val raw = new MockHttpServletRequest
      val grouped = headers.groupBy(_._1).map { case (n, vs) => (n, vs.map(_._2)) }
      raw.headers = grouped

      val adapter = adapterOn(raw)

      grouped.forall { case (name, values) =>
        adapter.headers(name).toSet == values.toSet
      } &&
      adapter.headers.map(_.name).toSet == grouped.keySet &&
      (grouped.isEmpty || adapter.header(grouped.keys.head).isDefined)
    }
  }

  private def plainFacts = {
    val raw = new MockHttpServletRequest
    raw.requestUri = "/some/path"
    raw.queryString = "a=1&b=2"
    raw.remoteAddr = "10.1.2.3"
    raw.contentType = "text/plain"
    raw.method = "GET"

    val adapter = adapterOn(raw)

    adapter.uri must_== "/some/path"
    adapter.queryString must_== Full("a=1&b=2")
    adapter.remoteAddress must_== "10.1.2.3"
    adapter.contentType must_== Full("text/plain")
    adapter.method must_== "GET"

    // a request with an empty header set reads as empty through the adapter
    adapter.headers must_== Nil
    adapter.headers("X-Missing") must_== Nil
  }

  private def cookieFacts = {
    val raw = new MockHttpServletRequest
    raw.cookies = List(
      new jakarta.servlet.http.Cookie("a", "1"),
      new jakarta.servlet.http.Cookie("b", "2"),
      // duplicate name: both cookies are visible through the adapter
      new jakarta.servlet.http.Cookie("a", "3")
    )

    val adapter = adapterOn(raw)

    val byName = adapter.cookies.groupBy(_.name)
    byName.size must_== 2
    byName("a").map(_.value) must_== List("1", "3")
    byName("b").map(_.value) must_== List("2")
  }
}
