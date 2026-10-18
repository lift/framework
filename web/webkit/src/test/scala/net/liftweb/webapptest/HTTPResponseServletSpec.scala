/*
 * Characterization tests for HTTPResponseServlet, the Lift adapter over the
 * container's HttpServletResponse.
 *
 * THEORY OF WHAT THESE TESTS TEST
 * -------------------------------
 * HTTPResponseServlet writes Lift responses to the container: the status,
 * headers, and body written through the adapter must be recorded by the raw
 * servlet response exactly as written — a property over arbitrary
 * (status, header, body) triples, because any response Lift produces must
 * reach the wire as Lift wrote it.
 *
 * Boundary conditions: a header value with non-ASCII characters survives
 * as written (the adapter passes strings through unchanged); a status set
 * with an empty body is still the status the container reports; a header
 * listed in LiftRules.overwrittenReponseHeaders is set (replaced) rather
 * than added (appended).
 *
 * The raw response is the testkit's MockHttpServletResponse, which records
 * status, headers, and body output in plain fields; the adapter must agree
 * with those fields.
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

import java.io.PrintWriter
import java.io.ByteArrayOutputStream

import mocks.MockHttpServletResponse
import http.provider.servlet.HTTPResponseServlet

object HTTPResponseServletSpec extends Specification with ScalaCheck {
  def is = s2"""

  HTTPResponseServlet response writes

  record status, headers, and body for arbitrary triples  ${triples}
  record a header value with non-ASCII characters as written  ${nonAscii}
  record a status with an empty body  ${statusOnly}
  replace a header listed in overwrittenReponseHeaders rather than appending
    to it  ${overwritten}

  """

  private def adapterOn(resp: MockHttpServletResponse): HTTPResponseServlet =
    new HTTPResponseServlet(resp)

  private def newMockResponse: (MockHttpServletResponse, ByteArrayOutputStream) = {
    val stream = new ByteArrayOutputStream
    val mock = new MockHttpServletResponse(new PrintWriter(stream, true),
      new jakarta.servlet.ServletOutputStream {
        def write(b: Int): Unit = stream.write(b)
        def isReady: Boolean = true
        def setWriteListener(listener: jakarta.servlet.WriteListener): Unit = ()
      })
    (mock, stream)
  }

  // Property: for arbitrary status, header name/value, and body, the raw
  // response records exactly what the adapter wrote.
  private def triples = {
    val statuses = Gen.oneOf(200, 201, 204, 302, 400, 404, 500)
    val headerPairs = for {
      name <- Gen.oneOf("X-Test", "Content-Language", "X-Lift")
      value <- Gen.alphaNumStr
    } yield (name, value)
    val bodies = Gen.alphaNumStr

    org.scalacheck.Prop.forAll(statuses, headerPairs, bodies) { (status: Int, header: (String, String), body: String) =>
      val (raw, stream) = newMockResponse
      val adapter = adapterOn(raw)

      adapter.setStatus(status)
      adapter.addHeaders(List(http.provider.HTTPParam(header._1, List(header._2))))
      adapter.outputStream.write(body.getBytes("UTF-8"))

      (raw.getStatus == status) &&
      (raw.getHeader(header._1) == header._2) &&
      (stream.toByteArray.toSeq == body.getBytes("UTF-8").toSeq)
    }
  }

  private def nonAscii = {
    val (raw, _) = newMockResponse
    val adapter = adapterOn(raw)

    val value = "caf\u00e9-cr\u00e8me"
    adapter.addHeaders(List(http.provider.HTTPParam("X-Flavor", List(value))))

    raw.getHeader("X-Flavor") must_== value
  }

  private def statusOnly = {
    val (raw, _) = newMockResponse
    val adapter = adapterOn(raw)

    adapter.setStatus(204)
    adapter.getStatus must_== 204
    raw.getStatus must_== 204
  }

  private def overwritten = {
    val previousDefault = http.LiftRules.overwrittenReponseHeaders.default.get
    try {
      http.LiftRules.overwrittenReponseHeaders.default.set(() => List("X-Once"))
      val (raw, _) = newMockResponse
      val adapter = adapterOn(raw)

      adapter.addHeaders(List(http.provider.HTTPParam("X-Once", List("first"))))
      adapter.addHeaders(List(http.provider.HTTPParam("X-Once", List("second"))))

      raw.getHeaders("X-Once").size must_== 1
      raw.getHeader("X-Once") must_== "second"
    } finally {
      http.LiftRules.overwrittenReponseHeaders.default.set(previousDefault)
    }
  }
}
