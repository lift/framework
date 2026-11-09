/*
 * Characterization test for Lift's multipart upload ingestion.
 *
 * THEORY OF WHAT THESE TESTS TEST
 * -------------------------------
 * A multipart form upload sent to Lift is ingested with byte-identical file
 * content and correct field metadata (field name, filename, mime type, size).
 * Ingestion goes through the container's getParts() via
 * HTTPRequestServlet.extractFiles; this test pins that behavior so any
 * future change must satisfy the same behavior statement, and any difference
 * the pin exposes is flagged to the user for a decision rather than absorbed
 * silently.
 *
 * The test posts a hand-built multipart body — byte-precise, with a known
 * boundary — through the real container, and the server endpoint echoes the
 * field name, filename, mime type, size, and SHA-256 digest of what it
 * received. Property over arbitrary small file contents, including binary
 * edge bytes (0x00, 0xFF, newline, carriage return).
 *
 * Boundaries outside the property: an empty file (zero bytes) and a
 * non-ASCII filename.
 *
 * This test cites the behavior statement it proves in
 * docs/servlet-behavior-test-map.md.
 */
package net.liftweb
package webapptest

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.specs2.matcher.XmlMatchers
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import java.net.{InetAddress, URL}

import common._
import util.Helpers._
import http.LiftRules
import http.testing.RequestKit

object UploadIngestionSpec extends Specification with ScalaCheck with XmlMatchers with RequestKit {
  sequential

  private def reachableLocalAddress = {
    val l = InetAddress.getLocalHost
    tryo { l.isReachable(50) } match {
      case Full(true) => l.getHostAddress
      case _          => "127.0.0.1"
    }
  }

  private val host_ = System.getProperty("net.liftweb.webapptest.upload.host", reachableLocalAddress)
  private val port_ = System.getProperty("net.liftweb.webapptest.upload.port", "8585").toInt

  private lazy val baseUrl_ = new URL("http://%s:%s".format(host_, port_))

  private lazy val jetty = new JettyTestServer(Full(baseUrl_))

  def baseUrl = jetty.baseUrl.toString

  step(jetty.start())

  private val boundary = "LiftCharacterizationUploadBoundary"

  /**
   * Build a multipart/form-data body by hand: the test controls the bytes
   * exactly, so the assertion compares the server's view against ground
   * truth rather than against a client-side parser's view.
   */
  private def multipartBody(field: String, fileName: String, content: Array[Byte]): (Array[Byte], String) = {
    val head =
      "--" + boundary + "\r\n" +
      "Content-Disposition: form-data; name=\"" + field + "\"; filename=\"" + fileName + "\"\r\n" +
      "Content-Type: application/octet-stream\r\n\r\n"
    val tail = "\r\n--" + boundary + "--\r\n"
    val bytes = head.getBytes("UTF-8") ++ content ++ tail.getBytes("UTF-8")
    (bytes, "multipart/form-data; boundary=" + boundary)
  }

  private def postUpload(body: Array[Byte], contentType: String) = {
    val resp = post("/lp/upload", body, contentType)
    resp.foreach(r => println("UPLOAD-DEBUG code=" + r.code + " body=" + r.bodyAsString))
    resp.flatMap(_.xml).openOrThrowException("the upload echo should be XML")
  }

  private def sha256Hex(bytes: Array[Byte]): String = {
    val digest = java.security.MessageDigest.getInstance("SHA-256")
    digest.digest(bytes).map(b => "%02x".format(b)).mkString
  }

  "multipart upload ingestion" should {

    // Property: for arbitrary small file contents — including binary edge
    // bytes — what the server receives is byte-identical to what the client
    // sent, and the field metadata travels intact.
    "ingest file content byte-identically with correct metadata" in {
      implicit val contents: Arbitrary[Array[Byte]] = Arbitrary(
        Gen.containerOf[Array, Byte](Gen.oneOf(
          Gen.const(0.toByte), Gen.const(255.toByte), Gen.const('\r'.toByte),
          Gen.const('\n'.toByte), Gen.choose(-128, 127).map(_.toByte)))
      )

      prop { (content: Array[Byte]) =>
        val (body, contentType) = multipartBody("upfile", "data.bin", content)
        val xml = postUpload(body, contentType)

        (xml \\ "field").text must_== "upfile"
        (xml \\ "fileName").text must_== "data.bin"
        (xml \\ "mime").text must_== "application/octet-stream"
        (xml \\ "size").text must_== content.length.toString
        (xml \\ "sha256").text must_== sha256Hex(content)
      }.when(jetty.running)
    }

    "ingest an empty file with size zero" in {
      val (body, contentType) = multipartBody("upfile", "empty.bin", Array.empty[Byte])
      val xml = postUpload(body, contentType)

      (xml \\ "size").text must_== "0"
      (xml \\ "sha256").text must_== sha256Hex(Array.empty[Byte])
    }

    // Option A restoration: LiftRules.maxMimeSize / maxMimeFileSize are
    // enforced in the ingestion path again — an upload beyond the limit
    // fails with the framework error (500) instead of being ingested, and
    // an upload within the limit passes. (During the getParts migration
    // these knobs lost their reader; this pins their restoration.)
    "reject an upload beyond LiftRules.maxMimeSize" in {
      val previous = LiftRules.maxMimeSize
      try {
        LiftRules.maxMimeSize = 1000L
        val content = Array.fill(5120)(7.toByte) // 5 KB against a 1 KB budget
        val (body, contentType) = multipartBody("upfile", "big.bin", content)
        val resp = post("/lp/upload", body, contentType)
        resp.map(_.code) must_== Full(500)
      } finally {
        LiftRules.maxMimeSize = previous
      }
    }

    "ingest an upload at exactly LiftRules.maxMimeSize" in {
      val previous = LiftRules.maxMimeSize
      try {
        LiftRules.maxMimeSize = 2000L
        val content = Array.fill(2048 - 60)(7.toByte) // body fits the budget
        val (body, contentType) = multipartBody("upfile", "fit.bin", content)
        val xml = postUpload(body, contentType)
        (xml \\ "sha256").text must_== sha256Hex(content)
      } finally {
        LiftRules.maxMimeSize = previous
      }
    }

    "ingest a file whose filename carries non-ASCII characters" in {
      val content = Array(1.toByte, 2.toByte, 3.toByte)
      val (body, contentType) = multipartBody("upfile", "caf\u00e9-cr\u00e8me.txt", content)
      val xml = postUpload(body, contentType)

      (xml \\ "fileName").text must_== "caf\u00e9-cr\u00e8me.txt"
      (xml \\ "sha256").text must_== sha256Hex(content)
    }
  }

  step {
    tryo {
      jetty.stop()
    }
  }
}
