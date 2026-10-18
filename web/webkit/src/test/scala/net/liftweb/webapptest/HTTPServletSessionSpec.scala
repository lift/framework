/*
 * Characterization tests for HTTPServletSession, the Lift adapter over the
 * container's HttpSession.
 *
 * THEORY OF WHAT THESE TESTS TEST
 * -------------------------------
 * HTTPServletSession is a thin delegation: attributes written through the
 * adapter must be visible in the raw container session and vice versa, for
 * arbitrary names and values (a property, because any name/value pair the
 * application chooses must round-trip); invalidation and session identity
 * must be delegated to the container session rather than cached in the
 * adapter, because session state belongs to the container.
 *
 * The container session here is a small recording fake (RecordingSession)
 * rather than the testkit's MockHttpSession: the mock's invalidate is a
 * silent no-op and its identity is null, so delegation of invalidate and
 * identity would be unobservable through it. The recording fake records
 * invalidate calls and stores attributes, which makes delegation observable
 * by comparing the two views.
 *
 * These tests cite the behavior statements they prove in
 * docs/servlet-behavior-test-map.md.
 */
package net.liftweb
package webapptest

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import jakarta.servlet.ServletContext
import jakarta.servlet.http.{HttpSession, HttpSessionContext}
import java.util.Enumeration

import http.provider.servlet.HTTPServletSession

object HTTPServletSessionSpec extends Specification with ScalaCheck {
  sequential

  private class RecordingSession extends HttpSession {
    val attributes = scala.collection.mutable.Map[String, Object]()
    var invalidated = 0
    private val id = java.util.UUID.randomUUID().toString

    def getId: String = id
    def invalidate(): Unit = invalidated += 1
    def getAttribute(key: String): Object = attributes.getOrElse(key, null)
    def setAttribute(key: String, value: Object): Unit = attributes(key) = value
    def removeAttribute(key: String): Unit = attributes -= key
    def getAttributeNames: Enumeration[String] = {
      val keys = attributes.keys.toList.iterator
      new Enumeration[String] {
        def hasMoreElements: Boolean = keys.hasNext
        def nextElement: String = keys.next
      }
    }

    def getCreationTime: Long = 0L
    def getLastAccessedTime: Long = 0L
    def getMaxInactiveInterval: Int = 0
    def setMaxInactiveInterval(i: Int): Unit = ()
    def isNew: Boolean = false
    def getServletContext: ServletContext = null

    // deprecated servlet 2.x pair, still on the interface
    def getValue(key: String): Object = getAttribute(key)
    def putValue(key: String, value: Object): Unit = setAttribute(key, value)
    def removeValue(key: String): Unit = removeAttribute(key)
    def getValueNames: Array[String] = Array.empty
    def getSessionContext: HttpSessionContext = null
  }

  private def adapterOn(session: RecordingSession): HTTPServletSession =
    new HTTPServletSession(session)

  "HTTPServletSession" should {

    // Property: for arbitrary attribute names and string values, the adapter
    // and the raw session are two views of the same storage. Boundaries: an
    // empty-string attribute name, overwriting an existing attribute, and
    // removing an attribute that was set.
    "round-trip arbitrary attribute names and values through both views" in {
      val names = Gen.frequency(1 -> Gen.const(""), 9 -> Gen.nonEmptyListOf(Gen.alphaNumChar).map(_.mkString))
      val values = Gen.alphaNumStr

      org.scalacheck.Prop.forAll(names, values) { (name: String, value: String) =>
        val raw = new RecordingSession
        val adapter = adapterOn(raw)

        adapter.setAttribute(name, value)
        raw.getAttribute(name) must_== value

        raw.setAttribute(name, value + "!")
        adapter.attribute(name) must_== value + "!"

        adapter.removeAttribute(name)
        raw.getAttribute(name) must_== null
      }
    }

    "delegate identity and invalidation to the container session" in {
      val raw = new RecordingSession
      val adapter = adapterOn(raw)

      // identity is delegated: the adapter reports the container session's id
      adapter.sessionId must_== raw.getId

      adapter.setAttribute("x", "1")
      raw.getAttribute("x") must_== "1"

      // terminate delegates to the container session's invalidation
      adapter.terminate
      raw.invalidated must_== 1
    }
  }
}
