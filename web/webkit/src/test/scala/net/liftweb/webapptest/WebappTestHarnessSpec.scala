/*
 * Guard-rail test for the web application test harness: the jwebunit
 * library is gone from the test classpath.
 *
 * THEORY OF WHAT THIS TEST TESTS
 * ------------------------------
 * The end-to-end web assertions run through HtmlUnit directly (Phase 3,
 * replacing jwebunit). If jwebunit returned to the test classpath it would
 * bring its bundled HtmlUnit 2.x line, which shares the
 * com.gargoylesoftware.htmlunit package namespace with HtmlUnit 4 and
 * collides with it (verified while porting: the mixture fails with
 * NoClassDefFoundError). The guard loads jwebunit's marker class and
 * requires the load to fail: if the library is present, the class loads and
 * this test fails. (The java.class.path of the forked test JVM does not
 * list the jars, so a path-string inspection is vacuous — verified with a
 * positive control during this phase — and class loading is the honest
 * oracle.)
 */
package net.liftweb
package webapptest

import org.specs2.mutable.Specification

object WebappTestHarnessSpec extends Specification {
  "the webapp test harness" should {

    "carry the htmlunit client" in {
      classOf[org.htmlunit.WebClient].getName must_== "org.htmlunit.WebClient"
    }

    "carry no jwebunit classes on the classpath" in {
      var found = true
      try {
        Class.forName("net.sourceforge.jwebunit.junit.WebTester")
      } catch {
        case _: ClassNotFoundException => found = false
      }
      found must beFalse
    }
  }
}
