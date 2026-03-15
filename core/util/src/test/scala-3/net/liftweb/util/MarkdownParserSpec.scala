/*
 * Copyright 2026 Lift Committers and Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb
package util

import scala.xml.{ Elem, NodeSeq }

import org.specs2.mutable.Specification

import net.liftweb.common.Full


/**
 * Basic characterization tests for MarkdownParser.
 * MarkdownParser delegates to Html5.parse so these also exercise that pipeline.
 */
class MarkdownParserSpec extends Specification {
  "MarkdownParser" should {
    "return Full for basic paragraph markdown" in {
      MarkdownParser.parse("hello world") must beAnInstanceOf[Full[NodeSeq]]
    }

    "render a paragraph as a <p> element" in {
      val result = MarkdownParser.parse("hello world").openOrThrowException("Test")
      val paras = result.filter(n => n.isInstanceOf[Elem] && n.asInstanceOf[Elem].label == "p")
      paras must not(beEmpty)
    }

    "render bold markdown as <strong>" in {
      val result = MarkdownParser.parse("**bold text**").openOrThrowException("Test")
      val strong = result \\ "strong"
      strong must not(beEmpty)
      strong.text must contain("bold text")
    }

    "render italic markdown as <em>" in {
      val result = MarkdownParser.parse("*italic text*").openOrThrowException("Test")
      val em = result \\ "em"
      em must not(beEmpty)
      em.text must contain("italic text")
    }

    "render a markdown link as <a>" in {
      val result = MarkdownParser.parse("[link text](http://example.com)").openOrThrowException("Test")
      val anchors = result \\ "a"
      anchors must not(beEmpty)
      anchors.text must contain("link text")
    }

    "render a code block as <pre><code>" in {
      val result = MarkdownParser.parse("    code line").openOrThrowException("Test")
      val pre = result \\ "pre"
      pre must not(beEmpty)
    }

    "childrenOfBody extracts children from a body element" in {
      val input = <html><body><p>x</p><p>y</p></body></html>
      val result = MarkdownParser.childrenOfBody(input)
      result.length must beGreaterThanOrEqualTo(2)
      (result \\ "p").text must contain("x")
    }

    "childrenOfBody returns input unchanged when no body element" in {
      val input = <div><p>x</p></div>
      val result = MarkdownParser.childrenOfBody(input)
      result === input
    }
  }

}
