package net.liftweb.markdown

/*
 * Copyright 2013 WorldWide Conferencing, LLC
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
 *
 * Based on https://github.com/chenkelmann/actuarius originally developed by
 * Christoph Henkelmann http://henkelmann.eu/
 */

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * tests parsing of individual lines
 */
@RunWith(classOf[JUnitRunner])
class LineParsersTest extends FlatSpec with ShouldMatchers with LineParsers{

    "The LineParsers" should "parse horizontal rulers" in {
        val p = ruler
        apply(p, "---")           should equal (new RulerLine("---"))
        apply(p, "***")           should equal (new RulerLine("***"))
        apply(p, "---------")     should equal (new RulerLine("---------"))
        apply(p, "*********")     should equal (new RulerLine("*********"))
        apply(p, "- - -")         should equal (new RulerLine("- - -"))
        apply(p, "* * *")         should equal (new RulerLine("* * *"))
        apply(p, "  ---")         should equal (new RulerLine("  ---"))
        apply(p, "  ***")         should equal (new RulerLine("  ***"))
        apply(p, "  - - -  ----") should equal (new RulerLine("  - - -  ----"))
        apply(p, "  * * *  ****") should equal (new RulerLine("  * * *  ****"))
    }

    it should "parse a ruler that starts like a setext header line" in {
        val p = setext2OrRulerOrUItem
        apply(p, "- - -")         should equal (new RulerLine("- - -"))
    }

    it should "parse a setext style level 1 header underline" in {
        val p = setextHeader1
        apply(p, "=") should equal (new SetExtHeaderLine("=", 1))
        apply(p, "== ") should equal (new SetExtHeaderLine("== ", 1))
        apply(p, "========== \t ") should equal (new SetExtHeaderLine("========== \t ", 1))
    }

    it should "parse a setext style level 2 header underline" in {
        val p = setextHeader2
        apply(p, "-") should equal (new SetExtHeaderLine("-", 2))
        apply(p, "-- ") should equal (new SetExtHeaderLine("-- ", 2))
        apply(p, "---------- \t ") should equal (new SetExtHeaderLine("---------- \t ", 2))
    }

    it should "parse an atx header line" in {
        val p = atxHeader
        apply(p, "#foo") should equal (new AtxHeaderLine("#", "foo"))
        apply(p, "## #foo##") should equal (new AtxHeaderLine("##", " #foo##"))
    }

    it should "parse an empty line" in {
        val p = emptyLine
        apply (p, "") should equal (new EmptyLine(""))
        apply (p, "  \t ") should equal (new EmptyLine("  \t "))
        evaluating (apply (p, " not empty ")) should produce[IllegalArgumentException]
    }

    it should "parse arbitrary lines as OtherLine tokens" in {
        val p = otherLine
        apply(p, "a line")          should equal (new OtherLine("a line"))
    }

    it should "parse quoted block lines" in {
        val p = blockquoteLine
        apply(p, "> quote") should equal (new BlockQuoteLine("> ", "quote"))
        apply(p, ">     codequote") should equal (new BlockQuoteLine("> ", "    codequote"))
        apply(p, "   >     codequote") should equal (new BlockQuoteLine("   > ", "    codequote"))
        evaluating(apply(p, "not a quote")) should produce[IllegalArgumentException]
    }

    it should "parse unordered item start lines" in {
        val p = uItemStartLine
        apply(p, "* foo") should equal (new UItemStartLine("* ", "foo"))
        apply(p, " * foo") should equal (new UItemStartLine(" * ", "foo"))
        apply(p, "  * foo") should equal (new UItemStartLine("  * ", "foo"))
        apply(p, "   * foo") should equal (new UItemStartLine("   * ", "foo"))
        apply(p, "   *    foo") should equal (new UItemStartLine("   *    ", "foo"))
        apply(p, "   * \t  foo") should equal (new UItemStartLine("   * \t  ", "foo"))
        apply(p, "   * \t  foo  ") should equal (new UItemStartLine("   * \t  ", "foo  "))

        evaluating(apply(p, "*foo")) should produce[IllegalArgumentException]
        evaluating(apply(p, "    * foo")) should produce[IllegalArgumentException]
        evaluating(apply(p, "1. foo")) should produce[IllegalArgumentException]

        apply(p, "* foo") should equal (new UItemStartLine("* ", "foo"))
        apply(p, "+ foo") should equal (new UItemStartLine("+ ", "foo"))
        apply(p, "- foo") should equal (new UItemStartLine("- ", "foo"))
    }


    it should "parse ordered item start lines" in {
        val p = oItemStartLine
        apply(p, "1. foo") should equal (OItemStartLine("1. ", "foo"))
        apply(p, " 12. foo") should equal (OItemStartLine(" 12. ", "foo"))
        apply(p, "  0. foo") should equal (OItemStartLine("  0. ", "foo"))
        apply(p, "   44444444. foo") should equal (OItemStartLine("   44444444. ", "foo"))
        apply(p, "   465789.    foo") should equal (OItemStartLine("   465789.    ", "foo"))
        apply(p, "   4455. \t  foo") should equal (OItemStartLine("   4455. \t  ", "foo"))
        apply(p, "   9. \t  foo  ") should equal (OItemStartLine("   9. \t  ", "foo  "))

        evaluating(apply(p, "1.foo")) should produce[IllegalArgumentException]
        evaluating(apply(p, "    1. foo")) should produce[IllegalArgumentException]
        evaluating(apply(p, "* foo")) should produce[IllegalArgumentException]
    }

    it should "parse link definitions" in {
        val p = linkDefinitionStart
        apply(p, "[foo]: http://example.com/  \"Optional Title Here\"") should equal (
        new LinkDefinitionStart("foo", "http://example.com/"), Some("Optional Title Here"))
        apply(p, "[foo]: http://example.com/") should equal (
        new LinkDefinitionStart("foo", "http://example.com/"), None)
        apply(p, "[Foo]: http://example.com/  'Optional Title Here'") should equal (
        new LinkDefinitionStart("foo", "http://example.com/"), Some("Optional Title Here"))
        apply(p, "[Foo]: http://example.com/?bla=<>  (Optional Title Here)") should equal (
        new LinkDefinitionStart("foo", "http://example.com/?bla=&lt;&gt;"), Some("Optional Title Here"))
        apply(p, "[Foo]: http://example.com/?bla=<>  (Optional Title Here)") should equal (
        new LinkDefinitionStart("foo", "http://example.com/?bla=&lt;&gt;"), Some("Optional Title Here"))
    }

    it should "parse link titles" in {
        val p = linkDefinitionTitle
        apply(p, "  (Optional Title Here)  ") should equal ("Optional Title Here")
    }
    
    it should "parse openings of fenced code blocks" in {
      val p = fencedCodeStartOrEnd
      apply(p, "```") should equal (
      new FencedCode("```"))
      apply(p, "   ```\t") should equal (
      new FencedCode("   ```\t"))
      apply(p, "  ``` \t ") should equal (
      new FencedCode("  ``` \t "))
      apply(p, "  ``` \t java  \t ") should equal (
      new ExtendedFencedCode("  ``` \t ", "java  \t "))
    }
}