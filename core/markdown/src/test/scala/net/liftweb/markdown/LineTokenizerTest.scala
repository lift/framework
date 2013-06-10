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
 * Tests the Line Tokenizer that prepares input for parsing.
 */
@RunWith(classOf[JUnitRunner])
class LineTokenizerTest extends LineTokenizer with FlatSpec with ShouldMatchers{

    "The LineTokenizer" should "split input lines correctly" in {
        splitLines("line1\nline2\n") should equal (List("line1", "line2"))
        splitLines("line1\nline2 no nl") should equal (List("line1", "line2 no nl"))
        splitLines("test1\n\ntest2\n") should equal (List("test1", "", "test2"))
        splitLines("test1\n\ntest2\n\n") should equal (List("test1", "", "test2"))
        splitLines("\n\n") should equal (Nil)
        splitLines("\n") should equal (Nil)
        splitLines("") should equal (List(""))
    }

    it should "preprocess the input correctly" in {
        tokenize("[foo]: http://example.com/  \"Optional Title Here\"") should equal(
            (new MarkdownLineReader(List(), Map( "foo"->new LinkDefinition("foo", "http://example.com/", Some("Optional Title Here")) )) ) )

        tokenize(
"""[Baz]:    http://foo.bar
'Title next line'
some text
> bla

[fOo]: http://www.example.com "A Title"
more text
[BAR]: <http://www.example.com/bla> (Also a title)"""
            ) should equal ( new MarkdownLineReader( List(
new OtherLine("some text"),
new BlockQuoteLine("> ", "bla"),
new EmptyLine(""),
new OtherLine("more text")
            ), Map (
"bar"->new LinkDefinition("bar", "http://www.example.com/bla", Some("Also a title")),
"baz"->new LinkDefinition("baz", "http://foo.bar", Some("Title next line")),
"foo"->new LinkDefinition("foo", "http://www.example.com", Some("A Title"))
    )))

    }

    it should "parse different line types" in {
        def p(line:String) = {
            lineToken(new LineReader(Seq(line))) match {
                case Success(result, _) => result
            }
        }
        p("a line")          should equal (new OtherLine("a line"))
        p("    a code line") should equal (new CodeLine("    ", "a code line"))
        p("#a header#")      should equal (new AtxHeaderLine("#", "a header#"))
        p("> a quote")       should equal (new BlockQuoteLine("> ", "a quote"))
        p(" \t ")            should equal (new EmptyLine(" \t "))
        p("* an item")       should equal (new UItemStartLine("* ", "an item"))
        p("- an item")       should equal (new UItemStartLine("- ", "an item"))
        p("+ an item")       should equal (new UItemStartLine("+ ", "an item"))
        p("===")             should equal (new SetExtHeaderLine("===", 1))
        p("---  ")           should equal (new SetExtHeaderLine("---  ", 2))
        p("- - -")           should equal (new RulerLine("- - -"))
    }
}