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

import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import collection.SortedMap
import org.junit.runner.RunWith

/**
 * Tests basic parsers that are used by the more complex parsing steps.
 */

@RunWith(classOf[JUnitRunner])
class BaseParsersTest extends FlatSpec with ShouldMatchers with BaseParsers{

    "The BaseParsers" should "parse a newline" in {
        val p = nl
        apply(p, "\n") should equal ("\n")
        evaluating(apply(p, "\r\n")) should produce[IllegalArgumentException]
        evaluating(apply(p, "  \n")) should produce[IllegalArgumentException]
    }

    it should "parse whitespace" in {
        val p = ws
        apply(p, " ") should equal (" ")
        apply(p, "\t") should equal ("\t")
        apply(p, "    ") should equal ("    ")
        apply(p, "\t\t") should equal ("\t\t")
        apply(p, "  \t  \t  ") should equal ("  \t  \t  ")
        //we want newlines to be treated diferrently from other ws
        evaluating (apply(p, "\n")) should produce[IllegalArgumentException]
    }

    it should "be able to look behind" in {
        apply (((elem('a') ~ lookbehind(Set('a')) ~ elem('b'))^^{case a~lb~b=>a+""+b}), "ab") should equal ("ab")
        evaluating {apply (((elem('a') ~ lookbehind(Set('b')) ~ elem('b'))^^{case a~b=>a+""+b}), "ab")} should produce[IllegalArgumentException]

        apply( (elem('a') ~ not(lookbehind(Set(' ', '\t', '\n'))) ~ '*' ), "a*"  )

    }

    it should "parse chars in ranges" in {
        val p = ranges(SortedMap('A' -> 'Z', '0' -> '9'))
        apply(p, "B") should equal ('B')
        apply(p, "A") should equal ('A')
        apply(p, "Z") should equal ('Z')
        apply(p, "5") should equal ('5')
        apply(p, "0") should equal ('0')
        apply(p, "9") should equal ('9')
        evaluating (apply(p, "a")) should produce[IllegalArgumentException]
        evaluating (apply(p, "z")) should produce[IllegalArgumentException]
        evaluating (apply(p, "<")) should produce[IllegalArgumentException]
    }

}