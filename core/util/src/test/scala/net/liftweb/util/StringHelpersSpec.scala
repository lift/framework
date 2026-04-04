/*
 * Copyright 2007-2026 Lift Committers and Contributors
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

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.Gen._
import org.scalacheck.Prop.forAll

import StringHelpers._


/**
 * Systems under specification for StringHelpers.
 */
class StringHelpersSpec extends Specification with ScalaCheck with StringGen {
  "StringHelpers Specification".title

  "The snakify function" should {
    "replace upper case with underscore" in {
      snakify("MyCamelCase") === "my_camel_case"
      snakify("CamelCase") === "camel_case"
      snakify("Camel") === "camel"
      snakify("MyCamel12Case") === "my_camel12_case"
      snakify("CamelCase12") === "camel_case12"
      snakify("Camel12") === "camel12"
    }

    "not modify existing snake case strings" in {
      snakify("my_snake_case") === "my_snake_case"
      snakify("snake") === "snake"
    }

    "handle abbeviations" in {
      snakify("HTML") === "html"
      snakify("HTMLEditor") === "html_editor"
      snakify("EditorTOC") === "editor_toc"
      snakify("HTMLEditorTOC") === "html_editor_toc"

      snakify("HTML5") === "html5"
      snakify("HTML5Editor") === "html5_editor"
      snakify("Editor2TOC") === "editor2_toc"
      snakify("HTML5Editor2TOC") === "html5_editor2_toc"
    }
  }

  "The camelify function" should {
    "CamelCase a name which is underscored, removing each underscore and capitalizing the next letter" in {
      def previousCharacterIsUnderscore(name: String, i: Int) = i > 1 && name.charAt(i - 1) == '_'
      def underscoresNumber(name: String, i: Int) = if (i == 0) 0 else name.substring(0, i).toList.count(_ == '_')
      def correspondingIndexInCamelCase(name: String, i: Int) = i - underscoresNumber(name, i)
      def correspondingCharInCamelCase(name: String, i: Int): Char = camelify(name).charAt(correspondingIndexInCamelCase(name, i))

      val doesntContainUnderscores = forAll(underscoredStrings){ ((name: String) => !camelify(name).contains("_")) }
      val isCamelCased = forAll(underscoredStrings) ((name: String) => {
        name.forall(_ == '_') && camelify(name).isEmpty ||
        name.toList.zipWithIndex.forall { case (c, i) =>
          c == '_' ||
          correspondingIndexInCamelCase(name, i) == 0 && correspondingCharInCamelCase(name, i) == c.toUpper ||
          !previousCharacterIsUnderscore(name, i) && correspondingCharInCamelCase(name, i) == c ||
          previousCharacterIsUnderscore(name, i) && correspondingCharInCamelCase(name, i) == c.toUpper
       }
      })
      (doesntContainUnderscores && isCamelCased)
    }
    "return an empty string if given null" in {
      camelify(null) === ""
    }
    "leave a CamelCased name untouched" in {
      forAll(camelCasedStrings){ (name: String) => camelify(name) == name }
    }
  }

  "The camelifyMethod function" should {
    "camelCase a name with the first letter being lower cased" in {
      forAll(underscoredStrings){
        (name: String) =>
        camelify(name).isEmpty && camelifyMethod(name).isEmpty ||
        camelifyMethod(name).toList.head.isLower && camelify(name) == camelifyMethod(name).capitalize
      }
    }
  }

  "SuperListString" should {
    """allow "foo" / "bar" """ in {
      ("foo" / "bar") === List("foo", "bar")
    }

    """allow "foo" / "bar" / "baz" """ in {
      ("foo" / "bar" / "baz") === List("foo", "bar", "baz")
    }
  }

  "The StringHelpers processString function" should {
    "replace groups found in a string surrounded by <%= ... %> by their corresponding value in a map" in {
      processString("<%=hello%>", Map("hello" -> "bonjour")) === "bonjour"
    }
    "replace groups found in several strings surrounded by <%= ... %> by their corresponding value in a map" in {
      processString("<%=hello%> <%=world%>", Map("hello" -> "bonjour", "world" -> "monde")) === "bonjour monde"
    }
    "not replace the group if it starts with %" in {
      processString("<%=%hello%>", Map("hello" -> "bonjour")) === "<%=%hello%>"
    }
    "throw an exception if no correspondance is found" in {
      processString("<%=hello%>", Map("hallo" -> "bonjour")) must throwA[Exception]
    }
  }

  "The StringHelpers capify function" should {
    "capitalize a word" in {
      capify("hello") === "Hello"
    }
    "capitalize the first letters of 2 words" in {
      capify("hello world") === "Hello World"
    }
    "capitalize the first letters of 2 words separated by an underscore" in {
      capify("hello_world") === "Hello_World"
    }
    "capitalize the second character in a word if the first is a digit" in {
      capify("hello 1world") === "Hello 1World"
    }
    "capitalize the third character in a word if the first 2 are digits" in {
      capify("hello 12world") === "Hello 12World"
    }
    "suppress symbols placed in front of words and capitalize the words" in {
      capify("@hello $world") === "Hello World"
    }
    "remove letters in the word if there are more than 250 digits in front of it" in {
      capify("1" * 250 + "hello") === "1" * 250
    }
  }
  "The StringHelpers clean function" should {
    "return an empty string if the input is null" in {
      clean(null) === ""
    }
    "remove every character which is not a-zA-Z0-9_" in {
      clean(" He@llo Wor+ld_!") === "HelloWorld_"
    }
  }
  "The StringHelpers randomString" should {
    "return an empty string if size is 0" in {
      randomString(0) === ""
    }
    /*
    "return a string of size n" in {
      randomString(10).toSeq must haveSize(10)
    }
    */

    "return only capital letters and digits" in {
      randomString(10) must beMatching("[A-Z0-9]*")
    }
  }
  "The StringHelpers escChar" should {
    "return the unicode value of a character as a string starting with \\u" in {
      escChar('{') === "\\u007b"
      escChar('A') === "\\u0041"
    }
  }
  "The StringHelpers splitColonPair" should {
    "split a string separated by a dot in 2 parts" in {
      splitColonPair("a.b", "1", "2") === ("a", "b")
    }
    "split a string separated by a dot in 2 trimmed parts" in {
      splitColonPair(" a . b ", "1", "2") === ("a", "b")
    }
    "split a string separated by a column in 2 parts" in {
      splitColonPair("a:b", "1", "2") === ("a", "b")
    }
    "split a string according to the dot prioritarily" in {
      splitColonPair("a.b:c", "1", "2") === ("a", "b:c")
    }
    "split a string according to first the dot if there are several ones and removing everything after the next dot" in {
      splitColonPair("a.b.c", "1", "2") === ("a", "b")
      splitColonPair("a.b.c.d", "1", "2") === ("a", "b")
    }
    "use a default value for the second part if there is nothing to split" in {
      splitColonPair("abc", "1", "2") === ("abc", "2")
    }
    "use null for the first part if the input string is null" in {
      splitColonPair(null, "1", "2") === ("", "2")
    }
    "use a default value for the first part if the input string is empty" in {
      splitColonPair("", "1", "2") === ("1", "2")
    }
  }
  "The StringHelpers parseNumber function" should {
    "return a Long corresponding to the value of a string with digits" in {
      parseNumber("1234") === 1234L
    }
    "return a positive Long value if the string starts with +" in {
      parseNumber("+1234") === 1234L
    }
    "return a positive Long value if the string starts with -" in {
      parseNumber("-1234") === -1234L
    }
    "return 0 if the string is null" in {
      parseNumber(null) === 0L
    }
    "return 0 if the string is empty" in {
      parseNumber("") === 0L
    }
    "return 0 if the string can't be parsed" in {
      parseNumber("string") === 0L
    }
  }
  "The SuperString class roboSplit method" should {
    "split a string according to a separator" in {
      "hello".roboSplit("ll") === List("he", "o")
    }
    "split a string according to a separator - 2" in {
      "hAeAlAlAo".roboSplit("A") === List("h", "e", "l", "l", "o")
    }
    "split a string into trimmed parts" in {
      "hello . world ".roboSplit("\\.") === List("hello", "world")
    }
    "split a string into trimmed parts whose length is > 0" in {
      "hello . . world ".roboSplit("\\.") === List("hello", "world")
    }
    "split a string according to a separator. The separator is always interpreted as a regular expression" in {
      "hello".roboSplit("(l)+") === List("he", "o")
    }
    "return a list containing the string if the separator is not found" in {
      "hello".roboSplit("tt") === List("hello")
    }
    "return an empty list if the string is null" in {
      (null: String).roboSplit("a") === List()
    }
    "return a list containing the string if the string is empty" in {
      "".roboSplit("a") === List()
    }
  }

  "The SuperString class charSplit method" should {
    "split a string according to a separator" in {
      "hello".charSplit('l') === List("he", "o")
    }
    "split a string according to a separator - 2" in {
      "hAeAlAlAo".charSplit('A') === List("h", "e", "l", "l", "o")
    }
    "split a string" in {
      "hello . world ".charSplit('.') === List("hello ", " world ")
    }
    "split a string into parts" in {
      "hello .. world ".charSplit('.') === List("hello ", " world ")
    }
    "return an empty list if the string is null" in {
      (null: String).charSplit('a') === List()
    }
    "return a list containing the string if the string is empty" in {
      "".charSplit('a') === List()
    }
  }


  "The SuperString class splitAt method" should {
    "split a string according to a separator and return a List containing a pair with the 2 parts" in {
      stringToSuper("hello").splitAt("ll") === List(("he", "o"))
    }
    "split a string according to a separator and return an empty List if no separator is found" in {
      stringToSuper("hello").splitAt("tt") === Nil
    }
    "split a string according to a separator and return an empty List if the string is null" in {
      stringToSuper(null: String).splitAt("tt") === Nil
    }
  }
  "The SuperString class encJs method" should {
    "encode a string replacing backslash with its unicode value" in {
      "\\hello".encJs === "\"\\u005chello\""
    }
    "encode a string replacing the quote character with its unicode value" in {
      "h'ello".encJs === "\"h\\u0027ello\""
    }
    "encode a string adding a quote before and a quote after the string" in {
      "hello".encJs === "\"hello\""
    }
    "encode a string replacing non-ASCII characters by their unicode value" in {
      "ni\u00f1a".encJs === "\"ni\\u00f1a\""
    }
    "return the string \"null\" if the input string is null" in {
      (null: String).encJs === "null"
    }
  }
  "The SuperString class commafy method" should {
    "add a comma before the last 3 characters of the input string" in {
      "hello".commafy === "he,llo"
    }
    "add nothing if the input string is less than 4 characters" in {
      "hel".commafy === "hel"
    }
    "return null if the input string is null" in {
      (null: String).commafy must beNull
    }
  }
  "The blankForNull method" should {
    "return the empty String for a given null String" in {
      StringHelpers.blankForNull(null) === ""
    }
    "return the given String for a given not-null String" in {
      StringHelpers.blankForNull("x") === "x"
    }
  }
  "The emptyForBlank method" should {
    import net.liftweb.common._
    "return Empty for a given null String" in {
      StringHelpers.emptyForBlank(null) must beEqualTo(Empty)
    }
    "return Empty for a given a blank String" in {
      StringHelpers.emptyForBlank("") must beEqualTo(Empty)
    }
    "return Empty for a String of spaces" in {
      StringHelpers.emptyForBlank("  ") must beEqualTo(Empty)
    }
    "return the trim'ed  String for a given not-null String" in {
      StringHelpers.emptyForBlank(" x ") must beEqualTo(Full("x"))
    }
  }
}


trait StringGen {
  val underscoredStrings =
    for {
      length <- choose(0, 4)
      s <- listOfN(length, frequency((3, alphaChar), (1, oneOf(List('_')))))
    } yield s.mkString

  val camelCasedStrings =
    for {
      length <- choose(0, 4)
      firstLetter <- alphaNumChar.map(_.toUpper)
      string <- listOfN(length, frequency((3, alphaNumChar.map(_.toLower)),
                                          (1, alphaNumChar.map(_.toUpper))))
  } yield (firstLetter :: string).mkString
}

