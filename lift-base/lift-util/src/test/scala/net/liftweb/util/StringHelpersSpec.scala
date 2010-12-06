/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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

package net.liftweb {
package util {

import _root_.org.specs.runner._
import _root_.org.specs._
import _root_.java.lang.reflect.{Method}
import _root_.org.scalacheck.Arbitrary
import _root_.org.scalacheck.{Prop, Gen}
import Gen._
import _root_.org.specs.ScalaCheck
import common._

object StringHelpersSpec extends Specification with StringHelpers with StringGenerators with ScalaCheck {
  "The snakify function" should {
    "replace upper case with underscore" in {
      snakify("MyCamelCase") must_== "my_camel_case"
      snakify("CamelCase") must_== "camel_case"
      snakify("Camel") must_== "camel"
      snakify("MyCamel12Case") must_== "my_camel12_case"
      snakify("CamelCase12") must_== "camel_case12"
      snakify("Camel12") must_== "camel12"
    }

    "not modify existing snake case strings" in {
      snakify("my_snake_case") must_== "my_snake_case"
      snakify("snake") must_== "snake"
    }

    "handle abbeviations" in {
      snakify("HTML") must_== "html"
      snakify("HTMLEditor") must_== "html_editor"
      snakify("EditorTOC") must_== "editor_toc"
      snakify("HTMLEditorTOC") must_== "html_editor_toc"

      snakify("HTML5") must_== "html5"
      snakify("HTML5Editor") must_== "html5_editor"
      snakify("Editor2TOC") must_== "editor2_toc"
      snakify("HTML5Editor2TOC") must_== "html5_editor2_toc"
    }
  }

  "The camelify function" should {
    "CamelCase a name which is underscored, removing each underscore and capitalizing the next letter" in {
      def previousCharacterIsUnderscore(name: String, i: Int) = i > 1 && name.charAt(i - 1) == '_'
      def underscoresNumber(name: String, i: Int) = if (i == 0) 0 else name.substring(0, i).toList.count(_ == '_')
      def correspondingIndexInCamelCase(name: String, i: Int) = i - underscoresNumber(name, i)
      def correspondingCharInCamelCase(name: String, i: Int): Char = camelify(name).charAt(correspondingIndexInCamelCase(name, i))

      val doesntContainUnderscores = forAllProp(underscoredStrings)((name: String) => !camelify(name).contains("_"))
      val isCamelCased = forAllProp(underscoredStrings) ((name: String) => {
        name.forall(_ == '_') && camelify(name).isEmpty ||
        name.toList.zipWithIndex.forall { case (c, i) =>
          c == '_' ||
          correspondingIndexInCamelCase(name, i) == 0 && correspondingCharInCamelCase(name, i) == c.toUpperCase ||
          !previousCharacterIsUnderscore(name, i) && correspondingCharInCamelCase(name, i) == c ||
          previousCharacterIsUnderscore(name, i) && correspondingCharInCamelCase(name, i) == c.toUpperCase
       }
      })
      doesntContainUnderscores && isCamelCased must pass
    }
    "return an empty string if given null" in {
      camelify(null) must_== ""
    }
    "leave a CamelCased name untouched" in {
      val camelCasedNameDoesntChange = forAllProp(camelCasedStrings){ (name: String) => camelify(name) == name }
      camelCasedNameDoesntChange must pass
    }
  }

  "The camelifyMethod function" should {
    "camelCase a name with the first letter being lower cased" in {
      val camelCasedMethodIsCamelCaseWithLowerCase = forAllProp(underscoredStrings){
        (name: String) =>
        camelify(name).isEmpty && camelifyMethod(name).isEmpty ||
        camelifyMethod(name).toList.head.isLowerCase && camelify(name) == camelifyMethod(name).capitalize
      }
      camelCasedMethodIsCamelCaseWithLowerCase must pass
    }
  }

  "SuperListString" should {
    """allow "foo" / "bar" """ in {
      ("foo" / "bar") must_== List("foo", "bar")
    }

    """allow "foo" / "bar" / "baz" """ in {
      ("foo" / "bar" / "baz") must_== List("foo", "bar", "baz")
    }
  }

  "The StringHelpers processString function" should {
    "replace groups found in a string surrounded by <%= ... %> by their corresponding value in a map" in {
      processString("<%=hello%>", Map("hello" -> "bonjour")) must_== "bonjour"
    }
    "replace groups found in several strings surrounded by <%= ... %> by their corresponding value in a map" in {
      processString("<%=hello%> <%=world%>", Map("hello" -> "bonjour", "world" -> "monde")) must_== "bonjour monde"
    }
    "not replace the group if it starts with %" in {
      processString("<%=%hello%>", Map("hello" -> "bonjour")) must_== "<%=%hello%>"
    }
    "throw an exception if no correspondance is found" in {
      processString("<%=hello%>", Map("hallo" -> "bonjour")) must throwA[Exception]
    }
  }

  "The StringHelpers capify function" should {
    "capitalize a word" in {
      capify("hello") must_== "Hello"
    }
    "capitalize the first letters of 2 words" in {
      capify("hello world") must_== "Hello World"
    }
    "capitalize the first letters of 2 words separated by an underscore" in {
      capify("hello_world") must_== "Hello_World"
    }
    "capitalize the second character in a word if the first is a digit" in {
      capify("hello 1world") must_== "Hello 1World"
    }
    "capitalize the third character in a word if the first 2 are digits" in {
      capify("hello 12world") must_== "Hello 12World"
    }
    "suppress symbols placed in front of words and capitalize the words" in {
      capify("@hello $world") must_== "Hello World"
    }
    "remove letters in the word if there are more than 250 digits in front of it" in {
      capify("1" * 250 + "hello") must_== "1" * 250
    }
  }
  "The StringHelpers clean function" should {
    "return an empty string if the input is null" in {
      clean(null) must_== ""
    }
    "remove every character which is not a-zA-Z0-9_" in {
      clean(" He@llo Wor+ld_!") must_== "HelloWorld_"
    }
  }
  "The StringHelpers randomString" should {
    "return an empty string if size is 0" in {
      randomString(0) must_== ""
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
      escChar('{') must_== "\\u007b"
      escChar('A') must_== "\\u0041"
    }
  }
  "The StringHelpers splitColonPair" should {
    "split a string separated by a dot in 2 parts" in {
      splitColonPair("a.b", "1", "2") must_== ("a", "b")
    }
    "split a string separated by a dot in 2 trimmed parts" in {
      splitColonPair(" a . b ", "1", "2") must_== ("a", "b")
    }
    "split a string separated by a column in 2 parts" in {
      splitColonPair("a:b", "1", "2") must_== ("a", "b")
    }
    "split a string according to the dot prioritarily" in {
      splitColonPair("a.b:c", "1", "2") must_== ("a", "b:c")
    }
    "split a string according to first the dot if there are several ones and removing everything after the next dot" in {
      splitColonPair("a.b.c", "1", "2") must_== ("a", "b")
      splitColonPair("a.b.c.d", "1", "2") must_== ("a", "b")
    }
    "use a default value for the second part if there is nothing to split" in {
      splitColonPair("abc", "1", "2") must_== ("abc", "2")
    }
    "use null for the first part if the input string is null" in {
      splitColonPair(null, "1", "2") must_== ("", "2")
    }
    "use a default value for the first part if the input string is empty" in {
      splitColonPair("", "1", "2") must_== ("1", "2")
    }
  }
  "The StringHelpers parseNumber function" should {
    "return a Long corresponding to the value of a string with digits" in {
      parseNumber("1234") must_== 1234L
    }
    "return a positive Long value if the string starts with +" in {
      parseNumber("+1234") must_== 1234L
    }
    "return a positive Long value if the string starts with -" in {
      parseNumber("-1234") must_== -1234L
    }
    "return 0 if the string is null" in {
      parseNumber(null) must_== 0L
    }
    "return 0 if the string is empty" in {
      parseNumber("") must_== 0L
    }
    "return 0 if the string can't be parsed" in {
      parseNumber("string") must_== 0L
    }
  }
  "The SuperString class roboSplit method" should {
    "split a string according to a separator" in {
      "hello".roboSplit("ll") must_== List("he", "o")
    }
    "split a string according to a separator - 2" in {
      "hAeAlAlAo".roboSplit("A") must_== List("h", "e", "l", "l", "o")
    }
    "split a string into trimmed parts" in {
      "hello . world ".roboSplit("\\.") must_== List("hello", "world")
    }
    "split a string into trimmed parts whose length is > 0" in {
      "hello . . world ".roboSplit("\\.") must_== List("hello", "world")
    }
    "split a string according to a separator. The separator is always interpreted as a regular expression" in {
      "hello".roboSplit("(l)+") must_== List("he", "o")
    }
    "return a list containing the string if the separator is not found" in {
      "hello".roboSplit("tt") must_== List("hello")
    }
    "return an empty list if the string is null" in {
      (null: String).roboSplit("a") must_== List()
    }
    "return a list containing the string if the string is empty" in {
      "".roboSplit("a") must_== List()
    }
  }

  "The SuperString class charSplit method" should {
    "split a string according to a separator" in {
      "hello".charSplit('l') must_== List("he", "o")
    }
    "split a string according to a separator - 2" in {
      "hAeAlAlAo".charSplit('A') must_== List("h", "e", "l", "l", "o")
    }
    "split a string" in {
      "hello . world ".charSplit('.') must_== List("hello ", " world ")
    }
    "split a string into parts" in {
      "hello .. world ".charSplit('.') must_== List("hello ", " world ")
    }
    "return an empty list if the string is null" in {
      (null: String).charSplit('a') must_== List()
    }
    "return a list containing the string if the string is empty" in {
      "".charSplit('a') must_== List()
    }
  }


  "The SuperString class splitAt method" should {
    "split a string according to a separator and return a List containing a pair with the 2 parts" in {
      stringToSuper("hello").splitAt("ll") must_== List(("he", "o"))
    }
    "split a string according to a separator and return an empty List if no separator is found" in {
      stringToSuper("hello").splitAt("tt") must_== Nil
    }
    "split a string according to a separator and return an empty List if the string is null" in {
      stringToSuper(null: String).splitAt("tt") must_== Nil
    }
  }
  "The SuperString class encJs method" should {
    "encode a string replacing backslash with its unicode value" in {
      "\\hello".encJs must_== "\"\\u005chello\""
    }
    "encode a string replacing the quote character with its unicode value" in {
      "h'ello".encJs must_== "\"h\\u0027ello\""
    }
    "encode a string adding a quote before and a quote after the string" in {
      "hello".encJs must_== "\"hello\""
    }
    "encode a string replacing non-ASCII characters by their unicode value" in {
      "ni\u00f1a".encJs must_== "\"ni\\u00f1a\""
    }
    "return the string \"null\" if the input string is null" in {
      (null: String).encJs must_== "null"
    }
  }
  "The SuperString class commafy method" should {
    "add a comma before the last 3 characters of the input string" in {
      "hello".commafy must_== "he,llo"
    }
    "add nothing if the input string is less than 4 characters" in {
      "hel".commafy must_== "hel"
    }
    "return null if the input string is null" in {
      (null: String).commafy must beNull
    }
  }
  "The emptyForNull method" should {
    "return the empty String for a given null String" in {
      StringHelpers.emptyForNull(null) must_== ""
    }
    "return the given String for a given not-null String" in {
      StringHelpers.emptyForNull("x") must_== "x"
    }
  }
}
class StringHelpersSpecTest extends JUnit4(StringHelpersSpec)
trait StringGenerators {
  val underscoredStrings = for {length <- choose(0, 4)
                                s <- listOfN(length, frequency((3, alphaChar), (1, Gen.oneOf(List('_')))))
                                } yield List.toString(s)

  val camelCasedStrings = for {length <- choose(0, 4)
         firstLetter <- alphaNumChar.map(_.toUpperCase)
         string <- listOfN(length, frequency((3, alphaNumChar.map(_.toLowerCase)), (1, alphaNumChar.map(_.toUpperCase))))
        } yield List.toString(firstLetter :: string)
}

}
}
