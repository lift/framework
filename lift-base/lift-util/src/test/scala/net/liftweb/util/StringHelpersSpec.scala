package net.liftweb.util
import _root_.org.specs._
import _root_.org.specs.runner._
import _root_.org.specs.Sugar._
import common._

object StringHelpersSpec extends Specification with StringHelpers {
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
  "The SuperString class splitAt method" should {
    "split a string according to a separator and return a List containing a pair with the 2 parts" in {
      "hello".splitAt("ll") must_== List(("he", "o"))
    }
    "split a string according to a separator and return an empty List if no separator is found" in {
      "hello".splitAt("tt") must_== Nil
    }
    "split a string according to a separator and return an empty List if the string is null" in {
      (null: String).splitAt("tt") must_== Nil
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
}
class StringHelpersSpecTest extends JUnit4(StringHelpersSpec)
