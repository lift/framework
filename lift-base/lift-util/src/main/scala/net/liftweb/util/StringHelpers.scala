/*
 * Copyright 2006-2010 WorldWide Conferencing, LLC
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

import _root_.java.util.regex._
import _root_.java.lang.Character._
import _root_.java.lang.{StringBuilder => GoodSB}
import _root_.scala.xml.NodeSeq
import common._

object StringHelpers extends StringHelpers

/**
 * Utility methods for manipulating strings.
 */
trait StringHelpers {

  /** random numbers generator */
  private val random = new _root_.java.security.SecureRandom

  /**
   * If str is surrounded by quotes it return the content between the quotes
   */
  def unquote(str: String) = {
    if (str != null && str.length >= 2 && str.charAt(0) == '\"' && str.charAt(str.length - 1) == '\"')
      str.substring(1, str.length - 1)
    else
      str
  }

  /**
   * Splits a string of the form &lt;name1=value1, name2=value2, ... &gt; and unquotes the quoted values.
   * The result is a Map[String, String]
   */
  def splitNameValuePairs(props: String): Map[String, String] = {
    val list = props.split(",").toList.map(in => {
      val pair = in.roboSplit("=")
       (pair(0), unquote(pair(1)))
    })
    val map: Map[String, String] = Map.empty

    (map /: list)((m, next) => m + (next))
  }

  /**
   * Replaces the value found in a string surrounded by &lt;%= ... %&gt; by a replacement according to the value found in the subst Map.<p/>
   * Throws an exception if no correspondance can be found.
   *
   * @param msg string where replacements should be done
   * @param subst map of [regular expression with groups, replacement]
   */
  def processString(msg: String, subst: Map[String, String]): String = {
    val pattern = Pattern.compile("\\<\\%\\=([^\\%]*)\\%\\>")
    val m = pattern.matcher(msg)
    val ret = new StringBuffer
    while (m.find) {
      m.appendReplacement(ret, subst(m.group(1).trim))
    }
    m.appendTail(ret)
    ret.toString
  }
  
 /**
   * Turn a string of format "FooBar" into snake case "foo_bar"
   * 
   * Note: snakify is not reversible, ie. in general the following will _not_ be true:
   * 
   * s == camelify(snakify(s))
   * 
   * @return the underscored string
   */
  def snakify(name : String) = name.replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2").replaceAll("([a-z\\d])([A-Z])", "$1_$2").toLowerCase

  
  /**
   * Turns a string of format "foo_bar" into camel case "FooBar"
   *
   * Functional code courtesy of Jamie Webb (j@jmawebb.cjb.net) 2006/11/28
   * @param name the String to CamelCase
   *
   * @return the CamelCased string
   */
  def camelify(name : String): String = {
    def loop(x : List[Char]): List[Char] = (x: @unchecked) match {
      case '_' :: '_' :: rest => loop('_' :: rest)
      case '_' :: c :: rest => Character.toUpperCase(c) :: loop(rest)
      case '_' :: Nil => Nil
      case c :: rest => c :: loop(rest)
      case Nil => Nil
    }
    if (name == null)
    ""
    else
    loop('_' :: name.toList).mkString
  }

  /**
   * Turn a string of format "foo_bar" into camel case with the first letter in lower case: "fooBar"
   * This function is especially used to camelCase method names.
   *
   * @param name the String to CamelCase
   *
   * @return the CamelCased string
   */
  def camelifyMethod(name: String): String = {
    val tmp: String = camelify(name)
    if (tmp.length == 0)
    ""
    else
    tmp.substring(0,1).toLowerCase + tmp.substring(1)
  }
  /**
   * Capitalize every "word" in the string. A word is either separated by spaces or underscores.
   * @param in string to capify
   * @return the capified string
   */
  def capify(in: String): String = {
    val tmp = ((in match {
      case null => ""
      case s => s
    }).trim match {
      case "" => "n/a"
      case s => s
    }).toLowerCase
    val sb = new GoodSB
    capify(tmp, 0, 250, false, false, sb)
    sb.toString
  }

  /**
   * Replaces the groups found in the msg string with a replacement according to the value found in the subst Map
   * @param msg string where replacements should be done
   * @param subst map of [regular expression with groups, replacement]
   */
  private def capify(in: String, pos: Int, max: Int, lastLetter: Boolean, lastSymbol: Boolean, out: GoodSB): Unit = {
    if (pos >= max || pos >= in.length) return
    else {
      in.charAt(pos) match {
        case c if Character.isDigit(c) => out.append(c); capify(in, pos + 1, max, false, false, out)
        case c if Character.isLetter(c) => out.append(if (lastLetter) c else Character.toUpperCase(c)) ; capify(in, pos + 1, max, true, false, out)
        case c if (c == ' ' || c == '_') && !lastSymbol => out.append(c) ; capify(in, pos + 1, max, false, true, out)
        case _ => capify(in, pos + 1, max, false, true, out)
      }
    }
  }

  /**
   * Remove all the characters from a string exception a-z, A-Z, 0-9, and '_'
   * @return the cleaned string and an empty string if the input is null
   */
  def clean(in : String) =  if (in == null) "" else in.replaceAll("[^a-zA-Z0-9_]", "")

  /**
   * Create a random string of a given size
   * @param size size of the string to create. Must be a positive or nul integer
   * @return the generated string
   */
  def randomString(size: Int): String = {
    def addChar(pos: Int, lastRand: Int, sb: GoodSB): GoodSB = {
      if (pos >= size) sb
      else {
        val randNum = if ((pos % 6) == 0) random.nextInt else lastRand
        sb.append((randNum & 0x1f) match {
          case n if n < 26 => ('A' + n).toChar
          case n => ('0' + (n - 26)).toChar
        })
        addChar(pos + 1, randNum >> 5, sb)
      }
    }
    addChar(0, 0, new GoodSB(size)).toString
  }

  /**
   * Create the unicode value of a character
   * @param in character
   * @return the unicode value as a string starting by \\u
   */
  def escChar(in: Char): String = {
    val ret = Integer.toString(in.toInt, 16)
    "\\u"+("0000".substring(ret.length)) + ret
  }

  /**
   * Split a string separated by a point or by a column in 2 parts. Uses default values if only one is found or if no parts are found
   * @param in string to split
   * @param first default value for the first part if no split can be done
   * @param second default value for the second part if one or less parts can be found
   * @return a pair containing the first and second parts
   */
  def splitColonPair(in: String, first: String, second: String): (String, String) = {
    (in match {
      case null => List("")
      case s if s.indexOf(".") != -1 => s.roboSplit("\\.")
      case s => s.roboSplit(":")
    }) match {
      case f :: s :: _ => (f,s)
      case f :: Nil => (f, second)
      case _ => (first, second)
    }
  }

  /** @return an Empty can if the node seq is empty and a full can with the NodeSeq text otherwise */
  implicit def nodeSeqToOptionString(in: NodeSeq): Box[String] = if (in.length == 0) Empty else Full(in.text)

  /**
   * Parse a string and return the Long value of that string.<p/>
   * The string can start with '-' if it is a negative number or '+' for a positive number
   * @return the Long value of the input String
   */
  def parseNumber(tin: String): Long = {
    def cToL(in: Char) = in.toLong - '0'.toLong
    def p(in: List[Char]) = in.takeWhile(isDigit).foldLeft(0L)((acc,c) => (acc * 10L) + cToL(c))

    if (tin eq null) 0L
    else {
      tin.trim.toList match {
        case '-' :: xs => -p(xs)
        case '+' :: xs => p(xs)
        case xs => p(xs)
      }
    }
  }

  /** @return a SuperString with more available methods such as roboSplit or commafy */
  implicit def stringToSuper(in: String): SuperString = new SuperString(in)

  /**
   * The SuperString class adds functionalities to the String class
   */
  class SuperString(val what: String) {

    /**
     * Split a string according to a separator
     * @param sep a regexp to use with the String::split method
     * @return a list of trimmed parts whose length is &gt; 0
     */
    def roboSplit(sep: String): List[String] = what match {case null => Nil case s => s.split(sep).toList.map(_.trim).filter(_.length > 0)}

    /**
     * Faster than roboSplit... this method splits Strings at a given
     * character
     */
    def charSplit(sep: Char): List[String] = what match {
      case null => Nil
      case str => {
        val ret = new scala.collection.mutable.ListBuffer[String]

        val len = str.length
        var pos = 0
        var lastPos = 0
        
        while (pos < len) {
          if (str.charAt(pos) == sep) {
            if (pos > lastPos) {
              val ns = str.substring(lastPos, pos)
              ret += ns
            }

            lastPos = pos + 1
          }
          pos += 1
        }

        if (pos > lastPos) {
          ret += str.substring(lastPos, pos)
        }

        ret.toList
      }
    }

    /**
     * Split a string in 2 parts at the first place where a separator is found
     * @return a List containing a pair of the 2 trimmed parts
     */
    def splitAt(sep: String): List[(String, String)] = {
      if (what == null)
        return Nil
      else
        what.indexOf(sep) match {
          case -1 => Nil
          case n => List((what.substring(0, n).trim, what.substring(n + sep.length).trim))
        }
    }

    /**
     * Encode the string to be including in JavaScript, replacing '\' or '\\' or non-ASCII characters by their unicode value
     * @return the encoded string inserted into quotes
     */
    def encJs: String = {
      if (what eq null) "null"
      else {
        val len = what.length
        val sb = new GoodSB(len * 2)
        sb.append('"')
        var pos = 0
        while (pos < len) {
          what.charAt(pos) match {
            case c @ ('\\' | '\'') => sb.append(escChar(c))
            case '"' => sb.append("\\\"")
            case c if c < ' ' || c > '~' => sb.append(escChar(c))
            case c => sb.append(c)
          }
          pos += 1
        }
        sb.append('"')
        sb.toString
      }
    }

    /**
     * Add commas before the last 3 characters
     * @return the string with commas
     */
    def commafy: String = {
      if (what eq null) null
      else {
        val toDo = what.toList.reverse

        def commaIt(in: List[Char]): List[Char] = in match {
          case Nil => in
          case x :: Nil => in
          case x1 :: x2 :: Nil => in
          case x1 :: x2 :: x3 :: Nil => in
          case x1 :: x2 :: x3 :: xs => x1 :: x2 :: x3 :: ',' :: commaIt(xs)
        }
        commaIt(toDo).reverse.mkString("")
      }
    }
  }

  /**
   * Test for null and return either the given String if not null or the empty String.
   */
  def emptyForNull(s: String) = if (s != null) s else ""
}

}
}
