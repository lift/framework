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

import _root_.java.util.regex.Pattern
import _root_.scala.collection.mutable.ListBuffer
import common._

/**
 * A regular expressions helper library
 * RE("foo") =~ "A string" -or-
 * "A string" =~: RE("foo") -or-
 * "A String".substring(RE("Str"))
 * ("A B cat D" =~: RE("([A-Z])")).capture // List(A,B,D)
 */
object RE {
  /**
   * Create a regular expression from a String
   */
  def apply(in: String) = new REDoer(in, Empty)
  def apply[T](in: String, func: PartialFunction[(T, List[String]), T]) = new REDoer(in, Full(func))

  implicit def matchResToBoolean(in: REMatcher): Boolean = {
    in match {
      case null => false
      case _ => in.matches
    }
  }

  class SuperString(val str: String) {
    def substring(re: REDoer[_]) = re.=~(str).matchStr
  }

  implicit def strToSuperStr(in: String): SuperString = new SuperString(in)
  implicit def strToRe(in: String): REDoer[Nothing] = new REDoer(in, Empty)
}

class REDoer[T](val pattern: String, val func: Box[PartialFunction[(T, List[String]), T]]) extends Function2[T, String, Box[T]] {
  val compiled = Pattern.compile(pattern)

  def =~(other: String) = {
    new REMatcher(other, compiled)
  }

  def =~:(other: String) = {
    new REMatcher(other, compiled)
  }

  def apply(obj: T, other: String): Box[T] = {
    val ma = new REMatcher(other, compiled)
    if (!ma.matches) Empty
    else func.flatMap(f => if (f.isDefinedAt((obj, ma.capture))) Full(f((obj, ma.capture))) else Empty)
  }
}

object REMatcher {
  def unapply(in: REMatcher): Option[List[String]] = Some(in.capture)
}

/**
 * This class adds higher-order functions and lazy evaluation
 * for pattern matching on top of the standard Java regular expressions
 * library.
 *
 * @param str the String in which to perform pattern matching
 * @param compiled the java.util.regex.Pattern to use to perform matches
 */
class REMatcher(val str: String, val compiled: Pattern) {
  private val matcher = compiled.matcher(str)

  /**
   * Matches for the pattern in the specified string.
   */
  lazy val matches = matcher.find

  /**
   * A Full Box containing the substring of this REMatcher's string containing the matches
   * for the specified pattern, or Empty if no match exists.
   */
  lazy val matchStr: Box[String] =
    if (matches) Full(str.substring(matcher.start, matcher.end))
    else Empty

  /**
   * Cached version of the matched groups in this matcher's string.
   */
  lazy val capture = map(s => s)

  /**
   * Call the specified function for each match with each match
   * and the list of all matched groups, then with any remaining data
   * to the end of the string.
   */
  def foreach(func: (String, List[String]) => Unit): Unit = {
     var pos = 0
     matcher.reset
     val m = matcher
     while (matcher.find) {
       func(str.substring(pos, m.start), (0 to m.groupCount).toList.map(i => m.group(i)))
       pos = matcher.end
     }

     func(str.substring(pos), Nil)
  }

  /**
   * Map the specified function over the matches.
   */
  def map[T](f : (String) => T): List[T] = synchronized {
    val ab = new ListBuffer[T]
    matcher.reset
    val cnt = matcher.groupCount

    def doIt {
      def runIt(pos: Int) {
        if (pos >= cnt) return
        else {ab += f(matcher.group(pos + 1)) ; runIt(pos + 1)}
      }

      if (!matcher.find) return
      else {runIt(0) ; doIt}
    }

    doIt

    ab.toList
  }

  /**
   * Return the list of lists of subgroups of matches.
   */
  def eachFound: List[List[String]] = {
    val ret = new ListBuffer[List[String]]
    matcher.reset

    while (matcher.find) {
      ret +=  (0 to matcher.groupCount).toList.map(i => matcher.group(i))
    }

    ret.toList
  }
}

}
}
