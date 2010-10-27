/*
 * Copyright 2010 WorldWide Conferencing, LLC
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
package common {

/*
 * This file contains common conversions and other utilities to make
 * conversions that are common
 */

/**
 * A helpful trait that will accept either a String or a NodeSeq via
 * an implicit conversion.  So, all you need to do is put in a String or
 * a NodeSeq and the right thing will happen.
 */
sealed trait StringOrNodeSeq {
  def nodeSeq: scala.xml.NodeSeq
}

/**
 * The companion object that has helpful
 * implicit conversions from String and NodeSeq
 */
object StringOrNodeSeq {
  import scala.xml._

  /**
   * Convert a String to a StringOrNodeSeq
   */
  implicit def strTo[T <% String](str: T): StringOrNodeSeq = 
    new StringOrNodeSeq {
      def nodeSeq: NodeSeq = Text(str)
    }

  /**
   * Convert a NodeSeq (well, a Seq[Node]) to a StringOrNodeSeq
   */
  implicit def nsTo(ns: Seq[Node]): StringOrNodeSeq = 
    new StringOrNodeSeq {
      def nodeSeq: NodeSeq = ns
    }

  /**
   * Convert a StringOrNodeSeq into a NodeSeq
   */
  implicit def toNodeSeq(sns: StringOrNodeSeq): NodeSeq = sns.nodeSeq
}

/**
 * Sometimes you want a function that returns a String as a parameter,
 * but many times, you'll just want to pass a String constant.  In
 * those cases, this trait and it's implicit conversions come in really
 * handy.  Basically, a String constant or a String function can be passed and
 * either will be implicitly converted into a StringFunc.
 */
sealed trait StringFunc {
  def func: () => String
}

/**
 * The companion object to StringFunc with helpful implicit conversions
 */
object StringFunc {
  /**
   * If you've got something that can be converted into a String (a constant)
   * but want a StringFunc, this implicit will do the conversion.
   */
  implicit def strToStringFunc[T](str: T)(implicit f: T => String): StringFunc = 
    ConstStringFunc(f(str))

  /**
   * If you've got something that can be converted into a String Function
   * but want a StringFunc, this implicit will do the conversion.
   */
  implicit def funcToStringFunc[T](func: () => T)(implicit f: T => String): StringFunc =
    RealStringFunc(() => f(func()))
}

/**
 * The case class that holds a String function.
 */
final case class RealStringFunc(func: () => String) extends StringFunc

/**
 * The case class that holds the String constant.
 */
final case class ConstStringFunc(str: String) extends StringFunc {
  lazy val func = () => str
}

}
}
// vim: set ts=2 sw=2 et:
