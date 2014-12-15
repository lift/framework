/*
 * Copyright 2010-2011 WorldWide Conferencing, LLC
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
package common 

import scala.language.implicitConversions
import scala.xml.NodeSeq

/*
 * This file contains common conversions and other utilities to make
 * conversions that are common
 */

/**
 * This trait is used to unify `String`s and `[[scala.xml.NodeSeq NodeSeq]]`s
 * into one type. It is used in conjuction with the implicit conversions defined
 * in its companion object.
 */
sealed trait StringOrNodeSeq {
  def nodeSeq: scala.xml.NodeSeq
}

/**
 * Provides implicit conversions to the `StringOrNodeSeq` trait, which can in
 * turn be implicitly converted to `[[scala.xml.NodeSeq NodeSeq]]`. This allows
 * using a `String` as a natural part of `NodeSeq` APIs without having to
 * explicitly wrap it in `scala.xml.Text` or having to write overloads for all
 * methods that should accept both.
 *
 * This is used in certain Lift APIs, for example, to accept either a `String`
 * or more complex content. For example, a `button` can have either a simple
 * label or complex HTML content. HTML APIs that can do this can accept a
 * parameter of type `StringOrNodeSeq` to allow the user to pass either in as
 * their needs dictate.
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
   * This is written in terms of a `Seq[Node]` to make sure Scala converts
   * everything it should to a `StringOrNodeSeq`. `NodeSeq` is a `Seq[Node]`.`
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
 * This trait is used to unify `()=>String` and `String` into one type. It is
 * used in conjunction with the implicit conversions defined in its companion
 * object.
 */
sealed trait StringFunc {
  def func: () => String
}

/**
 * Provides implicit conversions to the `StringFunc` trait. This allows using a
 * `String` as a natural part of APIs that want to allow the flexibility of a
 * `()=>String` without having to write overloads for all methods that should
 * accept both.
 *
 * Lift's Menu API, for example, allows CSS classes to be defined either as
 * a `String` or a `()=>String`. The latter could use the current request and
 * session state to do more interesting things than a hard-coded `String` would,
 * while the former is simpler to use.
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

/**
 * This trait is used to unify `()=>[[scala.xml.NodeSeq NodeSeq]]` and
 * `[[scala.xml.NodeSeq NodeSeq]]` into one type. It is used in conjunction
 * with the implicit conversions defined in its [[NodeSeqFunc$ companion
 * object]].
 */
sealed trait NodeSeqFunc {
  def func: () => NodeSeq
}

/**
 * Provides implicit conversions to the `NodeSeqFunc` trait. This allows using a
 * `[[scala.xml.NodeSeq NodeSeq]]` as a natural part of APIs that want to allow
 * the flexibility of a `()=>[[scala.xml.NodeSeq NodeSeq]]` without having to
 * write overloads for all methods that should accept both.
 */
object NodeSeqFunc {
  /**
   * If you've got something that can be converted into a NodeSeq (a constant)
   * but want a NodeSeqFunc, this implicit will do the conversion.
   */
  implicit def nsToNodeSeqFunc[T](ns: T)(implicit f: T => NodeSeq): NodeSeqFunc = 
    ConstNodeSeqFunc(f(ns))

  /**
   * If you've got something that can be converted into a String Function
   * but want a StringFunc, this implicit will do the conversion.
   */
  implicit def funcToNodeSeqFunc[T](func: () => T)(implicit f: T => NodeSeq): NodeSeqFunc =
    RealNodeSeqFunc(() => f(func()))
}

/**
 * The case class that holds a `[[scala.xml.NodeSeq NodeSeq]]` function.
 */
final case class RealNodeSeqFunc(func: () => NodeSeq) extends NodeSeqFunc

/**
 * The case class that holds the `[[scala.xml.NodeSeq NodeSeq]]` constant.
 */
final case class ConstNodeSeqFunc(ns: NodeSeq) extends NodeSeqFunc {
  lazy val func = () => ns
}

