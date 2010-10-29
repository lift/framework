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

/**
 * Support for heterogenious lists, aka <a href="http://apocalisp.wordpress.com/2010/07/06/type-level-programming-in-scala-part-6a-heterogeneous-list%C2%A0basics/">HLists</a>
 * 
 */
object HLists {
  /**
   * The trait that defines HLists
   */
  sealed trait HList {
    type Head
    type Tail <: HList

    /**
     * The length of the HList
     */
    def length: Int
  }

  /**
   * The last element of an HList
   */
  final class HNil extends HList {
    type Head = Nothing
    type Tail = HNil
    
    /**
     * Create a new HList based on this node
     */
    def :+:[T](v : T) = HCons(v, this)

    override def toString = "HNil"

    /**
     * The length of the HList
     */
    def length = 0
  }

  /**
   * The HNil singleton
   */
  val HNil = new HNil()

  /**
   * The HList cons cell
   */
  final case class HCons[H, T <: HList](head : H, tail : T) extends HList {
    type This = HCons[H, T]
    type Head = H
    type Tail = T
    
    /**
     * Create a new HList based on this node
     */
    def :+:[T](v : T) = HCons(v, this)

    override def toString = head + " :+: " + tail

    /**
     * The length of the HList
     */
    def length = 1 + tail.length
  }

  type :+:[H, T <: HList] = HCons[H, T]
  object :+: {
    def unapply[H, T <: HList](in: HCons[H, T]): Option[(H, T)] = Some(in.head, in.tail)
    }
  
}

}
}
// vim: set ts=2 sw=2 et:
