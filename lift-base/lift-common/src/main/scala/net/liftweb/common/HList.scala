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

import _root_.scala.reflect.Manifest
  
  sealed trait HList {
    type ConsType
    def manifest: Manifest[ConsType]

    /**
     * The length of the HList
     */
    def length: Int

  }

  final case class HCons[H, T <: HList](head: H, tail: T)(implicit val manifest: Manifest[H]) extends HList {
    type ConsType = H

    def :+:[T](v : T)(implicit m: Manifest[T]) = HCons(v, this)

    override def toString = head + " :+: "+tail

    def length = 1 + tail.length
  }

  final object HNil extends HList {
    type ConsType = Nothing

    def :+:[T](v : T)(implicit m: Manifest[T]) = HCons(v, this)

    private def implicitly[T](implicit m: Manifest[T]): Manifest[T] = m
    
    val manifest = implicitly[ConsType]
    
    override def toString = "HNil"

    def length = 0
  }

  object HList {
    type :+:[H, T <: HList] = HCons[H, T]
    type HNil = HCons[Nothing, Nothing]
    val :+: = HCons
  }
}
}
// vim: set ts=2 sw=2 et:
