/*
 * Copyright 2011 WorldWide Conferencing, LLC
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

import common.{Empty, Full}
import xml.NodeSeq


/**
 * The util package object
 */
package object util {
  type CssBindFunc = CssSel

  /**
   * Wrap a function and make sure it's a NodeSeq => NodeSeq.  Much easier
   * than explicitly casting the first parameter
   *
   * @param f the function
   * @return a NodeSeq => NodeSeq
   */
  def nsFunc(f: NodeSeq => NodeSeq): NodeSeq => NodeSeq = f

  /**
   * Promote to an IterableConst when implicits won't do it for you
   *
   * @param ic the thing that can be promoted to an IterableConst
   * @param f the implicit function that takes T and makes it an IterableConst
   * @tparam T the type of the parameter
   * @return an IterableConst
   */
  def itConst[T](ic: T)(implicit f: T => IterableConst): IterableConst = f(ic)
}
