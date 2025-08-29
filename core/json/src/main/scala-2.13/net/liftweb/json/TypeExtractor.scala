/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
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
package json

import scala.reflect.Manifest
import net.liftweb.json.Extraction.TypeExtractor

object TypeExtractor {
  /** Scala 2.13 implementation using Manifest - provides full type information including generics. */
  implicit def fromManifest[A](implicit mf: Manifest[A]): TypeExtractor[A] = new TypeExtractor[A] {
    def runtimeClass: Class[_] = mf.runtimeClass
    def typeArguments: List[TypeExtractor[_]] = mf.typeArguments.map(arg => fromManifest(arg))
  }
}