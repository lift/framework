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
package mapper

import org.specs2.mutable.Specification

import common._


/**
 * Systems under specification for Schemifier.
 */
object SchemifierSpec extends Specification  {
  "Schemifier Specification".title

  val provider = DbProviders.H2MemoryProvider
  
  "Schemifier" should {
    "not crash in readonly if table doesn't exist" in {
      provider.setupDB
      Schemifier.schemify(false, Schemifier.neverF _, Thing)
      success
    }
  }
}

