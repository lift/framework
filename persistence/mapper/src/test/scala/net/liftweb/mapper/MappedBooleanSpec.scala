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
package mapper

import org.specs2.mutable.Specification

import common._


/**
 * Systems under specification for MappedDate.
 */
object MappedBooleanSpec extends Specification  {
  "MappedBoolean Specification".title
  sequential

  val provider = DbProviders.H2MemoryProvider
  
  private def ignoreLogger(f: => AnyRef): Unit = ()
  def setupDB {
    provider.setupDB
    Schemifier.destroyTables_!!(ignoreLogger _,  Dog2, User)
    Schemifier.schemify(true, ignoreLogger _, Dog2, User)
  }

  "MappedBoolean" should {
    "not be marked dirty on read" in {
      setupDB
      val charlie = Dog2.create
      charlie.isDog(true).save

      val read = Dog2.find(charlie.dog2id)
      read.map(_.dirty_?) must_== Full(false)
    }

    "be marked dirty on update if value has changed" in {
      setupDB
      val charlie = Dog2.create
      charlie.save

      val read = Dog2.find(charlie.dog2id).openOrThrowException("This is a test")
      read.dirty_? must_== false
      read.isDog(false)
      read.dirty_? must_== false

      read.isDog(true)
      read.isDog(true)
      read.dirty_? must_== true
    }
  }
}

