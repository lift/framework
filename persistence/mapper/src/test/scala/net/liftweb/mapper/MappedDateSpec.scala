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
object MappedDateSpec extends Specification  {
  "MappedDate Specification".title

  "MappedDate" should {
    "handle a Number in setFromAny" in {
      val dog = Dog2.create
      val currentDate = new java.util.Date()
      dog.createdTime.setFromAny(BigInt(currentDate.getTime))
      dog.createdTime.get mustEqual currentDate
    }

    "handle a full Box in setFromAny" in {
      val dog = Dog2.create
      val someDate = new java.util.Date(1000)
      dog.createdTime.setFromAny(Full(someDate))
      dog.createdTime.get mustEqual someDate
    }

    "handle en empty Box in setFromAny" in {
      val dog = Dog2.create
      dog.createdTime.setFromAny(Empty)
      dog.createdTime.get must beNull
    }
  }
}

