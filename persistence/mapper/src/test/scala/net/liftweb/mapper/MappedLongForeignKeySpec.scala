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
import util._


/**
 * Systems under specification for MappedLongForeignKey.
 */
object MappedLongForeignKeySpec extends Specification with org.specs2.specification.BeforeExample {
  "MappedLongForeignKey Specification".title
  sequential

  // Make sure we have everything configured first
  MapperSpecsModel.setup()

  def provider = DbProviders.H2MemoryProvider

  def before = MapperSpecsModel.cleanup()

  "MappedLongForeignKey" should {
      (try {
        provider.setupDB
      } catch {
        case e if !provider.required_? => 1 must be_==(2).orSkip("Provider %s not available: %s".format(provider, e))
      }) must not(throwA[Exception]).orSkip

    "Not allow comparison to another FK" in {
      val dog = Dog.create.name("Froo").saveMe
      val user = {
        def ret: User = {
          val r = User.create.saveMe
          if (r.id.get >= dog.id.get) r
          else ret
        }

        ret
      }
      dog.owner(user).save
      val d2 = Dog.find(dog.id).openOrThrowException("Test")
      d2.id.get must_== user.id.get
      (d2.owner == user) must_== true
      (d2.owner == d2) must_== false
    }

    "be primed after setting a reference" in {
      val dog = Dog.create
      val user = User.create
      dog.owner(user)
      dog.owner.obj.isDefined must beTrue
    }
    
    "be primed after setting a Boxed reference" in {
      val dog = Dog.create
      val user = User.create
      dog.owner(Full(user))
      dog.owner.obj.isDefined must beTrue
    }
    
    "be empty after setting an Empty" in {
      val user = User.create
      val dog = Dog.create.owner(user)
      dog.owner(Empty)
      
      dog.owner.obj must_== Empty
      dog.owner.get must_== 0L
    }
  }
}

