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
package mapper {

import _root_.org.specs._
import _root_.org.specs.runner.JUnit3
import _root_.org.specs.runner.ConsoleRunner
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._

import Helpers._

class MappedLongForeignKeySpecAsTest extends JUnit3(MappedLongForeignKeySpec)
object MappedLongForeignKeySpecRunner extends ConsoleRunner(MappedLongForeignKeySpec)

object MappedLongForeignKeySpec extends Specification {
  "MappedLongForeignKey" should {
    "Not allow comparison to another FK" in {
      val dog = Dog.create.name("Froo").saveMe
      val user = {
        def ret: User = {
          val r = User.create.saveMe
          if (r.id.is >= dog.id.is) r
          else ret
        }

        ret
      }
      dog.owner(user).save
      val d2 = Dog.find(dog.id).open_!
      d2.id.is must_== user.id.is
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
      
      dog.owner.obj mustBe Empty
      dog.owner.is mustBe 0L
    }
  }
}
}
}
