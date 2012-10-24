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

object MyEnum extends Enumeration {
  val a = Value
  val b = Value
  val c = Value
  val d = Value
  val e = Value
}
  
class EnumObj extends LongKeyedMapper[EnumObj] with IdPK {
  def getSingleton = EnumObj

  object enum extends MappedEnum(this, MyEnum)
}

object EnumObj extends EnumObj with LongKeyedMetaMapper[EnumObj] 

object MappedEnumSpec extends Specification  {
  "MappedEnum Specification".title

  "MappedEnum" should {
    "preserve enumeration order when building display list" in {
      val v = EnumObj.create

      import MyEnum._
      v.enum.buildDisplayList must_== List(a.id -> a.toString, b.id -> b.toString, c.id -> c.toString, d.id -> d.toString, e.id -> e.toString)
    }
  }
}

