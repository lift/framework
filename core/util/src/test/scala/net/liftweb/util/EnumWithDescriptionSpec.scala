/*
 * Copyright 2007-2011 WorldWide Conferencing, LLC
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
package util

import org.specs2.mutable.Specification


/**
 * Systems under specification for EnumWithDescription.
 */
object EnumWithDescriptionSpec extends Specification  {
  "EnumWithDescription Specification".title

  "An enumWithDescription" should {
    "have a name" in {
      val t = Title2.valueOf("MR") == Some(Title2.mr)
      t must beTrue
    }

    "have a name" in {
      Title1.mr.toString must_== "MR"
    }

    "have a type 1" in {
      Title1.mr mustEqual Title1.mr
    }

    "have a type 2" in {
      Title1.mr mustEqual Title1.valueOf("MR").getOrElse(null)
    }

    "have a type 3" in {
      Title1.dr mustEqual Title1.valueOf("DR").getOrElse(null)
    }

    "have a mr description" in {
      Title1.mr.description must_== "Mr"
    }

    "be able to be created from a string name" in {
      Title1.valueOf("MRS").getOrElse(null) mustEqual Title1.mrs
    }

    "have a mrs description" in {
      Title1.valueOf("MRS").getOrElse(null).description must beMatching ("Mrs")
    }
  }

}

object Title1 extends EnumWithDescription {
  val mr  = Value("MR", "Mr")
  val mrs = Value("MRS", "Mrs")
  val dr  = Value("DR", "Dr")
  val sir = Value("SirS", "Sir")
}


object Title2 extends EnumWithDescription {
  val mr  = Value("MR", "Mr")
  val mrs = Value("MRS", "Mrs")
}

