/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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
package util {

import _root_.java.util.GregorianCalendar
import _root_.org.specs.runner._
import _root_.org.specs._
import common._

object Title2 extends EnumWithDescription {
    val mr = Value("MR", "Mr")
    val mrs = Value("MRS", "Mrs")
    val dr = Value("DR", "Dr")
    val sir = Value("SirS", "Sir")
}

object T2 extends EnumWithDescription {
    val mr = Value("MR", "Mr")
    val mrs = Value("MRS", "Mrs")
}

class EnumWithDescriptionSpecTest extends JUnit4(EnumWithDescriptionSpec)
object EnumWithDescriptionSpec extends Specification {

    "An enumWithDescription" should {
        "have a name" in {
            val t = T2.valueOf("MR") == Some(T2.mr)
            t must beTrue
        }

        "have a name" in {
            Title2.mr.toString must_== "MR"
        }

        "have a type 1" in {
            Title2.mr mustEqual Title2.mr
        }

        "have a type 2" in {
            Title2.mr mustEqual Title2.valueOf("MR").getOrElse(null)
        }

        "have a type 3" in {
            Title2.dr mustEqual Title2.valueOf("DR").getOrElse(null)
        }

        "have a mr description" in {
            Title2.mr.description must_== "Mr"
        }

        "be able to be created from a string name" in {
            Title2.valueOf("MRS").getOrElse(null) mustEqual Title2.mrs
        }

        "have a mrs description" in {
            Title2.valueOf("MRS").getOrElse(null).description mustMatch "Mrs"
        }
    }

}

}
}
