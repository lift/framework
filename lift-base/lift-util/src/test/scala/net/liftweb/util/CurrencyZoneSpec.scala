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

import _root_.org.specs.runner._
import _root_.org.specs._
import common._

class CurrencyZoneSpecTest extends JUnit4(CurrencyZoneSpec)
object CurrencyZoneSpec extends Specification {

    "Australian money" should {
        "not equal to other money" in {
            val auBoolean = AU(4.42) != US(4.42)
            auBoolean mustEqual true
        }

        "be not equal to a different amount of its own money" in {
            val auBoolean = AU(4.48) == AU(4.481)
            auBoolean mustEqual false
        }


        "be equal to the same amount of its own money" in {
            val auBoolean = AU(4.42) == AU(4.42)
            auBoolean mustEqual true
        }

        "be comparable not gt" in {
            val auBoolean = AU(4.42) > AU(4.420000)
            auBoolean mustEqual false
        }

        "be creatable" in {
            AU(20.1).get mustMatch "20.10"
        }

        "be addable" in {
            val au = AU(20.68) + AU(3.08)
            au.get mustMatch "23.76"
        }

        "be subtractable" in {
            val au = AU(23.76) - AU(3.08)
            au.get mustMatch "20.68"
        }

        "be mutipliable" in {
            val au = AU(20.68) * 3
            au.get mustMatch "62.04"
        }

        "be divisable" in {
            val au = AU(20.68) / AU(3)
            au.get mustMatch "6.89"
        }


        "be comparable gt" in {
            val auBoolean = AU(20.68) > AU(3)
            auBoolean mustEqual true
        }

        "be comparable lt" in {
            val auBoolean = AU(3.439) < AU(3.44)
            auBoolean mustEqual true
        }

        "be comparable lt or eq" in {
            val auBoolean = AU(20.68) <= AU(20.68)
            auBoolean mustEqual true
        }

    }

}

}
}
