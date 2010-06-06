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

package net.liftweb {
package wizard {

import _root_.org.scalacheck._
import _root_.org.scalacheck.Prop.forAll
import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}
import _root_.org.specs.ScalaCheck

import _root_.net.liftweb._
import http._
import common._
import _root_.net.liftweb.util._

class WizardTest extends Runner(WizardSpec) with JUnit
object WizardSpec extends Specification {
  val session : LiftSession = new LiftSession("", Helpers.randomString(20), Empty)

  val MyWizard = new Wizard {
    object completeInfo extends WizardVar(false)

    def finish() {
      S.notice("Thank you for registering your pet")
      completeInfo.set(true)
    }

    val nameAndAge = new Screen {
      val name = field(S ? "First Name", "",
                       valMinLen(2, S ?? "Name Too Short"))

      val age = field(S ? "Age", 0,
                      minVal(5, S ?? "Too young"),
        maxVal(120, S ?? "You should be dead"))

      override def nextScreen = if (age.is < 18) parentName else favoritePet
    }

    val parentName = new Screen {
      val parentName = field(S ? "Mom or Dad's name", "",
                             valMinLen(2, S ?? "Name Too Short"),
                             valMaxLen(40, S ?? "Name Too Long"))
    }

    val favoritePet = new Screen {
      val petName = field(S ? "Pet's name", "",
                          valMinLen(2, S ?? "Name Too Short"),
                          valMaxLen(40, S ?? "Name Too Long"))
    }
  }

  "A Wizard can be defined" in {
    MyWizard.nameAndAge.screenName must_== "Screen 1"

    MyWizard.favoritePet.screenName must_== "Screen 3"
  }

  "A field must have a correct Manifest" in {
    MyWizard.nameAndAge.age.manifest.erasure.getName must_== classOf[Int].getName
  }

  "A wizard must transition from first screen to second screen" in {
    S.initIfUninitted(session) {
      MyWizard.currentScreen.open_! must_== MyWizard.nameAndAge

      MyWizard.nextScreen

      MyWizard.currentScreen.open_! must_== MyWizard.nameAndAge

      MyWizard.nameAndAge.name.set("David")
      MyWizard.nameAndAge.age.set(14)

      MyWizard.nextScreen

      MyWizard.currentScreen.open_! must_== MyWizard.parentName

      MyWizard.prevScreen

      MyWizard.currentScreen.open_! must_== MyWizard.nameAndAge

      MyWizard.nameAndAge.age.set(45)

      MyWizard.nextScreen

      MyWizard.currentScreen.open_! must_== MyWizard.favoritePet

      S.clearCurrentNotices

      MyWizard.favoritePet.petName.set("Elwood")

      MyWizard.nextScreen

      MyWizard.currentScreen must_== Empty

      MyWizard.completeInfo.is must_== true
    }
  }

  "A wizard must be able to snapshot itself" in {
    val ss = S.initIfUninitted(session) {
      MyWizard.currentScreen.open_! must_== MyWizard.nameAndAge

      MyWizard.nextScreen

      MyWizard.currentScreen.open_! must_== MyWizard.nameAndAge

      MyWizard.nameAndAge.name.set("David")
      MyWizard.nameAndAge.age.set(14)

      MyWizard.nextScreen

      MyWizard.currentScreen.open_! must_== MyWizard.parentName

      MyWizard.createSnapshot
    }

    S.initIfUninitted(session) {
      MyWizard.currentScreen.open_! must_== MyWizard.nameAndAge


    }


    S.initIfUninitted(session) {
      ss.restore()

      MyWizard.prevScreen

      MyWizard.currentScreen.open_! must_== MyWizard.nameAndAge

      MyWizard.nameAndAge.age.set(45)

      MyWizard.nextScreen

      MyWizard.currentScreen.open_! must_== MyWizard.favoritePet

      S.clearCurrentNotices

      MyWizard.favoritePet.petName.set("Elwood")

      MyWizard.nextScreen

      MyWizard.currentScreen must_== Empty

      MyWizard.completeInfo.is must_== true
    }
  }
}

}
}
