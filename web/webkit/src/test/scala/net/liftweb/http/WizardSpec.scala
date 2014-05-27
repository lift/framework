/*
 * Copyright 2009-2011 WorldWide Conferencing, LLC
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
package wizard

import org.specs2.mutable.Specification

import common._
import util._
import http._

/**
 * System under specification for Wizard.
 */
object WizardSpec extends Specification  {
  "Wizard Specification".title

  val session: LiftSession = new LiftSession("", Helpers.randomString(20), Empty)

  class WizardForTesting extends Wizard {
    object completeInfo extends WizardVar(false)

    def finish() {
      S.notice("Thank you for registering your pet")
      completeInfo.set(true)
    }

    class NameAndAgeScreen extends Screen {
      val name = field(S ? "First Name", "",
                       valMinLen(2, S ? "Name Too Short"))

      val age = field(S ? "Age", 0,
                      minVal(5, S ? "Too young"),
                      maxVal(120, S ? "You should be dead"))

      override def nextScreen = if (age.is < 18) parentName else favoritePet
    }
    class ParentNameScreen extends Screen {
      val parentName = field(S ? "Mom or Dad's name", "",
                             valMinLen(2, S ? "Name Too Short"),
                             valMaxLen(40, S ? "Name Too Long"))
    }
    class FavoritePetScreen extends Screen {
      val petName = field(S ? "Pet's name", "",
                          valMinLen(2, S ? "Name Too Short"),
                          valMaxLen(40, S ? "Name Too Long"))
    }

    val nameAndAge = new NameAndAgeScreen
    val parentName = new ParentNameScreen
    val favoritePet = new FavoritePetScreen
  }

  val MyWizard = new WizardForTesting

  "A Wizard can be defined" in {
    MyWizard.nameAndAge.screenName must_== "Screen 1"
    MyWizard.favoritePet.screenName must_== "Screen 3"
  }

  "A field must have a correct Manifest" in {
    MyWizard.nameAndAge.age.manifest.runtimeClass.getName must_== classOf[Int].getName
  }

  "A wizard must transition from first screen to second screen" in {
    S.initIfUninitted(session) {
      MyWizard.currentScreen.openOrThrowException("legacy code") must_== MyWizard.nameAndAge

      MyWizard.nextScreen

      MyWizard.currentScreen.openOrThrowException("legacy code") must_== MyWizard.nameAndAge

      MyWizard.nameAndAge.name.set("David")
      MyWizard.nameAndAge.age.set(14)

      MyWizard.nextScreen

      MyWizard.currentScreen.openOrThrowException("legacy code") must_== MyWizard.parentName

      MyWizard.prevScreen

      MyWizard.currentScreen.openOrThrowException("legacy code") must_== MyWizard.nameAndAge

      MyWizard.nameAndAge.age.set(45)

      MyWizard.nextScreen

      MyWizard.currentScreen.openOrThrowException("legacy code") must_== MyWizard.favoritePet

      S.clearCurrentNotices

      MyWizard.favoritePet.petName.set("Elwood")

      MyWizard.nextScreen

      MyWizard.currentScreen must_== Empty

      MyWizard.completeInfo.is must_== true
    }
  }

  "A wizard must be able to snapshot itself" in {
    val ss = S.initIfUninitted(session) {
      MyWizard.currentScreen.openOrThrowException("legacy code") must_== MyWizard.nameAndAge

      MyWizard.nextScreen

      MyWizard.currentScreen.openOrThrowException("legacy code") must_== MyWizard.nameAndAge

      MyWizard.nameAndAge.name.set("David")
      MyWizard.nameAndAge.age.set(14)

      MyWizard.nextScreen

      MyWizard.currentScreen.openOrThrowException("legacy code") must_== MyWizard.parentName

      MyWizard.createSnapshot
    }

    S.initIfUninitted(session) {
      MyWizard.currentScreen.openOrThrowException("legacy code") must_== MyWizard.nameAndAge
    }

    S.initIfUninitted(session) {
      ss.restore()

      MyWizard.prevScreen

      MyWizard.currentScreen.openOrThrowException("legacy code") must_== MyWizard.nameAndAge

      MyWizard.nameAndAge.age.set(45)

      MyWizard.nextScreen

      MyWizard.currentScreen.openOrThrowException("legacy code") must_== MyWizard.favoritePet

      S.clearCurrentNotices

      MyWizard.favoritePet.petName.set("Elwood")

      MyWizard.nextScreen

      MyWizard.currentScreen must_== Empty

      MyWizard.completeInfo.is must_== true
    }
  }
}

