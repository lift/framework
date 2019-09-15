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
package http

import org.specs2.mutable.Specification

import common._


/**
 * System under specification for FactoryMaker.
 */
class FactoryMakerSpec extends Specification  {
  "FactoryMaker Specification".title


  object MyFactory extends Factory {
    val f1: FactoryMaker[List[String]] = new FactoryMaker(() => List("Hello", "World")) {}
     val f2: FactoryMaker[Boolean] = new FactoryMaker(() => false) {}
  }


  "Factories" should {
    "Allow multiple FactoryMakers to exist" in {
       val session = new LiftSession("hello", "", Empty)

    val res = S.initIfUninitted(session) {
      MyFactory.f2.request.set(true)

      MyFactory.f1.vend
    }
      res must_== List("Hello", "World")
    }
  }
}

