/*
 * Copyright 2011 WorldWide Conferencing, LLC
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
package actor

import common._

import org.specs._
import org.specs.specification._

object ActorSpec extends Specification {
  "An Actor" should {
    def make = new MyActor

    "Allow setting & getting of a value" in {
      val a = make
      a ! Set(33)
      (a. !?(5L, Get())) must_== Full(Answer(33))
    }

    "Allow setting & getting of a value with subclass of Get()" in {
      val a = make
      a ! Set(33)
      (a. !?(5L, new FunnyGet())) must_== Full(Answer(33))
    }

    "Allow adding of a value" in {
      val a = make
      a ! Set(33)
      (a !< Add(44)).get(5) must_== Full(Answer(77))
    }

    "Allow subtracting of a value" in {
      val a = make
      a ! Set(33)
      (a !< Sub(11)).get(5) must_== Full(Answer(22))
    }

    "Properly timeout" in {
      val a = make
      (a !< Set(33)).get(5) must_== Empty
    }
  }

  "A Java Actor" should {
    def make = new MyJavaActor()

    "Allow setting & getting of a value" in {
      val a = make
      a ! Set(33)
      a !? Get()
      (a.!?(25, Get())) must_== Full(Answer(33))
    }

    "Allow setting & getting of a value with subclass of Get()" in {
      val a = make
      a ! Set(33)
      a ! new FunnyGet()
      (a. !?(50L, new FunnyGet())) must_== Full(Answer(33))
    }


    "Allow adding of a value" in {
      val a = make
      a ! Set(33)
      (a !< Add(44)).get(5) must_== Full(Answer(77))
    }

    "Allow subtracting of a value" in {
      val a = make
      a ! Set(33)
      (a !< Sub(11)).get(5) must_== Full(Answer(22))
    }

    "Properly timeout" in {
      val a = make
      (a !< Set(33)).get(5) must_== Empty
    }
  }
}

case class Add(num: Int)
case class Sub(num: Int)
case class Set(num: Int)
case class Get()

class FunnyGet() extends Get()

case class Answer(num: Int)

class MyActor extends LiftActor {
  private var value = 0

  override protected def messageHandler = {
    case Add(n) => value += n; reply(Answer(value))
    case Sub(n) => value -= n; reply(Answer(value))
    case Set(n) => value = n
    case Get() => reply(Answer(value))
  }
}


class ActorSpecTest extends _root_.org.specs.runner.JUnit4(ActorSpec)

