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
package http {

import _root_.net.liftweb.common._
import util._
import _root_.scala.reflect.Manifest

/**
 * A base trait for a Factory.  A Factory is both an Injector and
 * a collection of FactorMaker instances.  The FactoryMaker instances auto-register
 * with the Injector.  This provides both concrete Maker/Vender functionality as
 * well as Injector functionality.
 */
trait Factory extends SimpleInjector {
  /**
   * Create an object or val that is a subclass of the FactoryMaker to
   * generate factory for a particular class as well as define session and
   * request specific vendors and use doWith to define the vendor just for
   * the scope of the call.
   */
  abstract class FactoryMaker[T](_default: Vendor[T])
                                (implicit man: Manifest[T]) extends StackableMaker[T] with Vendor[T] {
    registerInjection(this)(man)

    /**
     * The default function for vending an instance
     */
    object default extends PSettableValueHolder[Vendor[T]] {
      private var value = _default

      def get = value

      def is = get

      def set(v: Vendor[T]): Vendor[T] = {
        value = v
        v
      }
    }

    /**
     * The session-specific Maker for creating an instance
     */
    object session extends SessionVar[Maker[T]](Empty) {
      override protected def __nameSalt = Helpers.randomString(20)
    }

    /**
     * The request specific Maker for creating an instance
     */
    object request extends RequestVar[Maker[T]](Empty) {
      override protected def __nameSalt = Helpers.randomString(20)
    }
    private val _sub: List[PValueHolder[Maker[T]]] = List(request, session)

    /**
     * Vend an instance
     */
    implicit def vend: T = make openOr default.is.apply()

    /**
     * Make a Box of the instance.
     */
    override implicit def make: Box[T] = super.make or find(_sub) or Full(default.is.apply())
  }
}

}
}
