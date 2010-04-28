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
package util {

import _root_.java.util.concurrent.{ConcurrentHashMap => CHash}
import _root_.java.lang.ThreadLocal
import _root_.scala.reflect.Manifest
import common._
import xml.NodeSeq

/**
 * A trait that does basic dependency injection.
 */
trait Injector {
  implicit def inject[T](implicit man: Manifest[T]): Box[T]
}

/**
 * An implementation of Injector that has an implementation
 */
trait SimpleInjector extends Injector {
  private val diHash: CHash[String, Function0[_]] = new CHash

  /**
   * Perform the injection for the given type.  You can call:
   * inject[Date] or inject[List[Map[String, PaymentThing]]].  The
   * appropriate Manifest will be
   */
  implicit def inject[T](implicit man: Manifest[T]): Box[T] =
  (Box !! diHash.get(man.toString)).flatMap(f => Helpers.tryo(f.apply())).asInstanceOf[Box[T]]

  /**
   * Register a function that will inject for the given Manifest
   */
  def registerInjection[T](f: () => T)(implicit man: Manifest[T]) {
    diHash.put(man.toString, f)
  }

  /**
   * Create an object or val that is a subclass of the FactoryMaker to
   * generate factory for a particular class as well as define session and
   * request specific vendors and use doWith to define the vendor just for
   * the scope of the call.
   */
  abstract class Inject[T](_default: Vendor[T])
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
     * Vend an instance
     */
    implicit def vend: T = make openOr default.is.apply()

    /**
     * Make a Box of the instance.
     */
    override implicit def make: Box[T] = super.make or Full(default.is.apply())
  }
}

/**
 * In addition to an Injector, you can have a Maker which will make a given
 * type.  The important thing about a Maker is that it's intended to be used
 * as part of a factory that can vend an instance without the vaguaries of
 * whether the given class has registered a with the injector.
 */
trait Maker[T] {
  implicit def make: Box[T]
}

object Maker {
  def apply[T](value: T): Maker[T] = new Maker[T]{implicit def make: Box[T] = Full(value)}
  def apply[T](func:() => T): Maker[T] = new Maker[T]{implicit def make: Box[T] = Full(func())}
  def apply[T](func: Box[() => T]): Maker[T] = new Maker[T]{implicit def make: Box[T] = func.map(_.apply())}
  def apply1[T](box: Box[T]): Maker[T] = new Maker[T]{implicit def make: Box[T] = box}
  def apply2[T](func: Box[() => Box[T]]): Maker[T] = new Maker[T]{implicit def make: Box[T] = func.flatMap(_.apply())}
  def apply3[T](func: () => Box[T]): Maker[T] = new Maker[T]{implicit def make: Box[T] = func.apply()}

  implicit def vToMake[T](v: T): Maker[T] = this.apply(v)
  implicit def vToMake[T](v: () => T): Maker[T] = this.apply(v)
  implicit def vToMakeB1[T](v: Box[T]): Maker[T] = this.apply1(v)
  implicit def vToMakeB2[T](v: Box[() => T]): Maker[T] = this.apply(v)
  implicit def vToMakeB3[T](v: Box[() => Box[T]]): Maker[T] = this.apply2(v)
  implicit def vToMakeB4[T](v: () => Box[T]): Maker[T] = this.apply3(v)
}

/**
 * A StackableMaker allows DynamicVar functionality by supply a Maker or function
 * that will vend an instance during any sub-call on the stack and then
 * restore the implementation.  This is value for testing.
 */
trait StackableMaker[T] extends Maker[T] {
  private val _stack: ThreadLocal[List[PValueHolder[Maker[T]]]] = new ThreadLocal

  private def stack: List[PValueHolder[Maker[T]]] = _stack.get() match {
    case null => Nil
    case x => x
  }

  def doWith[F](value: T)(f: => F): F =
  doWith(PValueHolder(Maker(value)))(f)

  def doWith[F](vFunc: () => T)(f: => F): F =
  doWith(PValueHolder(Maker(vFunc)))(f)

  def doWith[F](addl: PValueHolder[Maker[T]])(f: => F): F = {
    val old = _stack.get()
    _stack.set(addl :: stack)
    try {
      f
    } finally {
      _stack.set(old)
    }
  }

  protected final def find(in: List[PValueHolder[Maker[T]]]): Box[T] = in match {
    case Nil => Empty
    case x :: rest =>
      x.is.make match {
        case Full(v) => Full(v)
        case _ => find(rest)
      }
  }

  implicit def make: Box[T] = find(stack)
}

/**
 * An implementation where you can define the stack of makers.
 */
class MakerStack[T](subMakers: PValueHolder[Maker[T]]*) extends StackableMaker[T] {
  private val _sub: List[PValueHolder[Maker[T]]] = subMakers.toList

  override implicit def make: Box[T] = super.make or find(_sub)
}

/**
 * A Vendor is a Maker that also guarantees that it will return a value
 */
trait Vendor[T] extends Maker[T] with Function0[T] {
  implicit def vend: T
  def apply() = vend
}



/**
 * A companion to the Vendor trait
 */
object Vendor {
  def apply[T](f: () => T): Vendor[T] = new Vendor[T] {
    implicit def vend: T = f()
    implicit def make: Box[T] = Full(f())
  }

  def apply[T](f: T): Vendor[T] = new Vendor[T] {
    implicit def vend: T = f
    implicit def make: Box[T] = Full(f)
  }

  implicit def valToVender[T](value: T): Vendor[T] = apply(value)
  implicit def funcToVender[T](f: () => T): Vendor[T] = apply(f)
}


case class FormBuilderLocator[T](func: (T, T => Unit) => NodeSeq)(implicit val manifest: Manifest[T])

}
}
