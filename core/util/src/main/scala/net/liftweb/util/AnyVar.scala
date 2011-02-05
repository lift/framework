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

import Helpers._
import common._

// This object holds string constants in a central place
private[liftweb] object VarConstants {
  val varPrefix = "_lift_sv_"
  val initedSuffix = "_inited_?"
  val lockSuffix="_lock_dude"
}

trait HasCalcDefaultValue[T] {
  protected def calcDefaultValue: T
}

trait MemoizeVar[K, V]  {
  protected def coreVar: AnyVar[LRU[K, V], _]

  protected def buildLRU = new LRU[K, V](cacheSize)

  /**
   * The number of entries that will be memoized
   */
  protected def cacheSize: Int = 200

  def apply(key: K): Box[V] = get(key)

  def apply(key: K, dflt: => V): V = get(key, dflt)

  def get(key: K): Box[V] = coreVar.doSync {
    coreVar.is.get(key)
  }

  def get(key: K, dflt: => V): V = coreVar.doSync {
    get(key) match {
      case Full(v) => v
      case _ =>
        val ret = dflt
        set(key, ret)
        ret
    }
  }

  protected def __nameSalt: String = ""

  def set(key: K, value: V): Unit = coreVar.doSync {
    coreVar.is.update(key, value)
  }

  def update(key: K, value: V): Unit = set(key,value)
}

abstract class AnyVar[T, MyType <: AnyVar[T, MyType]](dflt: => T) extends AnyVarTrait[T, MyType] {
  self: MyType =>

  protected def calcDefaultValue: T = dflt

  
}

/**
 * Abstract a request or a session scoped variable.
 */
trait AnyVarTrait[T, MyType <: AnyVarTrait[T, MyType]] extends PSettableValueHolder[T] with HasCalcDefaultValue[T] {
  self: MyType =>
  protected lazy val name = VarConstants.varPrefix+getClass.getName+"_"+__nameSalt
  protected def findFunc(name: String): Box[T]
  protected def setFunc(name: String, value: T): Unit
  protected def clearFunc(name: String): Unit
  protected def wasInitialized(name: String): Boolean


  protected def calcDefaultValue: T

  /**
   * A non-side-effecting test if the value was initialized
   */
  protected def testWasSet(name: String): Boolean

  protected def __nameSalt = ""

  /**
   * Keep track of whether we're currently setting the default value
   */
  private val settingDefault = new ThreadGlobal[Boolean]

  protected def settingDefault_? : Boolean = settingDefault.box openOr false

  type CleanUpParam

  /**
   * Different Vars require different mechanisms for synchronization.  This method implements
   * the Var specific synchronization mechanism
   */
  def doSync[F](f: => F): F

  /**
   * The current value of the variable
   */
  def is: T = doSync {
    findFunc(name) match {
      case Full(v) => v
      case _ => val ret = calcDefaultValue
        testInitialized
      settingDefault.doWith(true) {
        apply(ret)
      }
        // Use findFunc so that we clear the "unread" flag
        findFunc(name) match {
          case Full(v) => v
          case _ => ret
        }
    }
  }

  private def testInitialized: Unit = doSync {
    if (!wasInitialized(name)) {
      registerCleanupFunc(_onShutdown _)
    }
  }

  /**
   * Shadow of the 'is' method
   */
  def get: T = is

  /**
   * Shadow of the apply method
   */
  def set(what: T): T = apply(what)

  /**
   * Has this Var been set or accessed and had its default value calculated
   */
  def set_? : Boolean = testWasSet(name)

  /**
   * Set the Var if it has not been calculated
   */
  def setIsUnset(value: => T): T = doSync {
    if (!set_?) {
      set(value)
    }
    this.is
  }

  /**
   * Set the session variable
   *
   * @param what -- the value to set the session variable to
   */
  def apply(what: T): T = {
    testInitialized
    setFunc(name, what)
    what
  }

  /**
   * Applies the given function to the contents of this
   * variable and sets the variable to the resulting value.
   *
   * @param f -- the function to apply and set the result from.
   */
  def update(f: T => T): T = {
    apply(f(is))
    is
  }

  def remove(): Unit = clearFunc(name)

  //def cleanupFunc: Box[() => Unit] = Empty

  protected def registerCleanupFunc(in: CleanUpParam => Unit): Unit

  protected final def registerGlobalCleanupFunc(in: CleanUpParam => Unit) {
    cuf ::= in
  }

  private var cuf: List[CleanUpParam => Unit] = Nil

  private def _onShutdown(session: CleanUpParam): Unit = {
    cuf.foreach(f => tryo(f(session)))
    onShutdown(session)
  }

  protected def onShutdown(session: CleanUpParam): Unit = {}

  override def toString = is.toString

  /**
   * Change the value of the Var for the lifespan of the function
   */
  def doWith[F](newVal: T)(f: => F): F = {
    val old = findFunc(name)
    setFunc(name, newVal)
    try {
      f
    } finally {
      old match {
        case Full(t) => setFunc(name, t)
        case _ => clearFunc(name)
      }
    }
  }
}

abstract class NonCleanAnyVar[T](dflt: => T) extends AnyVar[T, NonCleanAnyVar[T]](dflt) {
  type CleanUpParam = Unit
  override protected def registerCleanupFunc(in: Unit => Unit): Unit = {}
}

object AnyVar {
  implicit def whatVarIs[T](in: AnyVar[T, _]): T = in.is
}

}
}
