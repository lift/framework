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
package http {

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._
import _root_.scala.collection.mutable.{HashMap, HashSet, ListBuffer}

/**
 * A typesafe container for data with a lifetime nominally equivalent to the
 * lifetime of HttpSession attributes.
 *
 * <code>
 * object MySnippetCompanion {
 *   object mySessionVar extends SessionVar[String]("hello")
 * }
 * </code>
 *
 * The standard pattern is to create a singleton object extending SessionVar instead
 * of creating an instance variable of a concrete SessionVar subclass. This is preferred
 * because SessionVar will use the name of its instantiating class for part of its state
 * maintenance mechanism.
 *
 * If you find it necessary to create a SessionVar subclass of which there may be more
 * than one instance, it is necessary to override the __nameSalt() method to return
 * a unique salt value for each instance to prevent name collisions.
 *
 * Note: SessionVars can be used within CometActors
 *
 * @param dflt - the default value to be returned if none was set prior to
 * requesting a value to be returned from the container
 */
abstract class SessionVar[T](dflt: => T) extends AnyVar[T, SessionVar[T]](dflt) with LazyLoggable {
  override protected def findFunc(name: String): Box[T] = S.session match {
    case Full(s) => s.get(name)
    case _ =>
      if (showWarningWhenAccessedOutOfSessionScope_?)
      logger.warn("Getting a SessionVar "+name+" outside session scope") // added warning per issue 188

      Empty
  }

  /**
   * Stateless session enforcement is new to Lift, but there
   * are some legacy issues in WebKit and allowing for a SessionVar
   * to be "magic" (settable even in stateless sessions) seems to be
   * an efficient, yet somewhat hacky, way around the issue
   */
  private[liftweb] def magicSessionVar_? = false

  override protected def setFunc(name: String, value: T): Unit = S.session match {
    // If we're in a stateless session, don't allow SessionVar setting
    case Full(s) if !magicSessionVar_? && !s.stateful_? && !settingDefault_? =>
      throw new StateInStatelessException("setting a SessionVar in a "+
                                          "stateless session: "+getClass.getName)

    case Full(s) => s.set(name, value)
    case _ =>
      if (showWarningWhenAccessedOutOfSessionScope_?)
      logger.warn("Setting a SessionVar "+name+" to "+value+" outside session scope") // added warning per issue 188
  }

      /**
   * Different Vars require different mechanisms for synchronization.  This method implements
   * the Var specific synchronization mechanism
   */
  def doSync[F](f: => F): F = S.session match {
    case Full(s) =>
      // lock the session while the Var-specific lock object is found/created
      val lockName = name + VarConstants.lockSuffix
      val lockObj = s.synchronized {
        s.get[AnyRef](lockName) match {
          case Full(lock) => lock
          case _ => val lock = new AnyRef
          s.set(lockName, lock)
          lock
        }
      }

      // execute the query in the scope of the lock obj
      lockObj.synchronized {
        f
      }
    case _ => f
  }

  def showWarningWhenAccessedOutOfSessionScope_? = false

  override protected def clearFunc(name: String): Unit = S.session.foreach(_.unset(name))

  override protected def wasInitialized(name: String): Boolean = {
    val bn = name + VarConstants.initedSuffix
    val old: Boolean = S.session.flatMap(_.get(bn)) openOr false
    S.session.foreach(_.set(bn, true))
    old
  }

  override protected def testWasSet(name: String): Boolean = {
    val bn = name + VarConstants.initedSuffix
    S.session.flatMap(_.get(name)).isDefined || (S.session.flatMap(_.get(bn)) openOr false)
  }





  protected override def registerCleanupFunc(in: LiftSession => Unit): Unit =
  S.session.foreach(_.addSessionCleanup(in))

  type CleanUpParam = LiftSession
}

private[http] trait HasLogUnreadVal {
  def logUnreadVal: Boolean
}


/**
 * A typesafe container for data with a lifetime nominally equivalent to the
 * lifetime of a page rendered by an HTTP request.
 * RequestVars maintain their value throughout the duration of the current HTTP
 * request and any callbacks for servicing AJAX calls associated with the rendered page.
 * RequestVar instances have no value at the beginning of request servicing (excluding
 * AJAX callbacks) and their value is discarded at the end of request processing.
 * They are commonly used to share values across many snippets. Basic usage:
 *
 * <code>
 * object MySnippetCompanion {
 *   object myRequestVar extends RequestVar[String]("hello")
 * }
 * </code>
 *
 * The standard pattern is to create a singleton object extending RequestVar instead
 * of creating an instance variable of a concrete RequestVar subclass. This is preferred
 * because RequestVar will use the name of its instantiating class for part of its state
 * maintenance mechanism.
 *
 * If you find it necessary to create a RequestVar subclass of which there may be more
 * than one instance, it is necessary to override the __nameSalt() method to return
 * a unique salt value for each instance to prevent name collisions.
 *
 * @param dflt - the default value to be returned if none was set prior to
 * requesting a value to be returned from the container
 */
abstract class RequestVar[T](dflt: => T) extends AnyVar[T, RequestVar[T]](dflt) with HasLogUnreadVal {
  type CleanUpParam = Box[LiftSession]

  override protected def findFunc(name: String): Box[T] = RequestVarHandler.get(name)

  override protected def setFunc(name: String, value: T): Unit = RequestVarHandler.set(name, this, value)

  override protected def clearFunc(name: String): Unit = RequestVarHandler.clear(name)

  override protected def wasInitialized(name: String): Boolean = {
    val bn = name + VarConstants.initedSuffix
    val old: Boolean = RequestVarHandler.get(bn) openOr false
    RequestVarHandler.set(bn, this, true)
    old
  }

  /**
   * Different Vars require different mechanisms for synchronization.  This method implements
   * the Var specific synchronization mechanism
   */
  def doSync[F](f: => F): F = f // no sync necessary for RequestVars... always on the same thread

  override protected def testWasSet(name: String): Boolean = {
    val bn = name + VarConstants.initedSuffix
    RequestVarHandler.get(name).isDefined || (RequestVarHandler.get(bn) openOr false)
  }

  /**
   * Generate a function that will take a snapshot of the current RequestVars
   * such that they can be restored
   */
  final def generateSnapshotRestorer[T](): Function1[Function0[T], T] = RequestVarHandler.generateSnapshotRestorer()

  override protected def registerCleanupFunc(in: Box[LiftSession] => Unit): Unit = {
    RequestVarHandler.addCleanupFunc(in)
  }

  /**
   * This defines whether or not Lift will log when a RequestVar is set but then not read within
   * the same request cycle. Change this to false to turn off logging. Logging can also be turned
   * off globally via LiftRules.logUnreadRequestVars.
   *
   * @see LiftRules#logUnreadRequestVars
   */
  def logUnreadVal = true
}

/**
 * A typesafe container for data with a lifetime strictly equal to the processing of a single
 * HTTP request. Unlike ordinary RequestVar instances, TransientRequestVars will not maintain
 * data for servicing of AJAX callbacks from a rendered page. This is useful in cases where
 * the value stored within the RequestVar cannot safely be used across multiple requests; an
 * example of such a value is a JTA UserTransaction which has a lifecycle strictly coupled
 * to the actul HTTP request handling by the enclosing container.
 *
 * @param dflt - the default value to be returned if none was set prior to
 * requesting a value to be returned from the container
 */
abstract class TransientRequestVar[T](dflt: => T) extends AnyVar[T, TransientRequestVar[T]](dflt) with HasLogUnreadVal {
  type CleanUpParam = Box[LiftSession]

  override protected def findFunc(name: String): Box[T] = TransientRequestVarHandler.get(name)

  override protected def setFunc(name: String, value: T): Unit = TransientRequestVarHandler.set(name, this, value)

  override protected def clearFunc(name: String): Unit = TransientRequestVarHandler.clear(name)

  override protected def wasInitialized(name: String): Boolean = {
    val bn = name + VarConstants.initedSuffix
    val old: Boolean = TransientRequestVarHandler.get(bn) openOr false
    TransientRequestVarHandler.set(bn, this, true)
    old
  }

  protected override def testWasSet(name: String): Boolean = {
    val bn = name + VarConstants.initedSuffix
    TransientRequestVarHandler.get(name).isDefined || (TransientRequestVarHandler.get(bn) openOr false)
  }

  /**
   * Different Vars require different mechanisms for synchronization.  This method implements
   * the Var specific synchronization mechanism
   */
  def doSync[F](f: => F): F = f // no sync necessary for RequestVars... always on the same thread

  override protected def registerCleanupFunc(in: Box[LiftSession] => Unit): Unit =
  TransientRequestVarHandler.addCleanupFunc(in)

  /**
   * This defines whether or not Lift will log when a RequestVar is set but then not read within
   * the same request cycle. Change this to false to turn off logging. Logging can also be turned
   * off globally via LiftRules.logUnreadRequestVars.
   *
   * @see LiftRules#logUnreadRequestVars
   */
  def logUnreadVal = false
}

trait CleanRequestVarOnSessionTransition {
  self: RequestVar[_] =>
}

private[http] object RequestVarHandler extends CoreRequestVarHandler {
  type MyType = RequestVar[_]
}

private[http] object TransientRequestVarHandler extends CoreRequestVarHandler {
  type MyType = TransientRequestVar[_]
}

private[http] trait CoreRequestVarHandler {
  type MyType <: HasLogUnreadVal
  
  private val logger = Logger(classOf[CoreRequestVarHandler])
  // This maps from the RV name to (RV instance, value, set-but-not-read flag)
  private val vals: ThreadGlobal[HashMap[String, (MyType, Any, Boolean)]] = new ThreadGlobal
  private val cleanup: ThreadGlobal[ListBuffer[Box[LiftSession] => Unit]] = new ThreadGlobal
  private val isIn: ThreadGlobal[String] = new ThreadGlobal
  private val sessionThing: ThreadGlobal[Box[LiftSession]] = new ThreadGlobal

  /**
   * Generate a function that will take a snapshot of the current RequestVars
   * such that they can be restored
   */
  final def generateSnapshotRestorer[T](): Function1[Function0[T], T] = {
    val myVals = vals.value
    val mySessionThing = sessionThing.value

    f => isIn.doWith("in")(
      vals.doWith(myVals)(
        cleanup.doWith(new ListBuffer) {
          sessionThing.doWith(mySessionThing) {
            val ret: T = f()

            cleanup.value.toList.foreach(clean => Helpers.tryo(clean(sessionThing.value)))

            ret
          }
        }
      )
    )
  }

  private[http] def get[T](name: String): Box[T] =
  for {
    ht <- Box.legacyNullTest(vals.value)
    (rvInstance,value,unread) <- ht.get(name)
  } yield {
    if (unread) {
      // Flag the variable as no longer being set-but-unread
      ht(name) = (rvInstance : MyType, value.asInstanceOf[T], false)
    }
    value.asInstanceOf[T]
  }

  private[http] def set[T](name: String, from: MyType, value: T): Unit =
  for (ht <- Box.legacyNullTest(vals.value))
  ht(name) = (from, value, true)

  private[http] def clear(name: String): Unit =
  for (ht <- Box.legacyNullTest(vals.value))
  ht -= name

  private[http] def addCleanupFunc(f: Box[LiftSession] => Unit): Unit =
  for (cu <- Box.legacyNullTest(cleanup.value))
  cu += f

  def apply[T](session: Box[LiftSession], f: => T): T = {
    if ("in" == isIn.value) {
      val tv = vals.value

      // remove all the session variables that are CleanRequestVarOnSessionTransition
      val toRemove: Iterable[String] = tv.flatMap {
        case (name, (it: CleanRequestVarOnSessionTransition, _, _)) => List(name)
        case _ => Nil
      }

      toRemove.foreach(n => tv -= n)


      sessionThing.set(session)
      f
    } else {
      isIn.doWith("in") (
        vals.doWith(new HashMap) (
          cleanup.doWith(new ListBuffer) {
            sessionThing.doWith(session) {
              val ret: T = f

              cleanup.value.toList.foreach(clean => Helpers.tryo(clean(sessionThing.value)))

              if (Props.devMode && LiftRules.logUnreadRequestVars) {
                vals.value.keys.filter(! _.startsWith(VarConstants.varPrefix+"net.liftweb"))
                .filter(! _.endsWith(VarConstants.initedSuffix))
                .foreach(key => vals.value(key) match {
                    case (rv,_,true) if rv.logUnreadVal => logger.warn("RequestVar %s was set but not read".format(key.replace(VarConstants.varPrefix,"")))
                    case _ =>
                  })
              }

              ret
            }
          }
        )
      )
    }
  }
}


object AnyVar {
  implicit def whatSessionVarIs[T](in: SessionVar[T]): T = in.is

  implicit def whatRequestVarIs[T](in: RequestVar[T]): T = in.is
}

/**
 * Memoize a value for the duration of the user's session
 */
abstract class SessionMemoize[K, V] extends MemoizeVar[K, V] {
  protected object coreVar extends SessionVar[LRU[K, V]](buildLRU) {
    override def __nameSalt = SessionMemoize.this.__nameSalt
  }
}

/**
 * Memoize a value for the duration of the current request (and subsequent Ajax requests made as a result of viewing the page)
 */
abstract class RequestMemoize[K, V] extends MemoizeVar[K, V] {
  protected object coreVar extends RequestVar[LRU[K, V]](buildLRU) {
    override def __nameSalt = RequestMemoize.this.__nameSalt
  }
}

}
}
