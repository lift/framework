/*
 * Copyright 2007-2011 WorldWide Conferencing, LLC
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

import net.liftweb.common._
import net.liftweb.util._
import scala.collection.mutable.ListBuffer
import java.util.concurrent.{ConcurrentHashMap, Callable}
import scala.collection.JavaConversions._

/**
 * The bridge between Scala *Vars implementations and
 * the 
 */
class VarsJBridge {
  def vendSessionVar[T](default: T, e: Exception): SessionVar[T] = {
    vendSessionVar(new Callable[T] {
      def call() = default
    }, e)
  }

  def vendSessionVar[T](default: Callable[T], e: Exception): SessionVar[T] = {
    new SessionVar(default.call()) {
      override val __nameSalt = e.getStackTrace.apply(1).toString
    }
  }
}

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
      if (LiftRules.throwOnOutOfScopeVarAccess) {
        throw new IllegalAccessException("Access to SessionVar outside a request or comet actor scope")
      }

      if (showWarningWhenAccessedOutOfSessionScope_?)
        logger.warn("Getting a SessionVar " + name + " outside session scope") // added warning per issue 188

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
      throw new StateInStatelessException("setting a SessionVar in a " +
        "stateless session: " + getClass.getName)

    case Full(s) => s.set(name, value)
    case _ =>
      if (LiftRules.throwOnOutOfScopeVarAccess) {
        throw new IllegalAccessException("Access to SessionVar outside a request or comet actor scope")
      }

      if (showWarningWhenAccessedOutOfSessionScope_?)
        logger.warn("Setting a SessionVar " + name + " to " + value + " outside session scope") // added warning per issue 188
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

  override protected def wasInitialized(name: String, bn: String): Boolean = {
    val old: Boolean = S.session.flatMap(_.get(bn)) openOr false
    S.session.foreach(_.set(bn, true))
    old
  }

  override protected def testWasSet(name: String, bn: String): Boolean = {
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
 * lifetime of HttpSession attributes.  This alternative to SessionVar
 * keeps data in the container's session and must be serializable to
 * support session migration.  Use SessionVars unless you are using
 * MigratoryLiftSessions.
 *
 * <code>
 * object MySnippetCompanion {
 *   object mySessionVar extends ContainerVar[String]("hello")
 * }
 * </code>
 *
 * The standard pattern is to create a singleton object extending ContainerVar instead
 * of creating an instance variable of a concrete ContainerVar subclass. This is preferred
 * because ContainerVar will use the name of its instantiating class for part of its state
 * maintenance mechanism.
 *
 * If you find it necessary to create a ContainerVar subclass of which there may be more
 * than one instance, it is necessary to override the __nameSalt() method to return
 * a unique salt value for each instance to prevent name collisions.
 *
 * @param dflt - the default value to be returned if none was set prior to
 * requesting a value to be returned from the container
 * @param containerSerializer -- an implicit parameter that keeps us honest
 * about only storing things that can be actually serialized.  Lift
 * provides a subset of these.
 */
abstract class ContainerVar[T](dflt: => T)(implicit containerSerializer: ContainerSerializer[T]) extends AnyVar[T, ContainerVar[T]](dflt) with LazyLoggable {
  override protected def findFunc(name: String): Box[T] = S.session match {
    case Full(session) => {
      localGet(session, name) match {
        case Full(array: Array[Byte]) => Full(containerSerializer.deserialize(array))
        case _ => Empty
      }
    }

    case _ => {
      if (showWarningWhenAccessedOutOfSessionScope_?)
        logger.warn("Getting a SessionVar " + name + " outside session scope") // added warning per issue 188

      Empty
    }
  }

  private def localSet(session: LiftSession, name: String, value: Any): Unit = {
    for {
      httpSession <- session.httpSession
    } httpSession.setAttribute(name, value)
  }

  private def localGet(session: LiftSession, name: String): Box[Any] = {
    for {
      httpSession <- session.httpSession
      attr <- Box !! httpSession.attribute(name)
    } yield attr
  }


  override protected def setFunc(name: String, value: T): Unit = S.session match {
    // If we're in a stateless session, don't allow SessionVar setting
    case Full(s) if !s.allowContainerState_? && !s.stateful_? && !settingDefault_? =>
      throw new StateInStatelessException("setting a SessionVar in a " +
        "stateless session: " + getClass.getName)

    case Full(session) => {
      localSet(session, name, containerSerializer.serialize(value))
    }

    case _ =>
      if (showWarningWhenAccessedOutOfSessionScope_?)
        logger.warn("Setting a ContainerVar " + name + " to " + value + " outside session scope") // added warning per issue 188
  }

  /**
   * Different Vars require different mechanisms for synchronization.  This method implements
   * the Var specific synchronization mechanism.
   *
   * In the case of ContainerVar, we synchronize on the ContainerVar
   * instance itself.
   */
  def doSync[F](f: => F): F = this.synchronized(f)

  def showWarningWhenAccessedOutOfSessionScope_? = false

  override protected def clearFunc(name: String): Unit =
    for {
      session <- S.session
      httpSession <- session.httpSession
    } httpSession.removeAttribute(name)

  override protected def wasInitialized(name: String, bn: String): Boolean = {
    val old: Boolean = S.session.flatMap(s => localGet(s, bn) match {
      case Full(b: Boolean) => Full(b)
      case _ => Empty
    }) openOr false
    S.session.foreach(s => localSet(s, bn, true))
    old
  }

  override protected def testWasSet(name: String, bn: String): Boolean = {
    S.session.flatMap(s => localGet(s, name)).isDefined ||
      (S.session.flatMap(s => localGet(s, bn) match {
        case Full(b: Boolean) => Full(b)
        case _ => Empty
      }) openOr false)
  }

  protected override def registerCleanupFunc(in: LiftSession => Unit): Unit =
    S.session.foreach(_.addSessionCleanup(in))

  type CleanUpParam = LiftSession
}

/**
 * A trait that provides *actual* serialization of a type so that
 * the type can be stored into a container's session and be migrated across
 * servers
 */
trait ContainerSerializer[T] {
  def serialize(in: T): Array[Byte]

  def deserialize(in: Array[Byte]): T
}

object ContainerSerializer {

  import java.util.Date
  import org.joda.time.DateTime

  private def buildSerializer[T]: ContainerSerializer[T] =
    new ContainerSerializer[T] {

      import java.io._

      def serialize(in: T): Array[Byte] = {
        val bos = new ByteArrayOutputStream()
        val oos = new ObjectOutputStream(bos)
        oos.writeObject(in)
        oos.flush()
        bos.toByteArray()
      }

      def deserialize(in: Array[Byte]): T = {
        val bis = new ByteArrayInputStream(in)
        val ois = new ObjectInputStream(bis)
        ois.readObject.asInstanceOf[T]
      }
    }

  implicit val objectSerializer: ContainerSerializer[Object] = buildSerializer
  implicit val intSerializer: ContainerSerializer[Int] = buildSerializer
  implicit val longSerializer: ContainerSerializer[Long] = buildSerializer
  implicit val charSerializer: ContainerSerializer[Char] = buildSerializer
  implicit val shortSerializer: ContainerSerializer[Short] = buildSerializer
  implicit val byteSerializer: ContainerSerializer[Byte] = buildSerializer
  implicit val floatSerializer: ContainerSerializer[Float] = buildSerializer
  implicit val doubleSerializer: ContainerSerializer[Double] = buildSerializer
  implicit val booleanSerializer: ContainerSerializer[Boolean] = buildSerializer
  implicit val dateSerializer: ContainerSerializer[Date] = buildSerializer
  implicit val stringSerializer: ContainerSerializer[String] = buildSerializer
  implicit val jodaDateSerializer: ContainerSerializer[DateTime] = buildSerializer

  implicit def arraySerializer[T](implicit tc: ContainerSerializer[T]): ContainerSerializer[Array[T]] = buildSerializer

  implicit def listSerializer[T](implicit tc: ContainerSerializer[T]): ContainerSerializer[List[T]] = buildSerializer

}

/**
 * Create case objects that implement this trait and use the case objects to denote
 * specific SnapshotGroups for RequestVars
 */
trait RequestVarSnapshotGroup

/**
 * This subclass of RequestVars that allow the specification of a RequestVarSnapshotGroup.
 * You can create a snapshot of all the members of this group in RequestVar.snapshot
 */
abstract class SnapshotRequestVar[T](val group: RequestVarSnapshotGroup, d: => T) extends RequestVar[T](d) {

  /**
   * The Snapshot group this requestvar is part of
   */
  override def snapshotGroup: Box[RequestVarSnapshotGroup] = Full(group)
}

/**
 * The companion object to RequestVars
 */
object RequestVar {
  /**
   * Given a RequestVarSnapshotGroup, generate a function that will snapshot all the RequestVars in
   * that group.  When the function is run, the RequestVars will be set to the value they held
   * when they were snapshotted
   */
  def snapshot(group: RequestVarSnapshotGroup): () => Unit = {
    // capture the restore functions
    val funcs = RequestVarHandler.instancesOfGroup(group).map(_.snapshot())

    // return a function that applies all the restore functions
    () => funcs.foreach(_.apply())
  }
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

  /**
   * Is this RequestVar a member of a snapshot group?  If so, specify the group here
   */
  def snapshotGroup: Box[RequestVarSnapshotGroup] = Empty

  /**
   * Return a function that, when applied, will set the value of the RequestVar to its
   * current value
   */
  def snapshot(): () => Unit = {
    if (set_?) {
      val v = this.get
      () => this.set(v)
    } else {
      () => this.remove()
    }
  }

  override protected def findFunc(name: String): Box[T] = RequestVarHandler.get(name)

  override protected def setFunc(name: String, value: T): Unit = RequestVarHandler.set(name, this, value)

  override protected def clearFunc(name: String): Unit = RequestVarHandler.clear(name)

  override protected def wasInitialized(name: String, bn: String): Boolean = {
    val old: Boolean = RequestVarHandler.get(bn) openOr false
    RequestVarHandler.set(bn, this, true)
    old
  }

  /**
   * Different Vars require different mechanisms for synchronization.  This method implements
   * the Var specific synchronization mechanism
   */
  def doSync[F](f: => F): F = f

  // no sync necessary for RequestVars... always on the same thread

  override protected def testWasSet(name: String, bn: String): Boolean = {
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

  override protected def wasInitialized(name: String, bn: String): Boolean = {
    val old: Boolean = TransientRequestVarHandler.get(bn) openOr false
    TransientRequestVarHandler.set(bn, this, true)
    old
  }

  protected override def testWasSet(name: String, bn: String): Boolean = {
    TransientRequestVarHandler.get(name).isDefined || (TransientRequestVarHandler.get(bn) openOr false)
  }

  /**
   * Different Vars require different mechanisms for synchronization.  This method implements
   * the Var specific synchronization mechanism
   */
  def doSync[F](f: => F): F = f

  // no sync necessary for RequestVars... always on the same thread

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


  private[http] def instancesOfGroup(grp: RequestVarSnapshotGroup): List[MyType] = {
    val cmp = Full(grp)
    for {
      bs <- backingStore.toList
      (rv, _, _) <- bs.values if rv.snapshotGroup == cmp
    } yield rv
  }
}

private[http] object TransientRequestVarHandler extends CoreRequestVarHandler {
  type MyType = TransientRequestVar[_]
}

private[http] trait CoreRequestVarHandler {
  type MyType <: HasLogUnreadVal

  private val logger = Logger(classOf[CoreRequestVarHandler])
  // This maps from the RV name to (RV instance, value, set-but-not-read flag)
  private val vals: ThreadGlobal[ConcurrentHashMap[String, (MyType, Any, Boolean)]] = new ThreadGlobal
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

  protected def backingStore: Box[ConcurrentHashMap[String, (MyType, Any, Boolean)]] =
    vals.value match {
      case null =>
        if (LiftRules.throwOnOutOfScopeVarAccess) {
          throw new IllegalAccessException("Access to Var outside a request or comet actor scope")
        }
        None
      case x => Full(x)
    }

  private[http] def get[T](name: String): Box[T] =
    for {
      ht <- backingStore
      (rvInstance, value, unread) <- Box !! ht.get(name)
    } yield {
      if (unread) {
        // Flag the variable as no longer being set-but-unread
        ht(name) = (rvInstance: MyType, value.asInstanceOf[T], false)
      }
      value.asInstanceOf[T]
    }

  private[http] def set[T](name: String, from: MyType, value: T): Unit =
    for (ht <- backingStore)
      ht(name) = (from, value, true)

  private[http] def clear(name: String): Unit =
    for (ht <- backingStore)
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
      isIn.doWith("in")(
        vals.doWith(new ConcurrentHashMap)(
          cleanup.doWith(new ListBuffer) {
            sessionThing.doWith(session) {
              val ret: T = f

              cleanup.value.toList.foreach(clean => Helpers.tryo(clean(sessionThing.value)))

              if (Props.devMode && LiftRules.logUnreadRequestVars) {
                vals.value.keys.filter(!_.startsWith(VarConstants.varPrefix + "net.liftweb"))
                  .filter(!_.endsWith(VarConstants.initedSuffix))
                  .foreach(key => vals.value(key) match {
                  case (rv, _, true) if rv.logUnreadVal => logger.warn("RequestVar %s was set but not read".format(key.replace(VarConstants.varPrefix, "")))
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

  implicit def whatTransientRequestVarIs[T](in: TransientRequestVar[T]): T = in.is
}

/**
 * Memoize a value for the duration of the user's session
 */
abstract class SessionMemoize[K, V] extends MemoizeVar[K, V] {

  protected object coreVar extends SessionVar[LRU[K, Box[V]]](buildLRU) {
    override def __nameSalt = SessionMemoize.this.__nameSalt
  }

}

/**
 * Memoize a value for the duration of the current request (and subsequent Ajax requests made as a result of viewing the page)
 */
abstract class RequestMemoize[K, V] extends MemoizeVar[K, V] {

  protected object coreVar extends RequestVar[LRU[K, Box[V]]](buildLRU) {
    override def __nameSalt = RequestMemoize.this.__nameSalt
  }

}

/**
 * Memoize a value for the duration of the current HTTP request
 */
abstract class TransientRequestMemoize[K, V] extends MemoizeVar[K, V] {

  protected object coreVar extends TransientRequestVar[LRU[K, Box[V]]](buildLRU) {
    override def __nameSalt = TransientRequestMemoize.this.__nameSalt
  }

}
