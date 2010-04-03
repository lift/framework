/*
 * Copyright 2006-2010 WorldWide Conferencing, LLC
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
package machine {

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util.Helpers._
import _root_.scala.collection.mutable.{Queue, HashMap}
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.actor._

/**
 *  This trait manages state/workflow transition
 */
trait ProtoStateMachine[MyType <: ProtoStateMachine[MyType, StateType],
			StateType <: Enumeration] extends KeyedMapper[Long, MyType]
{
  self: MyType =>

  /**
    * Shorthand for one of the states
    */
  type StV = StateType#Value

  /**
    *  Shorthand for the meta state machine
    */
  type Meta = MetaProtoStateMachine[MyType, StateType]

  /**
    * the primary key for the database
    */
  object id extends MappedLongIndex[MyType](this)

  object inProcess extends MappedBoolean[MyType](this)

  /**
    * get the primary key field
    */
  override def primaryKeyField = id

  /**
    * Implement a method that returns the singleton
    */
  def getSingleton: Meta

  /**
    * The column in the database that stores the current state
    */
  object currentState extends MappedInt[MyType](this)

  /**
    * The column in the database that stores the next time an event should go off
    */
  object timedEventAt extends MappedLong[MyType](this)

  object nextTransitionAt extends MappedLong[MyType](this) with LifecycleCallbacks {
    override def beforeSave {if (this.is < System.currentTimeMillis) this.set(-1L)}
    override def dbIndexed_?  = true
  }

  def setupTime(when: TimeSpan) {
    val trigger = timedEventAt.is + when.millis
    if (trigger >= System.currentTimeMillis && (nextTransitionAt.is <= 0L || trigger < nextTransitionAt.is)) nextTransitionAt.set(trigger)
  }

  /**
    * Get the current state
    */
  def state: StateType#Value = getSingleton.stateEnumeration(currentState.is)


  /**
    * This method is called on a transition from one state to another.  Override this method
    * to perform an action.  Please call super to actually change the state and save the instance
    */
  def transition(from: StV, to: StV, why: Meta#Event): Unit = this.currentState(to.id).inProcess(false).save

  /**
    * This item has reached a terminating state.  This method will remove the
    * item from the database.  Override this method (please call super at the end of your method)
    * to do any cleanup.
    */
  def terminate(from: StV,to: StV,event: Meta#Event): Unit = {
    this.delete_!
  }

  def !(event: Meta#Event): Unit = processEvent(event)

  /**
    * Process an event
    */
  def processEvent(event: Meta#Event): Unit = {
    synchronized {
      eventQueue += event
    }

    def processIt {
      val toProcess = synchronized {
        if (_isProcessing || eventQueue.isEmpty) None
        else {_isProcessing = true; Some(eventQueue.dequeue)}
      }

      toProcess.foreach {
        event =>
        try {
          getSingleton.processEvent(this, event)
        } finally {
          synchronized {_isProcessing = false}
          processIt
        }
      }
    }

    processIt
  }

  private var _isProcessing = false
  private val eventQueue = new Queue[Meta#Event]
}

/**
  * A singleton that implements this trait will manage transitions, etc. for the state machine instance
  */
trait MetaProtoStateMachine [MyType <: ProtoStateMachine[MyType, StateType],
                             StateType <: Enumeration] extends KeyedMetaMapper[Long, MyType] with ProtoStateMachine[MyType, StateType] {
  self: MyType =>
  /**
    * This method must be implemented.  It defines the states and legal state transitions
    */
  protected def states : List[Meta#State];

  /**
    * Any transitions that are applied to all states can be listed here
    */
  protected def globalTransitions: List[Meta#ATransition]

  // the state transition table
  private val stateInfo = new HashMap[StV, Seq[Meta#ATransition]]
  private val stateList = new HashMap[StV, Meta#State]

  // process the states
  states.foreach {
    st =>
    if (stateList.get(st.name).isDefined) throw new DuplicateStateException("State "+st.name+" is defined twice")
    stateInfo(st.name) = st.trans ++ globalTransitions
    stateList(st.name) = st
  }


  /**
    * The default initial state
    */
  def initialState : StV

  /**
    * The enumeration of states
    */
  def stateEnumeration: StateType

  /**
    *  Terminate an instance
    */
  def terminate(what: MyType,from: StV,to: StV,event: Meta#Event) {what.terminate(from, to, event)}


  protected def instantiate: MyType

  def newInstance(firstEvent: Meta#Event): MyType = createNewInstance(firstEvent, Empty)
  def createNewInstance(firstEvent: Meta#Event, setup: Box[MyType => Unit]): MyType = {
    val state = instantiate
    setup.foreach(_(state))
    state.processEvent(firstEvent)
    state
  }

  def createNewInstance(firstEvent: Meta#Event)(setup: MyType => Unit): MyType = createNewInstance(firstEvent, Full(setup))



  /**
    *  Process an event for an instance
    */
  protected def processEvent(who: MyType, what: Meta#Event) {
    val transitions = stateInfo(who.state) // get the transitions
    val which = first(transitions.toList) {t => if (t.on.isDefinedAt(what) && t.testGuard(who, who.state, t.to, what)) Full(t) else Empty}

    if (!which.isDefined) {
      val s = stateList(who.state)
      what.unmatchedEventHandler(who, s)
    }

    which.foreach {
      t =>
      val to = t.to
      val old = who.state
      stateList.get(old).foreach(_.performExit(who, old, to, what))
      t.performAction(who, old, to, what)
      who.timedEventAt(System.currentTimeMillis)
      who.nextTransitionAt(-1L)
      stateList.get(to).foreach(_.performSetup(who))
      who.transition(old, to, what)
      stateList.get(to).foreach(_.performEntry(who, old, to, what))
    }
  }

  class State(val name: StV,val trans: Seq[Meta#ATransition]) {
    def entry(act: (MyType, StV, StV, Meta#Event) => Any): Meta#State = {_entry = act :: _entry; this}
    def exit(act: (MyType, StV, StV, Meta#Event) => Any): Meta#State = {_exit = act :: _exit; this}
    private var _entry: List[(MyType, StV, StV, Meta#Event) => Any] = Nil
    private var _exit: List[(MyType, StV, StV, Meta#Event) => Any] = Nil

    def performSetup(who: MyType) = trans.foreach(_.performSetup(who, name))

    def performEntry(who: MyType, from: StV, to: StV, why: Meta#Event) {_entry.foreach(e => e(who, from, to, why))}
    def performExit(who: MyType, from: StV, to: StV, why: Meta#Event) {_exit.foreach(e => e(who, from, to, why))}
  }

  object State {
    def apply(name: StV, trans: Meta#ATransition*) = new State(name, trans)
  }

  abstract class ATransition(val to: StV,val on: PartialFunction[Meta#Event, Any]) {
    def testGuard(who: MyType, from: StV, to: StV, what: Meta#Event): Boolean =
      _guard.isEmpty || _guard.exists(_(who, from, to, what))

    def performAction(who: MyType, from: StV, to: StV, what: Meta#Event) {
      _action.foreach(_(who, from, to, what))
    }

    def performSetup(who: MyType, to: StV): Unit = _setup.foreach(_(who, to))

    private var _setup: List[(MyType, StV) => Any] = Nil
    private var _action: List[(MyType, StV, StV, Meta#Event) => Any] = Nil
    private var _guard: List[(MyType, StV, StV, Meta#Event) => Boolean] = Nil
    def action(act: (MyType, StV, StV, Meta#Event) => Any): this.type = {_action = act :: _action; this}
    def guard(gurd: (MyType, StV, StV, Meta#Event) => Boolean): this.type = {_guard = gurd :: _guard; this}
    def setup(setp: (MyType, StV) => Any): this.type = {_setup = setp :: _setup; this}
  }

  // case class TimeTransition(to: StV, time: TimeSpan) extends Transition
  case class After(when: TimeSpan, override val to: StV) extends ATransition(to, {case TimerEvent(len) if (when.millis <= len.millis) => true}) {
    setup ((what, state) => what.setupTime(when))
  }

  case class TimerEvent(len: TimeSpan) extends Event {
    /**
       * An unhandled event has occurred.  By default, throw an exception.  However,
       * you can override this method (and not call "super") to log issues or do
       * something else
       */
     override def unmatchedEventHandler(who: MyType, state: Meta#State) {
       who.nextTransitionAt(-1L)
       state.trans.foreach {
         to =>
         to match {
           case After(when,_) => who.setupTime(when)
           case _ =>
         }
       }
       if (who.nextTransitionAt.is == -1) super.unmatchedEventHandler(who, state)
       else who.inProcess(false).save
     }
  }

  /// case class FirstTransition extends Event

  case class On(override val on: PartialFunction[Meta#Event, Any], override val to: StV) extends ATransition(to, on)

  object Event {
    def unmatchedHandler: Box[(MyType, Meta#State, Event) => Any] = Empty
    def unmatchedEventHandler(who: MyType, state: Meta#State, event: Event) {
      val f = unmatchedHandler openOr ((who: MyType,state: Meta#State,what: Event) => throw new UnmatchedEventException("Event "+what+" was not matched at state "+who.state, who, what))
      f(who, state, event)
    }
  }

  abstract class Event {
    /**
       * An unhandled event has occurred.  By default, throw an exception.  However,
       * you can override this method (and not call "super") to log issues or do
       * something else
       */
     def unmatchedEventHandler(who: MyType, state: Meta#State) {
       Event.unmatchedEventHandler(who, state, this) // throw new UnmatchedEventException("Event "+this+" was not matched at state "+who.state, who, this)
     }

  }

  class DuplicateStateException(msg: String) extends Exception(msg)

  class UnmatchedEventException(msg: String,val who: MyType,
                                    val what: Event)  extends Exception(msg)

  /**
    * How long to wait to start looking for timed events.  Override this method to
    * specify a time
    */
  def timedEventInitialWait = 120000L

  /**
    * After the initial test, how long do we wait
    */
  def timedEventPeriodicWait = 10000L

  private class TimedEventManager(val metaOwner: Meta) extends LiftActor with Loggable {
    ActorPing.schedule(this, Ping, TimeSpan(timedEventInitialWait)) // give the system 2 minutes to "warm up" then start pinging

    protected def messageHandler = 
      {
        case Ping =>
          val now = System.currentTimeMillis
        try {
          val name = metaOwner.nextTransitionAt.dbColumnName
          metaOwner.findAll(By(metaOwner.inProcess, false),
			    BySql(name+" > 0 AND "+name+" <= ?",
				  IHaveValidatedThisSQL("dpp", "2008/12/03"),
				  now)).foreach {
			      stateItem =>
				stateItem.inProcess(true).save
			      val event = TimerEvent(TimeSpan(now - stateItem.timedEventAt.is))
			      timedEventHandler ! (stateItem, event)
			    }
        } catch {
          case e => logger.error("State machine loop", e)
        }
        ActorPing.schedule(this, Ping, TimeSpan(timedEventPeriodicWait))
      }
    
    case object Ping
  }
			       
    private class TimedEventHandler(val metaOwner: Meta) extends LiftActor with Loggable {
      protected def messageHandler =
        {
          case (item: MyType, event: Event) =>
            try {
              item.processEvent(event)
            } catch {
              case e => logger.error("Timed Event Handler"+e)
            }
        }

      case object Ping
    }

  val timedEventManager: LiftActor = {
    val ret = new TimedEventManager(getSingleton)
    ret
  }

  val timedEventHandler: LiftActor = {
    val ret = new TimedEventHandler(getSingleton)
    ret
  }
}

}
}
