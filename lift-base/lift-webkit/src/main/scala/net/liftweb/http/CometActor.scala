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
import _root_.net.liftweb.actor._
import _root_.scala.collection.mutable.{ListBuffer}
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util._
import _root_.net.liftweb.json._
import _root_.scala.xml.{NodeSeq, Text, Elem, Node, Group, Null, PrefixedAttribute, UnprefixedAttribute}
import _root_.scala.collection.immutable.TreeMap
import _root_.scala.collection.mutable.{HashSet, ListBuffer}
import _root_.net.liftweb.http.js._
import JsCmds._
import JE._
import _root_.java.util.concurrent.atomic.AtomicLong

/**
 * An actor that monitors other actors that are linked with it. If a watched
 * actor terminates,this actor captures the Exit messag, executes failureFuncs
 * and resurects the actor.
 */
object ActorWatcher extends scala.actors.Actor with Loggable {
  import scala.actors.Actor._
  def act = loop {
    react {
      case scala.actors.Exit(actor: scala.actors.Actor, why: Throwable) =>
        failureFuncs.foreach(f => tryo(f(actor, why)))

      case _ =>
    }
  }

  private def startAgain(a: scala.actors.Actor, ignore: Throwable) {
    a.start
    a ! RelinkToActorWatcher
  }

  private def logActorFailure(actor: scala.actors.Actor, why: Throwable) {
    logger.warn("The ActorWatcher restarted " + actor + " because " + why, why)
  }

  /**
   * If there's something to do in addition to starting the actor up, pre-pend the
   * actor to this List
   */
  @volatile var failureFuncs: List[(scala.actors.Actor, Throwable) => Unit] = logActorFailure _ ::
          startAgain _ :: Nil

  this.trapExit = true
  this.start
}

/**
 * This is used as an indicator message for linked actors.
 *
 * @see ActorWatcher
 */
case object RelinkToActorWatcher

trait DeltaTrait {
  def toJs: JsCmd
}

trait CometState[DeltaType <: DeltaTrait,
MyType <: CometState[DeltaType, MyType]] {
  self: MyType =>

  def -(other: MyType): Seq[DeltaType]

  def render: NodeSeq
}

trait CometStateWithUpdate[UpdateType, DeltaType <: DeltaTrait,
MyType <: CometStateWithUpdate[UpdateType,
        DeltaType, MyType]]
        extends CometState[DeltaType, MyType]
{
  self: MyType =>
  def process(in: UpdateType): MyType
}

trait StatefulComet extends CometActor {
  type Delta <: DeltaTrait
  type State <: CometState[Delta, State]

  /**
   * Test the parameter to see if it's an updated state object
   */
  def testState(in: Any): Box[State]

  /**
   * Return the empty state object
   */
  def emptyState: State

  /**
   * The current state objects
   */
  protected var state: State = emptyState

  /**
   * If there's some ThreadLocal variable that needs to be setup up
   * before processing the state deltas, set it up here.
   */
  protected def setupLocalState[T](f: => T): T = f

  private[http] override val _lowPriority = {
    val pf: PartialFunction[Any, Unit] = {
      case v if testState(v).isDefined =>
        testState(v).foreach {
          ns =>
                  if (ns ne state) {
                    val diff = ns - state
                    state = ns
                    partialUpdate(setupLocalState {diff.map(_.toJs).foldLeft(Noop)(_ & _)})
                  }
        }
    }

    pf orElse super._lowPriority
  }

  /**
   * The Render method
   */
  def render = state.render
}

object CurrentCometActor extends ThreadGlobal[Box[LiftCometActor]] {
  this.set(Empty)
}

object AddAListener {
  def apply(who: SimpleActor[Any]) = new AddAListener(who, { case _ => true } )
}

/**
 * This is a message class for use with ListenerManager and CometListener
 * instances. The use of the shouldUpdate function is deprecated, and
 * should instead be handled by the message processing partial functions
 * on the CometListner instances themselves.
 *
 * @see CometListener
 * @see ListenerManager
 */
case class AddAListener(who: SimpleActor[Any], shouldUpdate: PartialFunction[Any, Boolean])

/**
 * This is a message class for use with ListenerManager and CometListener
 * instances.
 *
 * @see CometListener
 * @see ListenerManager
 */
case class RemoveAListener(who: SimpleActor[Any])


object ListenerManager {
  type ActorTest = (SimpleActor[Any], PartialFunction[Any, Boolean])
}


/**
 * This trait manages a set of Actors in a publish/subscribe pattern. When you extend your Actor with
 * this trait, you automatically get handling for sending messages out to all subscribed Actors. Simply
 * override the high-, medium-, or lowPriority handlers to do your message processing. When you want to update
 * all subscribers, just call the updateListeners method. The createUpdate method is used to generate
 * the message that you want sent to all subscribers.
 *
 * Note that the AddAListener and RemoveAListener messages (for subscription control) are processed
 * after any highPriority or mediumPriority messages are processed, so take care to avoid overly
 * broad matches in those handlers that might consume internal messages.
 *
 * For example, you could write a simple service to provide clock ticks using the following code:
 *
 * <pre name="code" class="scala">
 * case object Tick
 * 
 * object Ticker extends ListenerManager {
 *   import net.liftweb.util.ActorPing
 * 
 *   // Set up the initial tick
 *   ActorPing.schedule(this, Tick, 1000L)
 *
 *   // This is a placeholder, since we're only interested
 *   // in Ticks
 *   def createUpdate = "Registered"
 * 
 *   override def mediumPriority = {
 *     case Tick => {
 *       updateListeneres(Tick)
 *       ActorPing.schedule(this, Tick, 1000L)
 *     }
 *   }
 * }
 * </pre>
 *
 * A client CometActor could look like:
 *
 * <pre name="code" class="scala">
 * class CometClock extends CometListener {
 *   val registerWith = Ticker
 *
 *   ... handling code ...
 * }
 * </pre>
 *
 * @see CometListener
 *   
 */
trait ListenerManager {
  self: SimpleActor[Any] =>

  import ListenerManager._

  /**
   * This is the list of all registered actors
   */
  private var listeners: List[ActorTest] = Nil

  protected def messageHandler: PartialFunction[Any, Unit] =
    highPriority orElse mediumPriority orElse
            listenerService orElse lowPriority

  protected def listenerService: PartialFunction[Any, Unit] =
    {
      case AddAListener(who, shouldUpdate) =>
        val pair = (who, shouldUpdate)
        listeners ::= pair
        updateIfPassesTest(createUpdate)(pair)

      case RemoveAListener(who) =>
        listeners = listeners.filter(_._1 ne who)
    }

  /**
   * Update the listeners with the message generated by createUpdate
   */
  protected def updateListeners() {
    val update = updateIfPassesTest(createUpdate) _
    listeners foreach update
  }

  /**
   * Update the listeners with a message that we create. Note that
   * with this invocation the createUpdate method is not used.
   */
  protected def updateListeners(msg: Any) {
    listeners foreach (_._1 ! msg)
  }

  /**
   * This method provides legacy functionality for filtering messages
   * before sending to each registered actor. It is deprecated in
   * favor of doing the filtering in the registered Actor's
   * message handling partial functions instead.
   */
  @deprecated("Accept/reject logic should be done in the partial function that handles the message.")
  protected def updateIfPassesTest(update: Any)(info: ActorTest) {
    info match {
      case (who, test) => if (test.isDefinedAt(update) && test(update)) who ! update
    }
  }

  /**
   * This method is called when the <pre>updateListeners()</pre> method
   * needs a message to send to subscribed Actors. In particular, createUpdate
   * is used to create the first message that a newly subscribed CometListener
   * will receive.
   */
  protected def createUpdate: Any

  /**
   * Override this method to process high priority messages. Note:
   * <b>you must not process messages with a wildcard (match all)</b>, since
   * this will intercept the messages used for subscription control.
   */
  protected def highPriority: PartialFunction[Any, Unit] = Map.empty

  /**
   * Override this method to process medium priority messages. See
   * the highPriority method for an important note on wildcard
   * processing.
   *
   * @see #highPriority
   */
  protected def mediumPriority: PartialFunction[Any, Unit] = Map.empty

  /**
   * Override this method to process low priority messages.
   */
  protected def lowPriority: PartialFunction[Any, Unit] = Map.empty
}

/**
 * This is a legacy trait, left over from Lift's
 * Scala 2.7 support. You should use or migrate to
 * CometListener instead.
 *
 * @see CometListener
 */
@deprecated("Use the CometListener trait instead.")
trait CometListenee extends CometListener {
  self: CometActor =>
}

/**
 * This trait adds functionality to automatically register with a given
 * Actor using AddAListener and RemoveAListener control messages. The most
 * typical usage would be to register with an instance of ListenerManager.
 * You will need to provide a def/val for the <pre>registerWith</pre> member
 * to control which Actor to connect to.
 *
 * See ListenerManager for a complete example.
 *
 * @see ListenerManager
 */
trait CometListener extends CometActor {
  self: CometActor =>

  /**
   * This controls which Actor to register with for updates. Typically
   * this will be an instance of ListenerActor, although you can provide
   * your own subscription handling on top of any SimpleActor.
   */
  protected def registerWith: SimpleActor[Any]

  /**
   * Override this in order to selectively update listeners based on the given message.
   * This method has been deprecated because it's executed in a seperate context from
   * the session's context.  This causes problems.  Accept/reject logic should be done
   * in the partial function that handles the message.
   */
  @deprecated("Accept/reject logic should be done in the partial function that handles the message.")
  protected def shouldUpdate: PartialFunction[Any, Boolean] = { case _ => true}

  abstract override protected def localSetup() {
    registerWith ! AddAListener(this, shouldUpdate)
    super.localSetup()
  }

  abstract override protected def localShutdown() {
    registerWith ! RemoveAListener(this)
    super.localShutdown()
  }
}

trait LiftCometActor extends TypedActor[Any, Any] with ForwardableActor[Any, Any] {
  def uniqueId: String

  private[http] def callInitCometActor(theSession: LiftSession,
                                       theType: Box[String],
                                       name: Box[String],
                                       defaultXml: NodeSeq,
                                       attributes: Map[String, String]) {
    initCometActor(theSession, theType, name, defaultXml, attributes)
  }

  protected def initCometActor(theSession: LiftSession,
                               theType: Box[String],
                               name: Box[String],
                               defaultXml: NodeSeq,
                               attributes: Map[String, String]): Unit

  def jsonCall: JsonCall

  def theType: Box[String]

  def name: Box[String]

  def hasOuter: Boolean

  def buildSpan(time: Long, xml: NodeSeq): NodeSeq

  def parentTag: Elem
}

/**
 * Takes care of the plumbing for building Comet-based Web Apps
 */
@serializable
trait CometActor extends LiftActor with LiftCometActor with BindHelpers {
  private val logger = Logger(classOf[CometActor])
  val uniqueId = Helpers.nextFuncName
  private var spanId = uniqueId
  private var lastRenderTime = Helpers.nextNum

  /**
   * If we're going to cache the last rendering, here's the
   * private cache
   */
  private[this] var _realLastRendering: RenderOut = _

  /**
   * The last rendering (cached or not)
   */
  private def lastRendering: RenderOut = 
    if (dontCacheRendering) {
      val ret = (render ++ jsonInCode): RenderOut
      theSession.updateFunctionMap(S.functionMap, spanId, lastRenderTime)
      ret
    } else {
      _realLastRendering
    }

  /**
   * set the last rendering... ignore if we're not caching
   */
  private def lastRendering_=(last: RenderOut) {
    if (!dontCacheRendering) {
      _realLastRendering = last
    }
  }
  
  private var wasLastFullRender = false
  @transient
  private var listeners: List[(ListenerId, AnswerRender => Unit)] = Nil
  private var askingWho: Box[LiftCometActor] = Empty
  private var whosAsking: Box[LiftCometActor] = Empty
  private var answerWith: Box[Any => Any] = Empty
  private var deltas: List[Delta] = Nil
  private var jsonHandlerChain: PartialFunction[Any, JsCmd] = Map.empty
  private val notices = new ListBuffer[(NoticeType.Value, NodeSeq, Box[String])]
  private var lastListenTime = millis

  private var _theSession: LiftSession = _

  def theSession = _theSession

  @volatile private var _defaultXml: NodeSeq = _

  def defaultXml = _defaultXml

  private var _name: Box[String] = Empty

  def name = _name

  private var _theType: Box[String] = Empty

  def theType = _theType

  private var _attributes: Map[String, String] = Map.empty

  def attributes = _attributes

  def lifespan: Box[TimeSpan] = Empty

  private var _running = true

  private var _shutDownAt = millis

  /**
   * Is the CometActor running?
   */
  protected def running = _running

  protected def initCometActor(theSession: LiftSession,
                               theType: Box[String],
                               name: Box[String],
                               defaultXml: NodeSeq,
                               attributes: Map[String, String]) {
    if (!dontCacheRendering) {
      lastRendering = RenderOut(Full(defaultXml),
                                Empty, Empty, Empty, false)
    }

    this._theType = theType
    this._theSession = theSession
    this._defaultXml = defaultXml
    this._name = name
    this._attributes = attributes
  }

  def defaultPrefix: Box[String] = Empty

  private lazy val _defaultPrefix: String = (defaultPrefix or _name) openOr "comet"

  /**
   * Set to 'true' if we should run "render" on every page load
   */
  protected def devMode = false

  def hasOuter = true

  def parentTag = <div style="display: inline"/>

  private def _handleJson(in: Any): JsCmd =
    if (jsonHandlerChain.isDefinedAt(in))
      jsonHandlerChain(in)
    else handleJson(in)


  /**
   * Prepends the handler to the Json Handlers.  Should only be used
   * during instantiation
   *
   * @param h -- the PartialFunction that can handle a JSON request
   */
  def appendJsonHandler(h: PartialFunction[Any, JsCmd]) {
    jsonHandlerChain = h orElse jsonHandlerChain
  }


  def handleJson(in: Any): JsCmd = Noop

  /**
   * If there's actor-specific JSON behavior on failure to make the JSON
   * call, include the JavaScript here.
   */
  def onJsonError: Box[JsCmd] = Empty

  lazy val (jsonCall, jsonInCode) = S.buildJsonFunc(Full(_defaultPrefix), onJsonError, _handleJson)
  
  /**
  * Override this method to deal with JSON sent from the browser via the sendJson function.  This
  * is based on the Lift JSON package rather than the handleJson stuff based on the older util.JsonParser.  This
  * is the prefered mechanism.  If you use the jsonSend call, you will get a JObject(JField("command", cmd), JField("param", params))
  */
  def receiveJson: PartialFunction[JsonAST.JValue, JsCmd] = Map()

  /**
  * The JavaScript call that you use to send the data to the server. For example:
  * &lt;button onclick={jsonSend("Hello", JsRaw("Dude".encJs))}&gt;Click&lt;/button&gt;
  */
  def jsonSend: JsonCall = _sendJson

  /**
  * The call that packages up the JSON and tosses it to the server.  If you set autoIncludeJsonCode to true,
  * then this will be included in the stuff sent to the server.
  */
  def jsonToIncludeInCode: JsCmd = _jsonToIncludeCode

  private lazy val (_sendJson, _jsonToIncludeCode) = S.createJsonFunc(Full(_defaultPrefix), onJsonError, receiveJson _)

  /**
  * Set this method to true to have the Json call code included in the Comet output
  */
  def autoIncludeJsonCode: Boolean = false

  /**
   * Creates the span element acting as the real estate for commet rendering.
   */
  def buildSpan(time: Long, xml: NodeSeq): NodeSeq = {
    Elem(parentTag.prefix, parentTag.label, parentTag.attributes,
         parentTag.scope, Group(xml)) %
    new UnprefixedAttribute("id", 
                            Text(spanId), 
                            if (time > 0L) {
                              new PrefixedAttribute("lift", "when", 
                                                    time.toString, 
                                                    Null)
                            } else {Null})
  }

  def messageHandler = {
    val what = composeFunction
    val myPf: PartialFunction[Any, Unit] = new PartialFunction[Any, Unit] {
      def apply(in: Any): Unit =
        CurrentCometActor.doWith(Full(CometActor.this)) {
          S.initIfUninitted(theSession) {
            S.functionLifespan(true) {
              what.apply(in)
              if (S.functionMap.size > 0) {
                theSession.updateFunctionMap(S.functionMap,
                                             uniqueId, lastRenderTime)
                S.clearFunctionMap
              }
            }
          }
        }
      
      def isDefinedAt(in: Any): Boolean =
        CurrentCometActor.doWith(Full(CometActor.this)) {
          S.initIfUninitted(theSession) {
            S.functionLifespan(true) {
              what.isDefinedAt(in)
            }
          }
        }
    }
    
    myPf
  }

  def fixedRender: Box[NodeSeq] = Empty

  def highPriority: PartialFunction[Any, Unit] = Map.empty

  def lowPriority: PartialFunction[Any, Unit] = Map.empty

  def mediumPriority: PartialFunction[Any, Unit] = Map.empty

  private[http] def _lowPriority: PartialFunction[Any, Unit] = {
    case s => logger.debug("CometActor " + this + " got unexpected message " + s)
  }

  private lazy val _mediumPriority: PartialFunction[Any, Unit] = {
    case l@Unlisten(seq) =>
      lastListenTime = millis
      askingWho match {
        case Full(who) => forwardMessageTo(l, who) // forward l
        case _ => listeners = listeners.filter(_._1 != seq)
      }

    case l@Listen(when, seqId, toDo) =>
      lastListenTime = millis
      askingWho match {
        case Full(who) => forwardMessageTo(l, who) // who forward l
        case _ =>
          if (when < lastRenderTime) {
            toDo(AnswerRender(new XmlOrJsCmd(spanId, lastRendering,
              buildSpan _, notices toList),
              whosAsking openOr this, lastRenderTime, wasLastFullRender))
            clearNotices
          } else {
            deltas.filter(_.when > when) match {
              case Nil => listeners = (seqId, toDo) :: listeners

              case all@(hd :: xs) =>
                toDo(AnswerRender(new XmlOrJsCmd(spanId, Empty, Empty,
                  Full(all.reverse.foldLeft(Noop)(_ & _.js)), Empty, buildSpan, false, notices toList),
                  whosAsking openOr this, hd.when, false))
                clearNotices
            }
          }
      }

    case PerformSetupComet =>
      // this ! RelinkToActorWatcher
      localSetup
      performReRender(true)

    /**
     * Update the defaultXml... sent in dev mode
     */
    case UpdateDefaultXml(xml) => {
      val redo = xml != _defaultXml
      
      _defaultXml = xml

      if (redo) performReRender(false)
    }

    case AskRender =>
      askingWho match {
        case Full(who) => forwardMessageTo(AskRender, who) //  forward AskRender
        case _ => if (!deltas.isEmpty || devMode) performReRender(false);
        reply(AnswerRender(new XmlOrJsCmd(spanId, lastRendering, buildSpan _, notices toList),
          whosAsking openOr this, lastRenderTime, true))
        clearNotices
      }

    case ActionMessageSet(msgs, req) =>
      S.doCometParams(req.params) {
        S.functionLifespan(true) {
          reply(msgs.map(_()))
        }
      }

    case AskQuestion(what, who, otherlisteners) =>
      this.spanId = who.uniqueId
      this.listeners = otherlisteners ::: this.listeners
      startQuestion(what)
      whosAsking = Full(who)
      this.reRender(true)


    case AnswerQuestion(what, otherListeners) =>
      S.functionLifespan(true) {
        askingWho.foreach {
          ah =>
                  reply("A null message to release the actor from its send and await reply... do not delete this message")
                  // askingWho.unlink(self)
                  ah ! ShutDown
                  this.listeners = this.listeners ::: otherListeners
                  this.askingWho = Empty
                  val aw = answerWith
                  answerWith = Empty
                  aw.foreach(_(what))
                  performReRender(true)
        }
      }

    case ShutdownIfPastLifespan =>
      for{
        ls <- lifespan if (lastListenTime + ls.millis) < millis
      } this ! ShutDown

    case ReRender(all) => performReRender(all)

    case Error(id, node) => notices += ((NoticeType.Error, node, id))

    case Warning(id, node) => notices += ((NoticeType.Warning, node, id))

    case Notice(id, node) => notices += ((NoticeType.Notice, node, id))

    case ClearNotices => clearNotices

    case ShutDown =>
      logger.info("The CometActor " + this + " Received Shutdown")
      askingWho.foreach(_ ! ShutDown)
      theSession.removeCometActor(this)
      _localShutdown()

    case PartialUpdateMsg(cmdF) =>
      val cmd: JsCmd = cmdF.apply
      val time = Helpers.nextNum
      val delta = JsDelta(time, cmd)
      theSession.updateFunctionMap(S.functionMap, uniqueId, time)
      S.clearFunctionMap
      val m = millis
      deltas = (delta :: deltas).filter(d => (m - d.timestamp) < 120000L)
      if (!listeners.isEmpty) {
        val rendered = AnswerRender(new XmlOrJsCmd(spanId, Empty, Empty,
          Full(cmd), Empty, buildSpan, false, notices toList),
          whosAsking openOr this, time, false)
        clearNotices
        listeners.foreach(_._2(rendered))
        listeners = Nil
      }
  }


  /**
   * It's the main method to override, to define what is rendered by the CometActor
   *
   * There are implicit conversions for a bunch of stuff to
   * RenderOut (including NodeSeq).  Thus, if you don't declare the return
   * turn to be something other than RenderOut and return something that's
   * coersable into RenderOut, the compiler "does the right thing"(tm) for you.
   * <br/>
   * There are implicit conversions for NodeSeq, so you can return a pile of
   * XML right here.  There's an implicit conversion for NodeSeq => NodeSeq,
   * so you can return a function (e.g., a CssBindFunc) that will convert
   * the defaultXml to the correct output.  There's an implicit conversion
   * from JsCmd, so you can return a pile of JavaScript that'll be shipped
   * to the browser.
   */
  def render: RenderOut

  /**
   * Cause the entire component to be reRendered and pushed out
   * to any listeners.
   *
   * @param sendAll -- Should the fixed part of the CometActor be
   * rendered.
   */
  def reRender(sendAll: Boolean) {
    this ! ReRender(sendAll)
  }

  /**
   * Cause the entire component to be reRendered and pushed out
   * to any listeners.
   */
  def reRender() {reRender(false)}


  /**
   * Set this method to true if you want to avoid caching the
   * rendering.  This trades space for time.
   */
  protected def dontCacheRendering: Boolean = false

  private def performReRender(sendAll: Boolean) {
    lastRenderTime = Helpers.nextNum
    wasLastFullRender = sendAll & hasOuter
    deltas = Nil

    if (!dontCacheRendering) {
      lastRendering = render ++ jsonInCode
    }

    theSession.updateFunctionMap(S.functionMap, spanId, lastRenderTime)

    val rendered: AnswerRender =
    AnswerRender(new XmlOrJsCmd(spanId, lastRendering, buildSpan _, notices toList),
      this, lastRenderTime, sendAll)

    clearNotices
    listeners.foreach(_._2(rendered))
    listeners = Nil
  }

  def unWatch = partialUpdate(Call("liftComet.lift_unlistWatch", uniqueId))

  protected def partialUpdate(cmd: => JsCmd) {
    this ! PartialUpdateMsg(() => cmd)
  }

  protected def startQuestion(what: Any) {}

  /**
   * This method will be called after the Actor has started.  Do any setup here
   */
  protected def localSetup(): Unit = {}

  private def _localShutdown() {
    localShutdown()
    clearNotices
    listeners = Nil
    askingWho = Empty
    whosAsking = Empty
    deltas = Nil
    jsonHandlerChain = Map.empty
    _running = false
    _shutDownAt = millis
  }

  /**
   * This method will be called as part of the shut-down of the actor.  Release any resources here.
   */
  protected def localShutdown(): Unit = {}

  protected def composeFunction = composeFunction_i

  private def composeFunction_i = {
    // if we're no longer running don't pass messages to the other handlers
    // just pass them to our handlers
    if (!_running && (millis - 20000L) > _shutDownAt) 
      _mediumPriority orElse _lowPriority
    else 
      highPriority orElse mediumPriority orElse 
    _mediumPriority orElse lowPriority orElse _lowPriority
  }

  def bind(prefix: String, vals: BindParam*): NodeSeq = bind(prefix, _defaultXml, vals: _*)

  def bind(vals: BindParam*): NodeSeq = bind(_defaultPrefix, vals: _*)

  protected def ask(who: LiftCometActor, what: Any)(answerWith: Any => Unit) {
    who.callInitCometActor(theSession, Full(who.uniqueId), name, defaultXml, attributes)
    theSession.addCometActor(who)
    // who.link(this)
    who ! PerformSetupComet
    askingWho = Full(who)
    this.answerWith = Full(answerWith)
    who ! AskQuestion(what, this, listeners)
    // this ! AskRender
  }

  protected def answer(answer: Any) {
    whosAsking.foreach(_ !? AnswerQuestion(answer, listeners))
    whosAsking = Empty
    performReRender(false)
  }

  protected implicit def nsToNsFuncToRenderOut(f: NodeSeq => NodeSeq) =
    new RenderOut((Box !! defaultXml).map(f), fixedRender, if (autoIncludeJsonCode) Full(jsonToIncludeInCode & S.jsToAppend()) else {
      S.jsToAppend match {
        case Nil => Empty
        case x :: Nil => Full(x)
        case xs => Full(xs.reduceLeft(_ & _))
      }
      }, Empty, false)

  protected implicit def arrayToRenderOut(in: Seq[Node]): RenderOut = new RenderOut(Full(in: NodeSeq), fixedRender, if (autoIncludeJsonCode) Full(jsonToIncludeInCode & S.jsToAppend()) else {
      S.jsToAppend match {
        case Nil => Empty
        case x :: Nil => Full(x)
        case xs => Full(xs.reduceLeft(_ & _))
      }
      }, Empty, false)

  protected implicit def jsToXmlOrJsCmd(in: JsCmd): RenderOut = new RenderOut(Empty, Empty, if (autoIncludeJsonCode) Full(in & jsonToIncludeInCode & S.jsToAppend()) else Full(in & S.jsToAppend()), Empty, false)

  implicit def pairToPair(in: (String, Any)): (String, NodeSeq) = (in._1, Text(in._2 match {case null => "null" case s => s.toString}))

  implicit def nodeSeqToFull(in: NodeSeq): Box[NodeSeq] = Full(in)

  implicit def elemToFull(in: Elem): Box[NodeSeq] = Full(in)

  /**
   * Similar with S.error
   */
  def error(n: String) {error(Text(n))}

  /**
   * Similar with S.error
   */
  def error(n: NodeSeq) {notices += ((NoticeType.Error, n, Empty))}

  /**
   * Similar with S.error
   */
  def error(id: String, n: NodeSeq) {notices += ((NoticeType.Error, n, Full(id)))}

  /**
   * Similar with S.error
   */
  def error(id: String, n: String) {error(id, Text(n))}

  /**
   * Similar with S.notice
   */
  def notice(n: String) {notice(Text(n))}

  /**
   * Similar with S.notice
   */
  def notice(n: NodeSeq) {notices += ((NoticeType.Notice, n, Empty))}

  /**
   * Similar with S.notice
   */
  def notice(id: String, n: NodeSeq) {notices += ((NoticeType.Notice, n, Full(id)))}

  /**
   * Similar with S.notice
   */
  def notice(id: String, n: String) {notice(id, Text(n))}

  /**
   * Similar with S.warning
   */
  def warning(n: String) {warning(Text(n))}

  /**
   * Similar with S.warning
   */
  def warning(n: NodeSeq) {notices += ((NoticeType.Warning, n, Empty))}

  /**
   * Similar with S.warning
   */
  def warning(id: String, n: NodeSeq) {notices += ((NoticeType.Warning, n, Full(id)))}

  /**
   * Similar with S.warning
   */
  def warning(id: String, n: String) {warning(id, Text(n))}

  private def clearNotices {notices clear}

}

abstract class Delta(val when: Long) {
  def js: JsCmd

  val timestamp = millis
}

case class JsDelta(override val when: Long, js: JsCmd) extends Delta(when)

sealed abstract class CometMessage

/**
 * Impersonates the actual comet response content
 */
private[http] class XmlOrJsCmd(val id: String,
                               _xml: Box[NodeSeq],
                               _fixedXhtml: Box[NodeSeq],
                               val javaScript: Box[JsCmd],
                               val destroy: Box[JsCmd],
                               spanFunc: (Long, NodeSeq) => NodeSeq,
                               ignoreHtmlOnJs: Boolean,
                               notices: List[(NoticeType.Value, NodeSeq, Box[String])]) {
  def this(id: String, ro: RenderOut, spanFunc: (Long, NodeSeq) => NodeSeq, notices: List[(NoticeType.Value, NodeSeq, Box[String])]) =
    this (id, ro.xhtml, ro.fixedXhtml, ro.script, ro.destroyScript, spanFunc, ro.ignoreHtmlOnJs, notices)

  val xml = _xml.flatMap(content => S.session.map(s => s.processSurroundAndInclude("JS SetHTML id: " + id, content)))
  val fixedXhtml = _fixedXhtml.flatMap(content => S.session.map(s => s.processSurroundAndInclude("JS SetHTML id: " + id, content)))

  /**
   * Returns the JsCmd that will be sent to client
   */
  def toJavaScript(session: LiftSession, displayAll: Boolean): JsCmd = {
    var ret: JsCmd = JsCmds.JsTry(JsCmds.Run("destroy_" + id + "();"), false) &
            ((if (ignoreHtmlOnJs) Empty else xml, javaScript, displayAll) match {
              case (Full(xml), Full(js), false) => LiftRules.jsArtifacts.setHtml(id, Helpers.stripHead(xml)) & JsCmds.JsTry(js, false)
              case (Full(xml), _, false) => LiftRules.jsArtifacts.setHtml(id, Helpers.stripHead(xml))
              case (Full(xml), Full(js), true) => LiftRules.jsArtifacts.setHtml(id + "_outer", ( 
                spanFunc(0, Helpers.stripHead(xml)) ++ fixedXhtml.openOr(Text("")))) & JsCmds.JsTry(js, false)
              case (Full(xml), _, true) => LiftRules.jsArtifacts.setHtml(id + "_outer", ( 
                spanFunc(0, Helpers.stripHead(xml)) ++ fixedXhtml.openOr(Text(""))))
              case (_, Full(js), _) => js
              case _ => JsCmds.Noop
            }) & JsCmds.JsTry(JsCmds.Run("destroy_" + id + " = function() {" + (destroy.openOr(JsCmds.Noop).toJsCmd) + "};"), false)

    S.appendNotices(notices)
    ret = S.noticesToJsCmd & ret
    ret
  }

  def inSpan: NodeSeq = xml.openOr(Text("")) ++ javaScript.map(s => Script(s)).openOr(Text(""))

  def outSpan: NodeSeq = Script(Run("var destroy_" + id + " = function() {" + (destroy.openOr(JsCmds.Noop).toJsCmd) + "}")) ++
          fixedXhtml.openOr(Text(""))
}

/**
 * Update the comet XML on each page reload in dev mode
 */
case class UpdateDefaultXml(xml: NodeSeq) extends CometMessage

case class PartialUpdateMsg(cmd: () => JsCmd) extends CometMessage
case object AskRender extends CometMessage
case class AnswerRender(response: XmlOrJsCmd, who: LiftCometActor, when: Long, displayAll: Boolean) extends CometMessage
case object PerformSetupComet extends CometMessage
case object ShutdownIfPastLifespan extends CometMessage
case class AskQuestion(what: Any, who: LiftCometActor, listeners: List[(ListenerId, AnswerRender => Unit)]) extends CometMessage
case class AnswerQuestion(what: Any, listeners: List[(ListenerId, AnswerRender => Unit)]) extends CometMessage
case class Listen(when: Long, uniqueId: ListenerId, action: AnswerRender => Unit) extends CometMessage
case class Unlisten(uniqueId: ListenerId) extends CometMessage
case class ActionMessageSet(msg: List[() => Any], req: Req) extends CometMessage
case class ReRender(doAll: Boolean) extends CometMessage
case class ListenerId(id: Long)
case class Error(id: Box[String], msg: NodeSeq) extends CometMessage
case class Warning(id: Box[String], msg: NodeSeq) extends CometMessage
case class Notice(id: Box[String], msg: NodeSeq) extends CometMessage
case object ClearNotices extends CometMessage

object Error {
  def apply(node: NodeSeq): Error = Error(Empty, node)

  def apply(node: String): Error = Error(Empty, Text(node))

  def apply(id: String, node: String): Error = Error(Full(id), Text(node))

  def apply(id: String, node: NodeSeq): Error = Error(Full(id), node)
}
object Warning {
  def apply(node: NodeSeq): Warning = Warning(Empty, node)

  def apply(node: String): Warning = Warning(Empty, Text(node))

  def apply(id: String, node: String): Warning = Warning(Full(id), Text(node))

  def apply(id: String, node: NodeSeq): Warning = Warning(Full(id), node)
}
object Notice {
  def apply(node: NodeSeq): Notice = Notice(Empty, node)

  def apply(node: String): Notice = Notice(Empty, Text(node))

  def apply(id: String, node: String): Notice = Notice(Full(id), Text(node))

  def apply(id: String, node: NodeSeq): Notice = Notice(Full(id), node)
}

/**
 * The RenderOut case class contains the rendering for the CometActor.
 * Because of the implicit conversions, RenderOut can come from 
 * <br/>
 * @param xhtml is the "normal" render body
 * @param fixedXhtml is the "fixed" part of the body.  This is ignored unless reRender(true)
 * @param script is the script to be executed on render.  This is where you want to put your script
 * @param destroyScript is executed when the comet widget is redrawn ( e.g., if you register drag or mouse-over or some events, you unregister them here so the page doesn't leak resources.)
 * @param ignoreHtmlOnJs -- if the reason for sending the render is a Comet update, ignore the xhtml part and just run the JS commands.  This is useful in IE when you need to redraw the stuff inside <table><tr><td>... just doing innerHtml on <tr> is broken in IE
 */
@serializable
case class RenderOut(xhtml: Box[NodeSeq], fixedXhtml: Box[NodeSeq], script: Box[JsCmd], destroyScript: Box[JsCmd], ignoreHtmlOnJs: Boolean) {
  def this(xhtml: NodeSeq) = this (Full(xhtml), Empty, Empty, Empty, false)

  def this(xhtml: NodeSeq, js: JsCmd) = this (Full(xhtml), Empty, Full(js), Empty, false)

  def this(xhtml: NodeSeq, js: JsCmd, destroy: JsCmd) = this (Full(xhtml), Empty, Full(js), Full(destroy), false)

  def ++(cmd: JsCmd) =
    RenderOut(xhtml, fixedXhtml, script.map(_ & cmd) or Full(cmd),
      destroyScript, ignoreHtmlOnJs)
}

@serializable
private[http] object Never

}
}
