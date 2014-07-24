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
import net.liftweb.actor._
import net.liftweb.util.Helpers._
import net.liftweb.util._
import net.liftweb.json._
import scala.xml.{NodeSeq, Text, Elem, Node, Group, Null, PrefixedAttribute, UnprefixedAttribute}
import scala.collection.mutable.ListBuffer
import net.liftweb.http.js._
import JsCmds._
import JE._
import java.util.Locale

/**
* A case class that contains the information necessary to set up a CometActor
*/
final case class CometCreationInfo(cometType: String,
                                   cometName: Box[String],
                                   cometHtml: NodeSeq,
                                   cometAttributes: Map[String, String],
                                   session: LiftSession)

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
  extends CometState[DeltaType, MyType] {
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
   * If there's some ThreadLocal variable that needs to be set up
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
              partialUpdate(setupLocalState {
                diff.map(_.toJs).foldLeft(Noop)(_ & _)
              })
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

object CurrentCometActor extends ThreadGlobal[LiftCometActor]

object AddAListener {
  def apply(who: SimpleActor[Any]) = new AddAListener(who, {
    case _ => true
  })
}

/**
 * This is a message class for use with ListenerManager and CometListener
 * instances. The use of the shouldUpdate function is deprecated, and
 * should instead be handled by the message processing partial functions
 * on the CometListener instances themselves.
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
 * object Ticker extends ListenerManager with LiftActor {
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
 *       sendListenersMessage(Tick)
 *       ActorPing.schedule(this, Tick, 1000L)
 * }
 * }
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

  protected def listenerService: PartialFunction[Any, Unit] = {
    case AddAListener(who, wantsMessage) =>
      val pair = (who, wantsMessage)
      listeners ::= pair

      updateListeners(pair :: Nil)

    case RemoveAListener(who) =>
      listeners = listeners.filter(_._1 ne who)
      if (listeners.isEmpty) {
        onListenersListEmptied()
      }
  }

  /**
   * Called after RemoveAListener-message is processed and no more listeners exist.
   * Default does nothing.
   */
  protected def onListenersListEmptied() {
  }

  /**
   * Update the listeners with the message generated by createUpdate
   */
  protected def updateListeners(listeners: List[ActorTest] = listeners) {
    val update = createUpdate

    listeners foreach {
      case (who, wantsMessage) if wantsMessage.isDefinedAt(update) && wantsMessage(update) =>
        who ! update
    }
  }

  /**
   * Send a message we create to all of the listeners. Note that with this
   * invocation the createUpdate method is not used.
   */
  protected def sendListenersMessage(msg: Any) {
    listeners foreach (_._1 ! msg)
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
 * A LiftActorJ with ListenerManager.  Subclass this class
 * to get a Java-usable LiftActorJ with ListenerManager
 */
abstract class LiftActorJWithListenerManager extends LiftActorJ with ListenerManager {
  protected override def messageHandler: PartialFunction[Any, Unit] =
    highPriority orElse mediumPriority orElse
      listenerService orElse lowPriority orElse _messageHandler
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

  abstract override protected def localSetup() {
    registerWith ! AddAListener(this,  { case _ => true })
    super.localSetup()
  }

  abstract override protected def localShutdown() {
    registerWith ! RemoveAListener(this)
    super.localShutdown()
  }
}

trait LiftCometActor extends TypedActor[Any, Any] with ForwardableActor[Any, Any] with Dependent {
  def uniqueId: String

  /**
   * The last "when" sent from the listener
   * @return the last when sent from the listener
   */
  def lastListenerTime: Long

  private[http] def callInitCometActor(creationInfo: CometCreationInfo) {
    initCometActor(creationInfo)
  }

  /**
   * Override in sub-class to customise timeout for the render()-method for the specific comet
   */
  def cometRenderTimeout = LiftRules.cometRenderTimeout

  /**
   * Override in sub-class to customise timeout for AJAX-requests to the comet-component for the specific comet
   */
  def cometProcessingTimeout = LiftRules.cometProcessingTimeout

  /**
   * This is to react to comet-requests timing out.
   * When the timeout specified in {@link LiftRules#cometProcessingTimeout} occurs one may override
   * this to send a message to the user informing of the timeout.
   * <p/><p/>
   * Do NOT manipulate actor-state here. If you want to manipulate state, send the actor a new message.
   * <p/><p/>
   * Typical example would be:
   * <pre>
   *   override def cometTimeoutHandler(): JsCmd = {
   *     Alert("Timeout processing comet-request, timeout is: " + cometProcessingTimeout + "ms")
   * }
   * </pre>
   */
  def cometProcessingTimeoutHandler(): JsCmd = Noop

  /**
   * This is to react to comet-actors timing out while initial rendering, calls to render().
   * When the timeout specified in {@link LiftRules#cometRenderTimeout} occurs one may override
   * this to customise the output.
   * <p/><p/>
   * Do NOT manipulate actor-state here. If you want to manipulate state, send the actor a new message.
   * <p/><p/>
   * Typical example would be:
   * <pre>
   *   override def renderTimeoutHandler(): Box[NodeSeq] = {
   *     Full(&lt;div&gt;Comet {this.getClass} timed out, timeout is {cometRenderTimeout}ms&lt;/div&gt;)
   * }
   * </pre>
   */
  def cometRenderTimeoutHandler(): Box[NodeSeq] = Empty

  protected def initCometActor(creationInfo: CometCreationInfo): Unit

  def theType: Box[String]

  def name: Box[String]

  def hasOuter: Boolean

  def buildSpan(xml: NodeSeq): NodeSeq

  def parentTag: Elem

  /**
   * Poke the CometActor and cause it to do a partial update Noop which
   * will have the effect of causing the component to redisplay any
   * Wiring elements on the component.
   * This method is Actor-safe and may be called from any thread, not
   * just the Actor's message handler thread.
   */
  def poke(): Unit = {}

  /**
   * Is this CometActor going to capture the initial Req
   * object?  If yes, override this method and return true
   * and override captureInitialReq to capture the Req.  Why
   * have to explicitly ask for the Req? In order to send Req
   * instances across threads, the Req objects must be snapshotted
   * which is the process of reading the POST or PUT body from the
   * HTTP request stream.  We don't want to do this unless we
   * have to, so by default the Req is not snapshotted/sent.  But
   * if you want it, you can have it.
   */
  def sendInitialReq_? : Boolean = false

  /**
   * If the predicate cell changes, the Dependent will be notified
   */
  def predicateChanged(which: Cell[_]): Unit = {
    poke()
  }


  /**
   * The locale for the session that created the CometActor
   */
  def cometActorLocale: Locale = _myLocale

  private var _myLocale = Locale.getDefault()

  private[http] def setCometActorLocale(loc: Locale) {
    _myLocale = loc
  }
}

/**
 * Subclass from this class if you're in Java-land
 * and want a CometActor
 */
abstract class CometActorJ extends LiftActorJ with CometActor {

  override def lowPriority = _messageHandler

}

/**
 * Subclass from this class if you want a CometActorJ with
 * CometListeners
 */
abstract class CometActorJWithCometListener extends CometActorJ with CometListener {
  override def lowPriority = _messageHandler
}

/**
 * Takes care of the plumbing for building Comet-based Web Apps
 */
trait CometActor extends LiftActor with LiftCometActor with CssBindImplicits {
  private val logger = Logger(classOf[CometActor])
  val uniqueId = Helpers.nextFuncName
  private var spanId = uniqueId
  @volatile private var lastRenderTime = Helpers.nextNum

  /**
   * If we're going to cache the last rendering, here's the
   * private cache
   */
  private[this] var _realLastRendering: RenderOut = _

  /**
   * Get the current render clock for the CometActor
   * @return
   */
  def renderClock: Long = lastRenderTime

  @volatile
  private var _lastListenerTime: Long = 0

  /**
   * The last "when" sent from the listener
   * @return the last when sent from the listener
   */
  def lastListenerTime: Long = _lastListenerTime

  /**
   * The last rendering (cached or not)
   */
  private def lastRendering: RenderOut =
    if (dontCacheRendering) {
      val ret = render
      theSession.updateFunctionMap(S.functionMap, uniqueId, lastRenderTime)
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

  private var _deltaPruner: (CometActor, List[Delta]) => List[Delta] =
    (actor, d) => {
      val m = Helpers.millis
      d.filter(d => (m - d.timestamp) < 120000L)
    }

  private var _theSession: LiftSession = _

  def theSession = _theSession

  @volatile private var _defaultHtml: NodeSeq = _

  /**
   * The template that was passed to this component during comet
   * initializations
   */
  def defaultHtml: NodeSeq = _defaultHtml

  private var _name: Box[String] = Empty

  /**
   * The optional name of this CometActors
   */
  def name: Box[String] = _name

  private var _theType: Box[String] = Empty

  /**
   * The optional type of this CometActor
   */
  def theType: Box[String] = _theType

  private var _attributes: Map[String, String] = Map.empty

  def attributes = _attributes

  /**
   * The lifespan of this component.  By default CometActors
   * will last for the entire session that they were created in,
   * even if the CometActor is not currently visible.  You can
   * set the lifespan of the CometActor.  If the CometActor
   * isn't visible on any page for some period after its lifespan
   * the CometActor will be shut down.
   */
  def lifespan: Box[TimeSpan] = Empty

  private var _running = true

  private var _shutDownAt = millis

  /**
   * Is the CometActor running?
   */
  protected def running = _running

  /**
   * It's seriously suboptimal to override this method.  Instead
   * use localSetup()
   */
  protected def initCometActor(creationInfo: CometCreationInfo) {
    if (!dontCacheRendering) {
      lastRendering = RenderOut(Full(defaultHtml),
        Empty, Empty, Empty, false)
    }

    _theType = Full(creationInfo.cometType)
    _name = creationInfo.cometName
    _defaultHtml = creationInfo.cometHtml
    _attributes = creationInfo.cometAttributes
    _theSession = creationInfo.session
  }

  def defaultPrefix: Box[String] = Empty

  private lazy val _defaultPrefix: String = (defaultPrefix or _name) openOr "comet"

  /**
   * Set to 'true' if we should run "render" on every page load
   */
  protected def devMode = false

  def hasOuter = true

  def parentTag = <div style="display: inline"/>

  /**
   * Return the list of ListenerIds of all long poll agents that
   * are waiting for this CometActor to change its state.  This method
   * is useful for detecting presence.
   */
  protected def cometListeners: List[ListenerId] = listeners.map(_._1)

  /**
   * This method will be called when there's a change in the long poll
   * listeners.  The method does nothing, but allows you to get a granular
   * sense of how many browsers care about this CometActor.  Note that
   * this method should not block for any material time and if there's
   * any processing to do, use Scheduler.schedule or send a message to this
   * CometActor.  Do not change the Actor's state from this method.
   */
  protected def listenerTransition(): Unit = {}

  /**
   * Prepends the handler to the Json Handlers.  Should only be used
   * during instantiation
   *
   * @param h -- the PartialFunction that can handle a JSON request
   */
  def appendJsonHandler(h: PartialFunction[Any, JsCmd]) {
    jsonHandlerChain = h orElse jsonHandlerChain
  }


  /**
   * If there's actor-specific JSON behavior on failure to make the JSON
   * call, include the JavaScript here.
   */
  def onJsonError: Box[JsCmd] = Empty

  /**
   * Override this method to deal with JSON sent from the browser via the sendJson function.  This
   * is based on the Lift JSON package rather than the handleJson stuff based on the older util.JsonParser.  This
   * is the preferred mechanism.  If you use the jsonSend call, you will get a JObject(JField("command", cmd), JField("param", params))
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
   * Creates the span element acting as the real estate for comet rendering.
   */
  def buildSpan(xml: NodeSeq): Elem = {
    parentTag.copy(child = xml) % ("id" -> spanId)
  }

  /**
   * How to report an error that occurs during message dispatch
   */
  protected def reportError(msg: String, exception: Exception) {
    logger.error(msg, exception)
  }

  protected override def messageHandler = {
    val what = composeFunction
    val myPf: PartialFunction[Any, Unit] = new PartialFunction[Any, Unit] {
      def apply(in: Any): Unit =
        CurrentCometActor.doWith(CometActor.this) {
          S.initIfUninitted(theSession) {
            RenderVersion.doWith(uniqueId) {
              S.functionLifespan(true) {
                try {
                  what.apply(in)
                } catch {
                  case e if exceptionHandler.isDefinedAt(e) => exceptionHandler(e)
                  case e: Exception => reportError("Message dispatch for " + in, e)
                }
                if (S.functionMap.size > 0) {
                  theSession.updateFunctionMap(S.functionMap,
                    uniqueId, lastRenderTime)
                  S.clearFunctionMap
                }
              }
            }
          }
        }

      def isDefinedAt(in: Any): Boolean =
        CurrentCometActor.doWith(CometActor.this) {
          S.initIfUninitted(theSession) {
            RenderVersion.doWith(uniqueId) {
              S.functionLifespan(true) {
                try {
                  what.isDefinedAt(in)
                } catch {
                  case e if exceptionHandler.isDefinedAt(e) => exceptionHandler(e); false
                  case e: Exception => reportError("Message test for " + in, e); false
                }
              }
            }
          }
        }
    }

    myPf
  }

  /**
   * A part of the CometActor's screen real estate that is not
   * updated by default with reRender().  This block of HTML is
   * useful for the editor part of a Comet-based control where
   * the data is JSON and updated with partialUpdates.
   */
  def fixedRender: Box[NodeSeq] = Empty

  /**
   * Calculate fixedRender and capture the postpage javascript
   */
  protected def calcFixedRender: Box[NodeSeq] =
    fixedRender.map(ns => theSession.postPageJavaScript() match {
      case Nil => ns
      case xs => {
        ns ++ Script(xs)
      }
    })

  /**
   * We have to cache fixedRender and only change it if
   * the template changes or we get a reRender(true)
   */
  private def internalFixedRender: Box[NodeSeq] =
    if (!cacheFixedRender) {
      calcFixedRender
    } else {
      cachedFixedRender.get
    }

  private val cachedFixedRender: FatLazy[Box[NodeSeq]] = FatLazy(calcFixedRender)

  /**
   * By default, we do not cache the value of fixedRender.  If it's
   * expensive to recompute it each time there's a conversion
   * of something to a RenderOut, override this method if you
   * want to cache fixedRender.
   */
  protected def cacheFixedRender = false

  /**
   * A helpful implicit conversion that takes a NodeSeq => NodeSeq
   * (for example a CssSel) and converts it to a Box[NodeSeq] by
   * applying the function to defaultHtml
   */
  protected implicit def nodeSeqFuncToBoxNodeSeq(f: NodeSeq => NodeSeq):
  Box[NodeSeq] = Full(f(defaultHtml))

  /**
   * By default, `CometActor` handles `RedirectShortcutException`, which is
   * used to handle many types of redirects in Lift. If you override this
   * `PartialFunction` to do your own exception handling and want redirects
   * from e.g. `S.redirectTo` to continue working correctly, make sure you
   * chain back to this implementation.
   */
  override def exceptionHandler : PartialFunction[Throwable, Unit] = {
    case  ResponseShortcutException(_, Full(redirectUri), _) =>
      partialUpdate(RedirectTo(redirectUri))

    case other if super.exceptionHandler.isDefinedAt(other) =>
      super.exceptionHandler(other)
  }

  /**
   * Handle messages sent to this Actor before the
   */
  def highPriority: PartialFunction[Any, Unit] = Map.empty

  def lowPriority: PartialFunction[Any, Unit] = Map.empty

  def mediumPriority: PartialFunction[Any, Unit] = Map.empty

  private[http] def _lowPriority: PartialFunction[Any, Unit] = {
    case s => logger.debug("CometActor " + this + " got unexpected message " + s)
  }

  private lazy val _mediumPriority: PartialFunction[Any, Unit] = {
    case l@Unlisten(seq) => {
      lastListenTime = millis
      askingWho match {
        case Full(who) => forwardMessageTo(l, who) // forward l
        case _ => listeners = listeners.filter(_._1 != seq)
      }
      listenerTransition()
    }


    case l@Listen(when, seqId, toDo) => {
      lastListenTime = millis
      askingWho match {
        case Full(who) => forwardMessageTo(l, who) // who forward l
        case _ =>
          if (when < lastRenderTime) {
            toDo(AnswerRender(new XmlOrJsCmd(spanId, lastRendering,
              buildSpan _, notices.toList),
              whosAsking openOr this, lastRenderTime, wasLastFullRender))
            clearNotices
          } else {
            lastRenderTime = when
            deltas.filter(_.when > when) match {
              case Nil => listeners = (seqId, toDo) :: listeners

              case all@(hd :: xs) =>
                toDo(AnswerRender(new XmlOrJsCmd(spanId, Empty, Empty,
                  Full(all.reverse.foldLeft(Noop)(_ & _.js)), Empty, buildSpan, false, notices.toList),
                  whosAsking openOr this, hd.when, false))
                clearNotices
                deltas = _deltaPruner(this, deltas)
            }
          }
      }
      listenerTransition()
    }


    case PerformSetupComet2(initialReq) => {
      localSetup()
      captureInitialReq(initialReq)
      performReRender(true)
    }

    /**
     * Update the defaultHtml... sent in dev mode
     */
    case UpdateDefaultHtml(html) => {
      val redo = html != _defaultHtml

      _defaultHtml = html

      if (redo) {
        performReRender(false)
      }
    }

    case AskRender =>
      askingWho match {
        case Full(who) => forwardMessageTo(AskRender, who) //  forward AskRender
        case _ => {
          if (!deltas.isEmpty || devMode)
            try {
              performReRender(false)
            } catch {
              case e if exceptionHandler.isDefinedAt(e) => exceptionHandler(e)
              case e: Exception => reportError("Failed performReRender", e)
            }

          reply(AnswerRender(new XmlOrJsCmd(spanId, lastRendering,
            buildSpan _, notices.toList),
            whosAsking openOr this, lastRenderTime, true))
          clearNotices
        }
      }


    case ActionMessageSet(msgs, req) =>
      S.doCometParams(req.params) {
        S.jsToAppend() match {
          case Nil =>
          case js => partialUpdate(js)
        }

        val computed: List[Any] =
          msgs.flatMap {
            f => try {
              List(f())
            } catch {
              case e if exceptionHandler.isDefinedAt(e) => exceptionHandler(e); Nil
              case e: Exception => reportError("Ajax function dispatch", e); Nil
            }
          }

        reply(computed ::: List(S.noticesToJsCmd))
      }

    case AskQuestion(what, who, otherlisteners) => {
      this.spanId = who.uniqueId
      this.listeners = otherlisteners ::: this.listeners
      startQuestion(what)
      whosAsking = Full(who)
      this.reRender(true)
      listenerTransition()
    }

    case AnswerQuestion(what, otherListeners) =>
      askingWho.foreach {
        ah => {
          reply("A null message to release the actor from its send and await reply... do not delete this message")
          // askingWho.unlink(self)
          ah ! ShutDown
          this.listeners = this.listeners ::: otherListeners
          this.askingWho = Empty
          val aw = answerWith
          answerWith = Empty
          aw.foreach(_(what))
          performReRender(true)
          listenerTransition()
        }
      }

    case ShutdownIfPastLifespan =>
      for {
        ls <- lifespan if listeners.isEmpty && (lastListenTime + ls.millis + 1000l) < millis
      } {
        this ! ShutDown
      }

    case ReRender(all) => performReRender(all)

    case SetDeltaPruner(f) =>
      _deltaPruner = f
      deltas = f(this, deltas)

    case Error(id, node) => notices += ((NoticeType.Error, node, id))

    case Warning(id, node) => notices += ((NoticeType.Warning, node, id))

    case Notice(id, node) => notices += ((NoticeType.Notice, node, id))

    case ClearNotices => clearNotices

    case ShutDown =>
      logger.info("The CometActor " + this + " Received Shutdown")
      askingWho.foreach(_ ! ShutDown)
      theSession.removeCometActor(this)
      _localShutdown()

    case PartialUpdateMsg(cmdF) => {
      val cmd: JsCmd = cmdF.apply
      val time = Helpers.nextNum
      val delta = JsDelta(time, cmd)
      theSession.updateFunctionMap(S.functionMap, uniqueId, time)
      S.clearFunctionMap
      deltas = _deltaPruner(this,  (delta :: deltas))
      if (!listeners.isEmpty) {
        val postPage = theSession.postPageJavaScript()
        val rendered =
          AnswerRender(new XmlOrJsCmd(spanId, Empty, Empty,
            Full(cmd & postPage),
            Empty, buildSpan, false,
            notices.toList),
            whosAsking openOr this, time, false)
        clearNotices
        listeners.foreach(_._2(rendered))
        listeners = Nil
        listenerTransition()
      }
    }
  }


  /**
   * It's the main method to override, to define what is rendered by the CometActor
   *
   * There are implicit conversions for a bunch of stuff to
   * RenderOut (including NodeSeq).  Thus, if you don't declare the return
   * turn to be something other than RenderOut and return something that's
   * coercible into RenderOut, the compiler "does the right thing"(tm) for you.
   * <br/>
   * There are implicit conversions for NodeSeq, so you can return a pile of
   * XML right here.  There's an implicit conversion for NodeSeq => NodeSeq,
   * so you can return a function (e.g., a CssBindFunc) that will convert
   * the defaultHtml to the correct output.  There's an implicit conversion
   * from JsCmd, so you can return a pile of JavaScript that'll be shipped
   * to the browser.<br/>
   * Note that the render method will be called each time a new browser tab
   * is opened to the comet component or the comet component is otherwise
   * accessed during a full page load (this is true if a partialUpdate
   * has occurred.)  You may want to look at the fixedRender method which is
   * only called once and sets up a stable rendering state.
   */
  def render: RenderOut

  /**
   * Cause the entire component to be reRendered and pushed out
   * to any listeners.  This method will cause the entire component
   * to be rendered which can result in a huge blob of JavaScript to
   * be sent to the client.  It's a much better practice to use
   * partialUpdate for non-trivial CometActor components.
   *
   * @param sendAll -- Should the fixed part of the CometActor be
   * rendered.
   */
  def reRender(sendAll: Boolean) {
    this ! ReRender(sendAll)
  }

  /**
   * Cause the entire component to be reRendered and pushed out
   * to any listeners.  This method will cause the entire component
   * to be rendered which can result in a huge blob of JavaScript to
   * be sent to the client.  It's a much better practice to use
   * partialUpdate for non-trivial CometActor components.
   */
  def reRender() {
    reRender(false)
  }


  /**
   * Set this method to true if you want to avoid caching the
   * rendering.  This trades space for time.
   */
  protected def dontCacheRendering: Boolean = false

  /**
   * Clear the common dependencies for Wiring.  This
   * method will clearPostPageJavaScriptForThisPage() and
   * unregisterFromAllDependencies().  The combination
   * will result in a clean slate for Wiring during a redraw.
   * You can change the behavior of the wiring dependency management
   * by overriding this method
   */
  protected def clearWiringDependencies() {
    if (!manualWiringDependencyManagement) {
      theSession.clearPostPageJavaScriptForThisPage()
      unregisterFromAllDependencies()
    }
  }

  /**
   * By default, Lift deals with managing wiring dependencies.
   * This means on each full render (a full render will
   * happen on reRender() or on a page load if there have been
   * partial updates.) You may want to manually deal with
   * wiring dependencies.  If you do, override this method
   * and return true
   */
  protected def manualWiringDependencyManagement = false

  private def performReRender(sendAll: Boolean) {
    lastRenderTime = Helpers.nextNum

    if (sendAll) {
      cachedFixedRender.reset
    }

    if (sendAll || !cacheFixedRender) {
      clearWiringDependencies()
    }

    wasLastFullRender = sendAll & hasOuter
    deltas = Nil

    if (!dontCacheRendering) {
      lastRendering = render
    }

    theSession.updateFunctionMap(S.functionMap, uniqueId, lastRenderTime)

    val rendered: AnswerRender =
      AnswerRender(new XmlOrJsCmd(spanId, lastRendering, buildSpan _, notices.toList),
        this, lastRenderTime, sendAll)

    clearNotices
    listeners.foreach(_._2(rendered))
    listeners = Nil
  }

  def unWatch = partialUpdate(Call("lift.unlistWatch", uniqueId))

  /**
   * Poke the CometActor and cause it to do a partial update Noop which
   * will have the effect of causing the component to redisplay any
   * Wiring elements on the component.
   * This method is Actor-safe and may be called from any thread, not
   * just the Actor's message handler thread.
   */
  override def poke(): Unit = {
    if (running) {
      partialUpdate(Noop)
    }
  }

  /**
   * Perform a partial update of the comet component based
   * on the JsCmd.  This means that the JsCmd will be sent to
   * all of the currently listening browser tabs.  This is the
   * preferred method over reRender to update the component
   */
  protected def partialUpdate(cmd: => JsCmd) {
    this ! PartialUpdateMsg(() => cmd)
  }

  protected def startQuestion(what: Any) {
  }

  /**
   * This method will be called after the Actor has started.  Do any setup here.
   * DO NOT do initialization in the constructor or in initCometActor... do it here.
   */
  protected def localSetup(): Unit = {
  }

  /**
   * Comet Actors live outside the HTTP request/response cycle.
   * However, it may be useful to know what Request led to the
   * creation of the CometActor.  You can override this method
   * and capture the initial Req object.  Note that keeping a reference
   * to the Req may lead to memory retention issues if the Req contains
   * large message bodies, etc.  It's optimal to capture the path
   * or capture any request parameters that you care about rather
   * the keeping the whole Req reference.
   */
  protected def captureInitialReq(initialReq: Box[Req]) {
  }

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
  protected def localShutdown(): Unit = {
  }

  /**
   * Compose the Message Handler function. By default,
   * composes highPriority orElse mediumPriority orElse internalHandler orElse
   * lowPriority orElse internalHandler.  But you can change how
   * the handler works if doing stuff in highPriority, mediumPriority and
   * lowPriority is not enough.
   */
  protected def composeFunction: PartialFunction[Any, Unit] = composeFunction_i

  private def composeFunction_i: PartialFunction[Any, Unit] = {
    // if we're no longer running don't pass messages to the other handlers
    // just pass them to our handlers
    if (!_running && (millis - 20000L) > _shutDownAt)
      _mediumPriority orElse _lowPriority
    else
      highPriority orElse mediumPriority orElse
        _mediumPriority orElse lowPriority orElse _lowPriority
  }

  /**
   * Ask another CometActor a question.  That other CometActor will
   * take over the screen real estate until the question is answered.
   */
  protected def ask(who: LiftCometActor, what: Any)(answerWith: Any => Unit) {
    who.callInitCometActor(CometCreationInfo(who.uniqueId, name, defaultHtml, attributes, theSession))
    theSession.addCometActor(who)

    who ! PerformSetupComet2(Empty)
    askingWho = Full(who)
    this.answerWith = Full(answerWith)
    who ! AskQuestion(what, this, listeners)
  }

  protected def answer(answer: Any) {
    whosAsking.foreach(_ !? AnswerQuestion(answer, listeners))
    whosAsking = Empty
    performReRender(false)
  }

  /**
   * Convert a NodeSeq => NodeSeq to a RenderOut.  The render method
   * returns a RenderOut.  This method implicitly (in Scala) or explicitly
   * (in Java) will convert a NodeSeq => NodeSeq to a RenderOut.  This
   * is helpful if you use Lift's CSS Selector Transforms to define
   * rendering.
   */
  protected implicit def nsToNsFuncToRenderOut(f: NodeSeq => NodeSeq) =
    new RenderOut((Box !! defaultHtml).map(f), internalFixedRender, if (autoIncludeJsonCode) Full(jsonToIncludeInCode & S.jsToAppend())
    else {
      S.jsToAppend match {
        case Nil => Empty
        case x :: Nil => Full(x)
        case xs => Full(xs.reduceLeft(_ & _))
      }
    }, Empty, false)

  /**
   * Convert a Seq[Node] (the superclass of NodeSeq) to a RenderOut.
   * The render method
   * returns a RenderOut.  This method implicitly (in Scala) or explicitly
   * (in Java) will convert a NodeSeq to a RenderOut.  This
   * is helpful if you return a NodeSeq from your render method.
   */
  protected implicit def arrayToRenderOut(in: Seq[Node]): RenderOut = new RenderOut(Full(in: NodeSeq), internalFixedRender, if (autoIncludeJsonCode) Full(jsonToIncludeInCode & S.jsToAppend())
  else {
    S.jsToAppend match {
      case Nil => Empty
      case x :: Nil => Full(x)
      case xs => Full(xs.reduceLeft(_ & _))
    }
  }, Empty, false)

  protected implicit def jsToXmlOrJsCmd(in: JsCmd): RenderOut = new RenderOut(Empty, internalFixedRender, if (autoIncludeJsonCode) Full(in & jsonToIncludeInCode & S.jsToAppend()) else Full(in & S.jsToAppend()), Empty, false)

  implicit def pairToPair(in: (String, Any)): (String, NodeSeq) = (in._1, Text(in._2 match {
    case null => "null"
    case s => s.toString
  }))

  implicit def nodeSeqToFull(in: NodeSeq): Box[NodeSeq] = Full(in)

  implicit def elemToFull(in: Elem): Box[NodeSeq] = Full(in)

  /**
   * Similar with S.error
   */
  def error(n: String) {
    error(Text(n))
  }

  /**
   * Similar with S.error
   */
  def error(n: NodeSeq) {
    notices += ((NoticeType.Error, n, Empty))
  }

  /**
   * Similar with S.error
   */
  def error(id: String, n: NodeSeq) {
    notices += ((NoticeType.Error, n, Full(id)))
  }

  /**
   * Similar with S.error
   */
  def error(id: String, n: String) {
    error(id, Text(n))
  }

  /**
   * Similar with S.notice
   */
  def notice(n: String) {
    notice(Text(n))
  }

  /**
   * Similar with S.notice
   */
  def notice(n: NodeSeq) {
    notices += ((NoticeType.Notice, n, Empty))
  }

  /**
   * Similar with S.notice
   */
  def notice(id: String, n: NodeSeq) {
    notices += ((NoticeType.Notice, n, Full(id)))
  }

  /**
   * Similar with S.notice
   */
  def notice(id: String, n: String) {
    notice(id, Text(n))
  }

  /**
   * Similar with S.warning
   */
  def warning(n: String) {
    warning(Text(n))
  }

  /**
   * Similar with S.warning
   */
  def warning(n: NodeSeq) {
    notices += ((NoticeType.Warning, n, Empty))
  }

  /**
   * Similar with S.warning
   */
  def warning(id: String, n: NodeSeq) {
    notices += ((NoticeType.Warning, n, Full(id)))
  }

  /**
   * Similar with S.warning
   */
  def warning(id: String, n: String) {
    warning(id, Text(n))
  }

  private def clearNotices {
    notices.clear
  }

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
                               spanFunc: (NodeSeq) => NodeSeq,
                               ignoreHtmlOnJs: Boolean,
                               notices: List[(NoticeType.Value, NodeSeq, Box[String])]) {
  def this(id: String, ro: RenderOut, spanFunc: (NodeSeq) => NodeSeq, notices: List[(NoticeType.Value, NodeSeq, Box[String])]) =
    this (id, ro.xhtml, ro.fixedXhtml, ro.script, ro.destroyScript, spanFunc, ro.ignoreHtmlOnJs, notices)

  val xml = _xml.flatMap(content => S.session.map(s => s.processSurroundAndInclude("JS SetHTML id: " + id, content)))
  val fixedXhtml = _fixedXhtml.flatMap(content => S.session.map(s => s.processSurroundAndInclude("JS SetHTML id: " + id, content)))

  /**
   * Returns the JsCmd that will be sent to client
   */
  def toJavaScript(session: LiftSession, displayAll: Boolean): JsCmd = {
    val updateJs =
      (if (ignoreHtmlOnJs) Empty else xml, javaScript, displayAll) match {
        case (Full(xml), Full(js), false) => LiftRules.jsArtifacts.setHtml(id, Helpers.stripHead(xml)) & JsCmds.JsTry(js, false)
        case (Full(xml), _, false) => LiftRules.jsArtifacts.setHtml(id, Helpers.stripHead(xml))
        case (Full(xml), Full(js), true) => LiftRules.jsArtifacts.setHtml(id + "_outer", (
          spanFunc(Helpers.stripHead(xml)) ++ fixedXhtml.openOr(Text("")))) & JsCmds.JsTry(js, false)
        case (Full(xml), _, true) => LiftRules.jsArtifacts.setHtml(id + "_outer", (
          spanFunc(Helpers.stripHead(xml)) ++ fixedXhtml.openOr(Text(""))))
        case (_, Full(js), _) => js
        case _ => JsCmds.Noop
      }
    val fullUpdateJs =
      LiftRules.cometUpdateExceptionHandler.vend.foldLeft(updateJs) { (commands, catchHandler) =>
        JsCmds.Run(
          "try{" +
            commands.toJsCmd +
          "}catch(e){" +
            catchHandler.toJsCmd +
          "}"
        )
      }

    var ret: JsCmd = JsCmds.JsTry(JsCmds.Run("destroy_" + id + "();"), false) &
       fullUpdateJs &
       JsCmds.JsTry(JsCmds.Run("destroy_" + id + " = function() {" + (destroy.openOr(JsCmds.Noop).toJsCmd) + "};"), false)

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
case class UpdateDefaultHtml(html: NodeSeq) extends CometMessage

case class PartialUpdateMsg(cmd: () => JsCmd) extends CometMessage

case object AskRender extends CometMessage

case class AnswerRender(response: XmlOrJsCmd, who: LiftCometActor, when: Long, displayAll: Boolean) extends CometMessage

case class PerformSetupComet2(initialReq: Box[Req]) extends CometMessage

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

case class SetDeltaPruner(f: (LiftCometActor, List[Delta]) => List[Delta]) extends CometMessage

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
case class RenderOut(xhtml: Box[NodeSeq], fixedXhtml: Box[NodeSeq], script: Box[JsCmd], destroyScript: Box[JsCmd], ignoreHtmlOnJs: Boolean) {
  def this(xhtml: NodeSeq) = this (Full(xhtml), Empty, Empty, Empty, false)

  def this(xhtml: NodeSeq, js: JsCmd) = this (Full(xhtml), Empty, Full(js), Empty, false)

  def this(xhtml: NodeSeq, js: JsCmd, destroy: JsCmd) = this (Full(xhtml), Empty, Full(js), Full(destroy), false)

  def ++(cmd: JsCmd) =
    RenderOut(xhtml, fixedXhtml, script.map(_ & cmd) or Full(cmd),
      destroyScript, ignoreHtmlOnJs)
}

private[http] object Never extends Serializable

