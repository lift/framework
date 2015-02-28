/*
 * Copyright 2007-2015 WorldWide Conferencing, LLC
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

 import scala.xml._

 import common._
 import js._

 import JsCmds.Noop

/**
 * A way to represent a delta between two component states so that the delta can
 * be applied to the client via a `[[JsCmd]]` returned from `[[toJs]]`.
 */
trait DeltaTrait {
  /**
   * Should provide a `[[JsCmd]]` that, when sent to the client, will apply this
   * delta to the contents on the client.
   */
  def toJs: JsCmd
}

/**
 * For `StatefulComet`, a way to capture the comet's state.
 *
 * `CometState` represents a particular state of a component. It knows two
 * things:
 *  - How to render this current state to a `[[NodeSeq scala.xml.NodeSeq]]`.
 *  - How to diff itself with another version of `CometState`.
 *
 * @tparam DeltaType The subclass of `[[DeltaTrait]]` that will represent deltas
 *         between your states.
 * @tparam MyType The type of your `CometState` subclass.
 */
trait CometState[DeltaType <: DeltaTrait,
                 MyType <: CometState[DeltaType, MyType]] {
  self: MyType =>

  /**
   * Override to provide a way to compute the difference between this
   * `CometState` and an earlier `CometState`, returned as a renderable delta of
   * the type.
   */
  def -(other: MyType): Seq[DeltaType]

  /**
   * Renders this state as a `NodeSeq`.
   */
  def render: NodeSeq
}

/**
 * Similar to `[[CometState]]`, but provides the ability to compute a new state
 * based on an object of `UpdateType`.
 *
 * @tparam UpdateType An object type that can be used to compute a new
 *         `CometState`.
 */
trait CometStateWithUpdate[UpdateType,
                           DeltaType <: DeltaTrait,
                           MyType <: CometStateWithUpdate[UpdateType, DeltaType, MyType]]
    extends CometState[DeltaType, MyType] { self: MyType =>
  /**
   * Given the input `UpdateType` object, compute a new comet state of this
   * type.
   *
   * If a `DeltaType` is used for `UpdateType`, it would make sense for
   * `cometState.process(update) - cometState == update` to hold true.
   */
  def process(in: UpdateType): MyType
}

/**
 * A `[[RenderingCometActor]]` that can track its state using a `[[CometState]]`
 * and push updates to that state using `[[DeltaTrait]]`s.
 *
 * Rendering this comet is the same as rendering the current state, but state
 * changes can be pushed to existing clients using the `DeltaTrait`.
 *
 * Implementors need to implement `[[testState]]`, which determines if an
 * arbitrary message received by the comet represents an update to the state,
 * and `[[emptyState]]`, which provides the base starting state for the comet.
 */
trait StatefulComet extends RenderingCometActor {
  /**
   * The subtype of `[[DeltaTrait]]` that represents changes between this
   * comet's states.
   */
  type Delta <: DeltaTrait
  /**
   * The subtype of `[[CometState]]` that represents the current state of this
   * comet at any given time.
   */
  type State <: CometState[Delta, State]

  /**
   * Test a message received by this comet to see if it represents a state
   * update, and returns the updated `State` if so.
   */
  def testState(in: Any): Box[State]

  /**
   * Return the empty/starting state for this comet.
   */
  def emptyState: State

  /**
   * The comet's current state.
   */
  protected var state: State = emptyState

  /**
   * If there's some `[[ThreadLocal]]` variable that needs to be set up before
   * processing the state deltas, set it up here.
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
   * Rendering for this comet; by default, simply renders the current
   * `[[state]]`.
   */
  def render = state.render
}

