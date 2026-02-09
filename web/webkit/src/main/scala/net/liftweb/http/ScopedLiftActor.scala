/*
 * Copyright 2007-2013 Lift Committers and Contributors
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

import common._
import actor._
import util._
  import Helpers._

/**
 * A LiftActor that runs in the scope of the current Session, repleat with SessionVars, etc.
 * In general, you'll want to use a ScopedLiftActor when you do stuff with clientActorFor, etc.
 * so that you have the session scope
 *
 */
trait ScopedLiftActor extends LiftActor with LazyLoggable {
  /**
   * The session captured when the instance is created. It should be correct if the instance is created
   * in the scope of a request
   */
  protected val _session: LiftSession = S.session openOr new LiftSession("", Helpers.nextFuncName, Empty)

  /**
   * The render version of the page that this was created in the scope of
   */
  protected val _uniqueId: String = RenderVersion.get

  /**
   * The session associated with this actor. By default it's captured at the time of instantiation, but
   * that doesn't always work, so you might have to override this method
   * @return
   */
  def session: LiftSession = _session


  /**
   * The unique page ID of the page that this Actor was created in the scope of
   * @return
   */
  def uniqueId: String = _uniqueId

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
    highPriority orElse mediumPriority orElse
      lowPriority
  }

  /**
   * Handle messages sent to this Actor before the
   */
  def highPriority: PartialFunction[Any, Unit] = Map.empty

  def lowPriority: PartialFunction[Any, Unit] = Map.empty

  def mediumPriority: PartialFunction[Any, Unit] = Map.empty

  protected override def messageHandler = {
    val what = composeFunction
    val myPf: PartialFunction[Any, Unit] = new PartialFunction[Any, Unit] {
      def apply(in: Any): Unit =
        S.initIfUninitted(session) {
          RenderVersion.doWith(uniqueId) {
            S.functionLifespan(true) {
              try {
                what.apply(in)
              } catch {
                case e if exceptionHandler.isDefinedAt(e) => exceptionHandler(e)
                case e: Exception => reportError("Message dispatch for " + in, e)
              }
              if (S.functionMap.size > 0) {
                session.updateFunctionMap(S.functionMap, uniqueId, millis)
                S.clearFunctionMap
              }
            }
          }
        }

      def isDefinedAt(in: Any): Boolean =
        S.initIfUninitted(session) {
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

    myPf
  }

  /**
   * How to report an error that occurs during message dispatch
   */
  protected def reportError(msg: String, exception: Exception): Unit = {
    logger.error(msg, exception)
  }
}
