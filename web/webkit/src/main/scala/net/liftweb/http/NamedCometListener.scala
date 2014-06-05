/*
 * Copyright 2007-2012 WorldWide Conferencing, LLC
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

import util.Schedule
import util.Helpers._
import actor.{LAFuture, LiftActor}
import common.{Empty, Full, Box, Loggable}

/**
 * Maintain a Map[Value the actor monitors -> Ref to the Actor Dispatcher]
 *
 * For a url like: http://hostnbame/index/?p=icecream
 * If you name your actor based on the value of p
 * For each flavor that users have on their urls,
 * the map would be like:
 * chocolate -> code.comet.CometClassNames@ea5e9e7 ,
 * vanilla   -> code.comet.CometClassNames@wv9i7o3, etc
 *
 * If we have the actor already on the Map, just return it,
 * because it has to update the UI.
 * If wee do not have this actor on our Map. create a new
 * Dispatcher that will monitor this value, add it to our Map
 * and return the Ref to this new dispatcher so it updates the UI
 *
 *
 */
object NamedCometListener extends Loggable  {

  /**
   * The map of key -> Dispatchers
   */
  private var disptchers: Map[String, LiftActor] = Map()

  /**
   * Either returns  or creates a dispatcher for the @str key
   */
  def getOrAddDispatchersFor(str: Box[String]): LAFuture[LiftActor] = synchronized {
    val name= str.getOrElse("")
    val liftActor: LAFuture[LiftActor] = new LAFuture()
    Schedule{() => {
      val ret= disptchers.get(name ) match {
        case Some(actor) => actor
        case None        => {
          val ret = new NamedCometDispatcher(str)
          disptchers += name -> ret
          logger.debug("Our map of NamedCometDispatchers is: %s".format(disptchers));
          ret
        }
      }
      liftActor.satisfy(ret)
    }
    }
    liftActor
  }

  /**
   * Returns a Future containing a Full dispatcher or None for the @str key
   *
   * A common use case for this method is if you are sending updates to comet actors from a rest endpoint,
   * you do not want to create dispatchers if no comet is presently monitoring the @str key
   *
   */
  def getDispatchersFor(str: Box[String]): LAFuture[Box[LiftActor]] = synchronized {
    val name= str.getOrElse("")
    val liftActor: LAFuture[Box[LiftActor]] = new LAFuture()
    Schedule{() => {
      val ret= disptchers.get(name ) match {
        case Some(actor) => Full(actor)
        case None        => Empty
      }
      liftActor.satisfy(ret)
    }
    }
    liftActor
  }


}
