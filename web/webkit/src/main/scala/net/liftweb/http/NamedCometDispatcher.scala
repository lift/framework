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

import common.{Box, Full, Loggable}
import actor.LiftActor

/**
 * This class keeps a list of comet actors that need to update the UI
 */
class NamedCometDispatcher(name: Box[String]) extends LiftActor with Loggable {

  logger.debug("DispatcherActor got name: %s".format(name))

  private var cometActorsToUpdate: Vector[CometActor]= Vector()

  override def messageHandler  = {
    /**
     * if we do not have this actor in the list, add it (register it)
     */
    case registerCometActor(actor, Full(name)) => {
      if(cometActorsToUpdate.contains(actor) == false){
        logger.debug("We are adding actor: %s to the list".format(actor))
        cometActorsToUpdate= cometActorsToUpdate :+ actor
      } else {
        logger.debug("The list so far is %s".format(cometActorsToUpdate))
      }
    }
    case unregisterCometActor(actor) => {
      logger.debug("before %s".format(cometActorsToUpdate))
      cometActorsToUpdate= cometActorsToUpdate.filterNot(_ == actor)
      logger.debug("after %s".format(cometActorsToUpdate))
    }

    //Catch the dummy message we send on comet creation
    case CometName(name) =>

    /**
     * Go through the list of actors and send them a message
     */
    case msg => {
      cometActorsToUpdate.par.foreach{ x => {
        x ! msg
        logger.debug("We will update this comet actor: %s showing name: %s".format(x, name))
      }
      }
    }
  }
}


/**
 * These are the message we pass around to
 * register each named comet actor with a dispatcher that
 * only updates the specific version it monitors
 */
case class registerCometActor(actor: CometActor, name: Box[String])
case class unregisterCometActor(actor: CometActor)
case class CometName(name: String)
