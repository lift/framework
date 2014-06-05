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

import util.Helpers._
import common.{Loggable, Full}


trait NamedCometActorTrait extends CometActor with Loggable {

  /**
   * First thing we do is registering this comet actor
   * for the "name" key
   */
  override  def localSetup = {
    NamedCometListener.getOrAddDispatchersFor(name).foreach(
      dispatcher=> dispatcher ! registerCometActor(this, name)
    )
    super.localSetup()
  }

  /**
   * We remove the CometActor from the map of registered actors
   */
  override  def localShutdown = {
    NamedCometListener.getOrAddDispatchersFor(name).foreach(
      dispatcher=> dispatcher !  unregisterCometActor(this)
    )
    super.localShutdown()
  }

  // time out the comet actor if it hasn't been on a page for 2 minutes
  override def lifespan = Full(120.seconds)

}
