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
package jpa {

import _root_.javax.persistence.EntityManager

import _root_.net.liftweb.http.TransientRequestVar

import _root_.org.scala_libs.jpa.{ScalaEMFactory, ScalaEntityManager}

/**
 * This trait provides specific functionality for the Lift web framework
 * by using a Lift <code>RequestVar</code> to hold the underlying EM. This
 * allows you to use a singleton for EM access. You must mix in some
 * other class to provide the actual ScalaEMFactory functionality.
 * Example usage would be:
 *
 * <p>
 * <code>
 * object Model extends LocalEMF("test") with RequestVarEM
 * </code>
 * </p>
 *
 * @author Derek Chen-Becker
 */
trait RequestVarEM extends ScalaEntityManager with ScalaEMFactory {
  /**
   * Provides the request var that holds the underlying <code>EntityManager</code>
   * for each request.
   */
  object emVar extends TransientRequestVar[EntityManager](openEM()) {
    this.registerGlobalCleanupFunc(ignore => closeEM(this.is))

    override def __nameSalt = net.liftweb.util.Helpers.randomString(10)
  }

  // Must be provided to properly implement ScalaEntityManager
  protected def em = emVar.is
  val factory = this

  /**
   * Returns the current underlying <code>EntityManager</code>. Generally
   * you shouldn't need to do this unless you're using some very
   * advanced or propietary functionality on the EM.
   *
   * @return The underlying EM
   */
  def getUnderlying : EntityManager = em
}

}
}
