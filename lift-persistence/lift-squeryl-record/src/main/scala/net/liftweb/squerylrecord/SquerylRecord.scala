/*
 * Copyright 2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package squerylrecord {

import _root_.net.liftweb.common.{Box, Full, Loggable}
import _root_.net.liftweb.mapper.DB // FIXME should be moved out of mapper
import _root_.net.liftweb.util.DynoVar
import _root_.org.squeryl.{Session, SessionFactory}
import _root_.org.squeryl.internals.{DatabaseAdapter, FieldMetaData}

/** Object containing initialization logic for the Squeryl/Record integration */
object SquerylRecord extends Loggable {
  /** Keep track of the current Squeryl Session we've created using DB */
  private object currentSession extends DynoVar[Session]
  
  /**
   * Initialize the Squeryl/Record integration. This must be called somewhere during your Boot, and before you use any
   * Records with Squeryl.
   */
  def init(mkAdapter: () => DatabaseAdapter) = {
    FieldMetaData.factory = new RecordMetaDataFactory
    SessionFactory.externalTransactionManagementAdapter = Some(() => currentSession.is openOr {
      DB.currentConnection match {
        case Full(superConn) =>
          val sess = Session.create(superConn.connection, mkAdapter())
          sess.setLogger(s => logger.debug(s))
          currentSession.set(sess)
          sess

        case _ => error("no current connection in scope. wrap your transaction with DB.use or use one of the DB loan wrappers")
      }
    })
  }
}

}
}
