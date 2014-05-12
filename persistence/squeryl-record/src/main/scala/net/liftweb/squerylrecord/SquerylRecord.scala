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

package net.liftweb
package squerylrecord

import common.{Box, Full, Loggable}
import db.DB
import util.DynoVar
import org.squeryl.{Session, SessionFactory}
import org.squeryl.internals.{DatabaseAdapter, FieldMetaData}
import net.liftweb.util.LoanWrapper
import RecordTypeMode._

/** Object containing initialization logic for the Squeryl/Record integration */
object SquerylRecord extends Loggable {

  /**
   * We have to remember the default Squeryl metadata factory before
   * we override it with our own implementation, so that we can use
   * the original factory for non-record classes.
   */
  private[squerylrecord] val posoMetaDataFactory = FieldMetaData.factory
  

  /**
   * Initialize the Squeryl/Record integration. This must be called somewhere during your Boot before you use any
   * Records with Squeryl.  When using this method, configure your Session separately
   * (see [[http://squeryl.org/sessions-and-tx.html]] for details) or you can use initWithSquerylSession to do both at once.
   */
  def init() {
    FieldMetaData.factory = new RecordMetaDataFactory
  }

  /**
   * Initialize the Squeryl/Record integration and configure a default Session at the same time.
   */
  def initWithSquerylSession(sessionFactory: => Session) {
    init()
    SessionFactory.concreteFactory = Some(() => sessionFactory)
  }
  
  def buildLoanWrapper() = new LoanWrapper {
    override def apply[T](f: => T): T = inTransaction {
      f
    }
  }
  
  /** 
   * 
   * NOTE: Remove this along with the deprecated method below
   * Keep track of the current Squeryl Session we've created using DB 
   * */
  private object currentSession extends DynoVar[Session]
}
