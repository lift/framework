/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
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
package transaction {

import net.liftweb.common.Loggable

import _root_.javax.naming.{NamingException, Context, InitialContext}
import _root_.javax.transaction.{
  Transaction,
  UserTransaction,
  TransactionManager,
  Status,
  Synchronization,
  RollbackException,
  SystemException,
  TransactionRequiredException
}
import _root_.javax.persistence.{
  EntityManager,
  EntityManagerFactory,
  NonUniqueResultException,
  NoResultException
}

import com.atomikos.icatch.jta.{
  J2eeTransactionManager,
  J2eeUserTransaction
}
import com.atomikos.icatch.config.{
  TSInitInfo,
  UserTransactionService,
  UserTransactionServiceImp
}

/**
 * EntityManager JTA synchronization class.
 * <p>
 * Registered in method: [joinTransaction].
 *
 * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
 */
class EntityManagerSynchronization(
  val em: EntityManager,
  val tm: TransactionManager,
  val closeAtTxCompletion: Boolean) 
  extends Synchronization with Loggable {

  def beforeCompletion = {
    try {
      val status = tm.getStatus
      if (status != Status.STATUS_ROLLEDBACK &&
          status != Status.STATUS_ROLLING_BACK &&
          status != Status.STATUS_MARKED_ROLLBACK) {
        logger.debug("Flushing EntityManager...") 
        em.flush // flush EntityManager on success
      }
    } catch {
      case e: SystemException => throw new RuntimeException(e)
    }
  }

  def afterCompletion(status: Int) = {
    val status = tm.getStatus
    if (closeAtTxCompletion) { 
      em.close
    }
    if (status == Status.STATUS_ROLLEDBACK ||
        status == Status.STATUS_ROLLING_BACK ||
        status == Status.STATUS_MARKED_ROLLBACK) {
        TransactionContext.closeEntityManager // destroy EntityManager on rollback
     }
  }
}

}
}
