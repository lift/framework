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
package atomikos {

import _root_.javax.transaction.{TransactionManager, SystemException}

/**
 * Atomikos implementation of the transaction service trait.
 *
 * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
 */
object AtomikosTransactionService extends
  TransactionService with
  EntityManagerService with
  TransactionProtocol {

  import com.atomikos.icatch.jta.{J2eeTransactionManager, J2eeUserTransaction}
  import com.atomikos.icatch.config.{TSInitInfo, UserTransactionService, UserTransactionServiceImp}

  // FIXME: make configurable
  val JTA_TRANSACTION_TIMEOUT = 60
  private val txService: UserTransactionService = new UserTransactionServiceImp
  private val info: TSInitInfo = txService.createTSInitInfo

  val transactionManager =
    try {
      txService.init(info)
      val tm: TransactionManager = new J2eeTransactionManager
      tm.setTransactionTimeout(JTA_TRANSACTION_TIMEOUT)
      tm
    } catch {
      case e => throw new SystemException("Could not create a new Atomikos J2EE Transaction Manager, due to: " + e.toString)
    }

  // TODO: gracefully shutdown of the TM
  //txService.shutdown(false)
}

}
}
}
