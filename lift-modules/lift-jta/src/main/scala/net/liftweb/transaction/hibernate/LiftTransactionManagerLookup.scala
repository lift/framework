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
package hibernate {

import javax.transaction.{TransactionManager, Transaction}

/**
 * Hibernate TransactionManager lookup class.
 *
 * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
 */
class LiftTransactionManagerLookup extends org.hibernate.transaction.TransactionManagerLookup {
  def getTransactionManager(props: _root_.java.util.Properties): TransactionManager = TransactionContext.getTransactionManager
  def getUserTransactionName: String = "java:comp/UserTransaction"
  def getTransactionIdentifier(tx: Transaction) = tx
}

}
}
}
