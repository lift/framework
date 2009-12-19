package net.liftweb.transaction.hibernate


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

