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

import _root_.javax.persistence.{EntityManager, EntityManagerFactory}
import _root_.javax.transaction.{Transaction, Status, TransactionManager}
import _root_.net.liftweb.common.Loggable
import _root_.org.scala_libs.jpa.{ScalaEntityManager, ScalaEMFactory}
/**
 * Base monad for the transaction monad implementations.
 *
 * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
 */
trait TransactionMonad {

  // -----------------------------
  // Monadic definitions
  // -----------------------------

  def map[T](f: TransactionMonad => T): T
  def flatMap[T](f: TransactionMonad => T): T
  def foreach(f: TransactionMonad => Unit): Unit
  def filter(f: TransactionMonad => Boolean): TransactionMonad =
    if (f(this)) this else TransactionContext.NoOpTransactionMonad

  // -----------------------------
  // JTA Transaction definitions
  // -----------------------------

  /**
   * Returns the current Transaction.
   */
  def getTransaction: Transaction = TransactionContext.getTransactionManager.getTransaction

  /**
   * Marks the current transaction as doomed.
   */
  def setRollbackOnly = TransactionContext.setRollbackOnly

  /**
   * Marks the current transaction as doomed.
   */
  def doom = TransactionContext.setRollbackOnly

  /**
   * Checks if the current transaction is doomed.
   */
  def isRollbackOnly = TransactionContext.isRollbackOnly

  /**
   * Checks that the current transaction is NOT doomed.
   */
  def isNotDoomed = !TransactionContext.isRollbackOnly

  // -----------------------------
  // JPA EntityManager definitions
  // -----------------------------

  /**
   * Returns the current EntityManager.
   */
  def getEntityManager: EntityManager = TransactionContext.getEntityManager

  /**
   * Checks if an EntityManager exists in current context.
   */
  //def hasEntityManager: Boolean = TransactionContext.hasEntityManager

  /**
   * Closes and removes the current EntityManager.
   * <p/>
   * IMPORTANT: This method must always be used to close the EntityManager, never use em.close directly.
   */
  def closeEntityManager = TransactionContext.closeEntityManager
}

/**
 * Manages a thread-local stack of TransactionContexts.
 * <p/>
 * Choose TransactionService implementation by implicit definition of the implementation of choice,
 * e.g. <code>implicit val txService = TransactionServices.AtomikosTransactionService</code>.
 * <p/>
 * Example usage 1:
 * <pre>
 * for {
 *   ctx <- TransactionContext.Required
 *   entity <- updatedEntities
 *   if !ctx.isRollbackOnly
 * } {
 *   // transactional stuff
 *   ctx.getEntityManager.merge(entity)
 * }
 * </pre>
 * Example usage 2:
 * <pre>
 * val users = for {
 *   ctx <- TransactionContext.Required
 *   name <- userNames
 * } yield {
 *   // transactional stuff
 *   val query = ctx.getEntityManager.createNamedQuery("findUserByName")
 *   query.setParameter("userName", name)
 *   query.getSingleResult
 * }
 * </pre>
 *
 * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
 */
object TransactionContext extends TransactionProtocol with Loggable {
  // FIXME: make configurable
  private implicit val defaultTransactionService = atomikos.AtomikosTransactionService

  private[TransactionContext] val stack = new scala.util.DynamicVariable(new TransactionContext)

  object Required extends TransactionMonad {
    def map[T](f: TransactionMonad => T): T =        withTxRequired { f(this) }
    def flatMap[T](f: TransactionMonad => T): T =    withTxRequired { f(this) }
    def foreach(f: TransactionMonad => Unit): Unit = withTxRequired { f(this) }
  }

  object RequiresNew extends TransactionMonad {
    def map[T](f: TransactionMonad => T): T =        withTxRequiresNew { f(this) }
    def flatMap[T](f: TransactionMonad => T): T =    withTxRequiresNew { f(this) }
    def foreach(f: TransactionMonad => Unit): Unit = withTxRequiresNew { f(this) }
  }

  object Supports extends TransactionMonad {
    def map[T](f: TransactionMonad => T): T =        withTxSupports { f(this) }
    def flatMap[T](f: TransactionMonad => T): T =    withTxSupports { f(this) }
    def foreach(f: TransactionMonad => Unit): Unit = withTxSupports { f(this) }
  }

  object Mandatory extends TransactionMonad {
    def map[T](f: TransactionMonad => T): T =        withTxMandatory { f(this) }
    def flatMap[T](f: TransactionMonad => T): T =    withTxMandatory { f(this) }
    def foreach(f: TransactionMonad => Unit): Unit = withTxMandatory { f(this) }
  }

  object Never extends TransactionMonad {
    def map[T](f: TransactionMonad => T): T =        withTxNever { f(this) }
    def flatMap[T](f: TransactionMonad => T): T =    withTxNever { f(this) }
    def foreach(f: TransactionMonad => Unit): Unit = withTxNever { f(this) }
  }

  object NoOpTransactionMonad extends TransactionMonad {
    def map[T](f: TransactionMonad => T): T =        f(this)
    def flatMap[T](f: TransactionMonad => T): T =    f(this)
    def foreach(f: TransactionMonad => Unit): Unit = f(this)
    override def filter(f: TransactionMonad => Boolean): TransactionMonad = this
  }

  private[transaction] def setRollbackOnly = current.setRollbackOnly

  private[transaction] def isRollbackOnly = current.isRollbackOnly

  private[transaction] def getTransactionManager: TransactionManager = current.getTransactionManager

  private[transaction] def getTransaction: Transaction = current.getTransactionManager.getTransaction

  private[transaction] def getEntityManager: EntityManager = current.getEntityManager

  private[transaction] def closeEntityManager = current.closeEntityManager

  private[this] def current = stack.value

  /**
   * Continues with the invocation defined in 'body' with the brand new context define in 'newCtx', the old
   * one is put on the stack and will automatically come back in scope when the method exits.
   * <p/>
   * Suspends and resumes the current JTA transaction.
   */
  private[transaction] def withNewContext[T](body: => T): T = {
    val suspendedTx: Option[Transaction] =
      if (isInExistingTransaction(getTransactionManager)) {
        logger.debug("Suspending TX")
        Some(getTransactionManager.suspend)
      } else None
    val result = stack.withValue(new TransactionContext) { body }
    if (suspendedTx.isDefined) {
      logger.debug("Resuming TX")
      getTransactionManager.resume(suspendedTx.get)
    }
    result
  }
}

/**
 * Transaction context, holds the EntityManager and the TransactionManager.
 *
 * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
 */
class TransactionContext(private implicit val transactionService: TransactionService)
    extends ScalaEntityManager with ScalaEMFactory {

  val em: EntityManager = transactionService.entityManagerFactory.createEntityManager
  val tm: TransactionManager = transactionService.transactionManager

  private def setRollbackOnly = tm.setRollbackOnly

  protected def getUnitName = "N/A"

  private def isRollbackOnly: Boolean = tm.getStatus == Status.STATUS_MARKED_ROLLBACK

  private def getTransactionManager: TransactionManager = tm

  private def getEntityManager: EntityManager = em

  private def closeEntityManager = em.close

  // ---------------------------------
  // To make ScalaEMFactory happy
  val factory = this
  def openEM: javax.persistence.EntityManager = em
  def closeEM(e: javax.persistence.EntityManager) = closeEntityManager
}

}
}
