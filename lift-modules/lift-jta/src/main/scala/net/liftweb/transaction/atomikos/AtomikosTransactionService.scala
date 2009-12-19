package net.liftweb.transaction.atomikos


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
