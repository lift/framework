package net.liftweb.http

import net.liftweb.actor.{LAFuture, LAScheduler}
import net.liftweb.common.{EmptyBox, Failure, Full}


object LAFutureWithSession {

  /**
    * Creates `LAFuture` instance aware of current request and session. Each `LAFuture` returned by chained
    * transformation method (e.g. `map`, `flatMap`) will be also request/session-aware. However, it's
    * important to bear in mind that initial session or request are not propagated to chained methods. It's required
    * that current execution thread for chained method has its own request or session available if reading/writing
    * some data to it as a part of chained method execution.
    */
  def withCurrentSession[T](task: => T, scheduler: LAScheduler = LAScheduler): LAFuture[T] = {
    S.session match {
      case Full(session) =>
        withSession(task, scheduler)

      case empty: EmptyBox =>
        withFailure(empty ?~! "LiftSession not available in this thread context", scheduler)
    }
  }

  private[this] def withSession[T](task: => T, scheduler: LAScheduler): LAFuture[T] = {
    val sessionContext = new LAFuture.Context {

      def around[S](fn: () => S): () => S = {
        val session = S.session openOrThrowException "LiftSession not available in this thread context"
        session.buildDeferredFunction(fn)
      }

      def around[A, S](fn: (A) => S): (A) => S = {
        val session = S.session openOrThrowException "LiftSession not available in this thread context"
        session.buildDeferredFunction(fn)
      }
    }

    LAFuture.build(task, scheduler, Full(sessionContext))
  }

  private[this] def withFailure[T](failure: Failure, scheduler: LAScheduler): LAFuture[T] = {
    val future = new LAFuture[T](scheduler)
    future.complete(failure)
    future
  }
}
