package net.liftweb.http

import net.liftweb.common.Full

import scala.concurrent.duration.Duration
import scala.concurrent.{CanAwait, ExecutionContext, Future}
import scala.util.Try

/**
  * Decorates `Future` instance to allow access to session and request resources. Should be created with
  * `FutureWithSession.withCurrentSession` that takes the current Lift request and session and make it available
  * to all transformation methods and initial `Future` execution body. Each transformation method returns
  * `FutureWithSession`, thus, they can be all chained together.
  *
  * It's important to bear in mind that each chained method requires current thread's `LiftSession` to be available.
  * `FutureWithSession` does _not_ propagate initial session or request to all chained methods.
  *
  * @see FutureWithSession.withCurrentSession
  *
  * @param delegate original `Future` instance that will be enriched with session and request access
  */
private[http] class FutureWithSession[T](private[this] val delegate: Future[T]) extends Future[T] {

  import FutureWithSession.withCurrentSession

  override def isCompleted: Boolean = delegate.isCompleted

  override def value: Option[Try[T]] = delegate.value

  override def result(atMost: Duration)(implicit permit: CanAwait): T = delegate.result(atMost)

  override def ready(atMost: Duration)(implicit permit: CanAwait) = {
    delegate.ready(atMost)
    this
  }

  override def onComplete[U](f: (Try[T]) => U)(implicit executor:ExecutionContext): Unit = {
    val sessionFn = withCurrentSession(f)
    delegate.onComplete(sessionFn)
  }

  override def map[S](f: T => S)(implicit executor: ExecutionContext): FutureWithSession[S] = {
    val sessionFn = withCurrentSession(f)
    new FutureWithSession(delegate.map(sessionFn))
  }

  override def flatMap[S](f: T => Future[S])(implicit executor: ExecutionContext): FutureWithSession[S] = {
    val sessionFn = withCurrentSession(f)
    new FutureWithSession(delegate.flatMap(sessionFn))
  }

  override def andThen[U](pf: PartialFunction[Try[T], U])(implicit executor: ExecutionContext): FutureWithSession[T] = {
    val sessionFn = withCurrentSession(pf)
    new FutureWithSession(delegate.andThen {
      case t => sessionFn(t)
    })
  }

  override def failed: FutureWithSession[Throwable] = {
    new FutureWithSession(delegate.failed)
  }

  override def fallbackTo[U >: T](that: Future[U]): FutureWithSession[U] = {
    new FutureWithSession[U](delegate.fallbackTo(that))
  }

  override def recover[U >: T](pf: PartialFunction[Throwable, U])(implicit executor: ExecutionContext): FutureWithSession[U] = {
    val sessionFn = withCurrentSession(pf)
    new FutureWithSession(delegate.recover {
      case t => sessionFn(t)
    })
  }

  override def recoverWith[U >: T](pf: PartialFunction[Throwable, Future[U]])(implicit executor: ExecutionContext): FutureWithSession[U] = {
    val sessionFn = withCurrentSession(pf)
    new FutureWithSession(delegate.recoverWith {
      case t => sessionFn(t)
    })
  }

  override def transform[S](s: T => S, f: Throwable => Throwable)(implicit executor: ExecutionContext): Future[S] = {
    val sessionSuccessFn = withCurrentSession(s)
    val sessionFailureFn = withCurrentSession(f)

    new FutureWithSession(delegate.transform(s => sessionSuccessFn(s), f => sessionFailureFn(f)))
  }

  override def zip[U](that: Future[U]): Future[(T, U)] = {
    new FutureWithSession(delegate.zip(that))
  }
}

object FutureWithSession {

  /**
    * Creates `Future` instance aware of current request and session. Each `Future` returned by chained
    * transformation method (e.g. `map`, `flatMap`) will be also request/session-aware. However, it's
    * important to bear in mind that initial request and session are not propagated to chained methods.
    * It's required that current execution thread for chained method has its own request/session available
    * if reading/writing some data to it as a part of chained method execution.
    */
  def withCurrentSession[T](task: => T)(implicit executionContext: ExecutionContext): Future[T] = {
    FutureWithSession(task)
  }

  private def apply[T](task: => T)(implicit executionContext: ExecutionContext): FutureWithSession[T] = {
    S.session match {
      case Full(_) =>
        val sessionFn = withCurrentSession(() => task)
        new FutureWithSession(Future[T](sessionFn()))

      case _ =>
        new FutureWithSession(Future.failed[T](
          new IllegalStateException("LiftSession not available in this thread context")
        ))
    }
  }

  private def withCurrentSession[T](task: () => T): () => T = {
    val session = S.session openOrThrowException "LiftSession not available in this thread context"
    session.buildDeferredFunction(task)
  }

  private def withCurrentSession[A,T](task: (A)=>T): (A)=>T = {
    val session = S.session openOrThrowException "LiftSession not available in this thread context"
    session.buildDeferredFunction(task)
  }
}
