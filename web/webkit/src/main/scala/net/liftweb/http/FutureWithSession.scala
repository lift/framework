package net.liftweb.http

import net.liftweb.common._
import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.util.Try

object FutureWithSession {
  implicit class FutureDecorator[+T](future:Future[T]) {
    val withCurrentSession:Future[T] = S.session.map(new FutureWithSession[T](future)(_)).
      openOr(Future.failed(new Exception("LiftSession not available in this thread context")))
    def withImplicitSession(implicit session:LiftSession):Future[T] = new FutureWithSession[T](future)
  }
}

class FutureWithSession[+T](future:Future[T])(implicit session:LiftSession) extends Future[T] {
  import FutureWithSession._

  override def map[S](f: (T) ⇒ S)(implicit executor: ExecutionContext): Future[S] =
    future.map ( t => S.initIfUninitted(session) ( f(t) )).withImplicitSession
  override def flatMap[S](f: (T) ⇒ Future[S])(implicit executor: ExecutionContext): Future[S] =
    future.flatMap ( t => S.initIfUninitted(session) ( f(t) )).withImplicitSession
  override def andThen[U](pf: PartialFunction[Try[T], U])(implicit executor: ExecutionContext): Future[T] =
    future.andThen { case t => S.initIfUninitted(session) ( pf(t) ) }.withImplicitSession
  override def failed: Future[Throwable] = future.failed.withImplicitSession
//  override def fallbackTo[U >: T](that: Future[U]): Future[U] = future.fallbackTo(that)

  // Override all of the abstract Future[T] stuff to pass thru
  override def isCompleted = future.isCompleted
  override def value = future.value
  override def onComplete[U](f: (Try[T]) => U)(implicit executor:ExecutionContext) = future.onComplete(f)
  override def result(atMost: Duration)(implicit permit: CanAwait) = future.result(atMost)
  override def ready(atMost: Duration)(implicit permit: CanAwait) = {
    future.ready(atMost)
    this
  }
}
