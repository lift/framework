package net.liftweb.http

import net.liftweb.common._
import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.util.Try

object FutureWithSession {
  implicit class FutureDecorator[+T](future:Future[T]) {
    val withSession:Future[T] = S.session.map(new FutureWithSession[T](future, _)).
      openOr(Future.failed(new Exception("LiftSession not available in this thread context")))
  }
}

class FutureWithSession[+T](future:Future[T], session:LiftSession) extends Future[T] {
  import FutureWithSession._

  override def map[S](f: (T) ⇒ S)(implicit executor: ExecutionContext): Future[S] =
    future.map ( t => S.initIfUninitted(session) ( f(t) )).withSession
  override def flatMap[S](f: (T) ⇒ Future[S])(implicit executor: ExecutionContext): Future[S] =
    future.flatMap ( t => S.initIfUninitted(session) ( f(t) )).withSession

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
