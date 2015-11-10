package net.liftweb.http

import scala.concurrent.{ExecutionContext, CanAwait, Future} 
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

  // Override all of the abstract Future[T] stuff to pass thru
  override def isCompleted:Boolean = future.isCompleted
  override def value:Option[Try[T]] = future.value
  override def result(atMost: Duration)(implicit permit: CanAwait):T = future.result(atMost)
  override def ready(atMost: Duration)(implicit permit: CanAwait) = {
    future.ready(atMost)
    this
  }
  override def onComplete[U](f: (Try[T]) => U)(implicit executor:ExecutionContext):Unit =
    future.onComplete( t => S.initIfUninitted(session) ( f(t) ) )

  // Override all methods with functions that need to run in session scope and/or return other futures
  override def andThen[U](pf: PartialFunction[Try[T], U])(implicit executor: ExecutionContext): Future[T] =
    future.andThen { case t => S.initIfUninitted(session) ( pf(t) ) }.withImplicitSession
  override def failed: Future[Throwable] = future.failed.withImplicitSession
  override def fallbackTo[U >: T](that: Future[U]): Future[U] = future.fallbackTo(that).withImplicitSession
  override def flatMap[S](f: (T) ⇒ Future[S])(implicit executor: ExecutionContext): Future[S] =
    future.flatMap ( t => S.initIfUninitted(session) ( f(t) )).withImplicitSession
  override def map[S](f: (T) ⇒ S)(implicit executor: ExecutionContext): Future[S] =
    future.map ( t => S.initIfUninitted(session) ( f(t) )).withImplicitSession
  override def recover[U >: T](pf: PartialFunction[Throwable, U])(implicit executor: ExecutionContext): Future[U] =
    future.recover { case t => S.initIfUninitted(session) ( pf(t) ) }.withImplicitSession
  override def recoverWith[U >: T](pf: PartialFunction[Throwable, Future[U]])(implicit executor: ExecutionContext): Future[U] =
    future.recoverWith { case t => S.initIfUninitted(session) ( pf(t) ) }.withImplicitSession
  override def transform[S](s: (T) ⇒ S, f: (Throwable) ⇒ Throwable)(implicit executor: ExecutionContext): Future[S] =
    future.transform( t => S.initIfUninitted(session) ( s(t) ), t => S.initIfUninitted(session) ( f(t) ) ).withImplicitSession
  override def zip[U](that: Future[U]): Future[(T, U)] = future.zip(that).withImplicitSession

  // Note that the following methods are implemented reusing existing methods: filter, foreach, mapTo, onFailure, onSuccess
  // We still have them covered by tests in case future changes to the Scala standard lib changes this.
}
