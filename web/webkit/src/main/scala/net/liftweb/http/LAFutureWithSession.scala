package net.liftweb.http

import net.liftweb.actor.LAFuture
import net.liftweb.common.{Box, Empty, Failure}

object LAFutureWithSession {
  implicit class LAFutureDecorator[T](future:LAFuture[T]) {
    val withCurrentSession:LAFuture[T] = S.session.map(new LAFutureWithSession[T](future)(_)).
      openOr {
        val f = new LAFuture[T](future.scheduler)
        f.fail(Failure("LiftSession not available in this thread context", Empty, Empty))
        f
      }

    def withImplicitSession(implicit session:LiftSession):LAFuture[T] = new LAFutureWithSession[T](future)
  }
}

class LAFutureWithSession[T](future:LAFuture[T])(implicit session:LiftSession) extends LAFuture[T](future.scheduler) {
  import LAFutureWithSession._

  // The following methods don't need access to the session, but need to write through to the original future
  override def abort() = future.abort()
  override def isAborted: Boolean = future.isAborted
  override def complete_? : Boolean = future.complete_?
  override def fail(e: Box[Nothing]) = future.fail(e)
  override def satisfy(value: T): Unit = future.satisfy(value)
  override def get(timeout: Long): Box[T] = future.get(timeout)
  override def get: T = future.get
  override def isSatisfied: Boolean = future.isSatisfied

  // The following methods execute the function arguments in the session
  override def onComplete(f: Box[T] => Unit) = future.onComplete( b => S.initIfUninitted(session) { f(b) } )
  override def onFail(f: Box[Nothing] => Unit) = future.onFail( b => S.initIfUninitted(session) { f(b) } )
  override def onSuccess(f: T => Unit) = future.onSuccess( v => S.initIfUninitted(session) { f(v) } )

  // The following methods reuse existing methods, so no need to handle sessions here. Overriding so chaining
  // and hence for-comprehensions work.
  override def flatMap[A](f: T => LAFuture[A]): LAFuture[A] = future.flatMap(f).withImplicitSession
  override def filter(f: T => Boolean): LAFuture[T] = future.filter(f).withImplicitSession
  override def map[A](f: T => A): LAFuture[A] = future.map(f).withImplicitSession

  // The following methods are implemented reusing existing methods: complete, foreach, fail(Exception), and withFilter
  // We still have them covered by tests in case future changes to the LAFuture changes this.
}
