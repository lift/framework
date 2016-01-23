package net.liftweb.http

import net.liftweb.actor.LAFuture
import net.liftweb.common.{Empty, Failure}

object LAFutureWithSession {
  implicit class LAFutureDecorator[T](future:LAFuture[T]) {
    val withCurrentSession:LAFuture[T] = S.session.map(new LAFutureWithSession[T](future)(_)).
      openOr {
        val f = new LAFuture[T]()
        f.fail(Failure("LiftSession not available in this thread context", Empty, Empty))
        f
      }

    def withImplicitSession(implicit session:LiftSession):LAFuture[T] = new LAFutureWithSession[T](future)
  }
}

class LAFutureWithSession[T](future:LAFuture[T])(implicit session:LiftSession) extends LAFuture[T] {
}