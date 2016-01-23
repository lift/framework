package net.liftweb.http

import net.liftweb.actor.LAFuture
import net.liftweb.common.{Box, Empty, Failure}

/**
  * Contains implicit conversion for `net.liftweb.actor.LAFuture` to ease access to `LiftSession` resources.
  *
  * If you need to access session state (such as a `SessionVar`) within the context of an `LAFuture`, then import
  * `LAFutureWithSession` and convert your `LAFuture`s with either `withCurrentSession` or `withImplicitSession`.
  * Use `withCurrentSession` when you are already in the context of a `LiftSession`.  Use `withImplicitSession`
  * if you have a reference to the `LiftSession`.
  *
  * Note that the returned `LAFuture` writes through to the original, and each method which returns another
  * `LAFuture` also will be an `LAFutureWithSession`.  Hence any calls can be chained together, allowing an
  * `LAFutureWithSession` to work in an arbitrary for-comprehension.
  *
  * Full working example:
  * {{{
  * package code.snippet
  *
  * import net.liftweb.actor.LAFuture
  * import net.liftweb.http.{SessionVar, SHtml}
  * import net.liftweb.http.LAFutureWithSession._
  * import net.liftweb.http.js.JE.JsVar
  * import net.liftweb.http.js.JsCmds
  * import net.liftweb.util.Helpers._
  *
  * object MyVar extends SessionVar("init")
  *
  * object AjaxButton {
  *   def render = "type=button [onclick]" #>
  *     SHtml.ajaxCall(
  *       JsVar("window.myGlobal"),
  *       myGlobal => {
  *         futureOp(myGlobal)
  *           .withCurrentSession
  *           .foreach(MyVar.set(_))
  *         JsCmds.Noop
  *       }
  *     )
  *
  *   def futureOp(s:String):LAFuture[String] = LAFuture(() => "Back to the Future[T]: "+s)
  * }
  * }}}
  *
  * @see FutureWithSession
  */
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

private [http] class LAFutureWithSession[T](future:LAFuture[T])(implicit session:LiftSession) extends LAFuture[T](future.scheduler) {
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
