package net.liftweb.http

import net.liftweb.actor.LAFuture
import net.liftweb.common._

/**
  * Contains implicit conversion for `net.liftweb.actor.LAFuture` to ease access to `LiftSession` resources.
  *
  * If you need to access session state (such as a `SessionVar`) within the context of an `LAFuture`, then import
  * `LAFutureWithSession` and convert your `LAFuture`s with either `withCurrentSession` or `withImplicitSession`.
  * Use `withCurrentSession` when you are already in the context of a `LiftSession`.  Use `withImplicitSession`
  * if you instead have a reference to the `LiftSession`.
  *
  * The `LAFuture` returned by `withCurrentSession` or `withImplicitSession` is satisfied by the same value
  * as the original `LAFuture` and each method which returns another `LAFuture` also will be an `LAFutureWithSession`.
  * Hence any calls can be chained together, allowing an `LAFutureWithSession` to work in an arbitrary for-comprehension.
  *
  * Note that the returned `LAFuture` does NOT write through to the original, hence satisfying the
  * `LAFutureWithSession` will have no bearing on the original `LAFuture`.
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
  def sessionWrapper(session:LiftSession):LAFuture.ContextFn = Some(new CommonLoanWrapper {
    override def apply[T](f: => T): T = S.initIfUninitted(session) { f }
  })

  private def withSession[T](f:LAFuture[T], s:LiftSession):LAFuture[T] = {
    val sf = new LAFuture[T](f.scheduler, sessionWrapper(s))
    f.onComplete(sf.complete)
    sf
  }

  private def withFailure[T](f:LAFuture[T], failure:Failure):LAFuture[T] = {
    val ff = new LAFuture[T](f.scheduler)
    ff.complete(failure)
    ff
  }

  implicit class LAFutureDecorator[T](val future:LAFuture[T]) extends AnyVal {
    def withCurrentSession:LAFuture[T] = S.session match {
      case Full(s) =>    withSession(future, s)
      case f:EmptyBox => withFailure(future, f ?~! "LiftSession not available in this thread context")
    }

    def withImplicitSession(implicit session:LiftSession):LAFuture[T] = withSession(future, session)
  }
}
