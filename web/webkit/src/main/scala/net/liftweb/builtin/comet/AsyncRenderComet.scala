package net.liftweb
package builtin
package comet

import scala.xml.NodeSeq

import common._
import http._
  import js._
import util._
  import Helpers._

case class Compute(js: () => JsCmd)
private case class Render(js: JsCmd)

/**
 * `AsyncRenderComet` facilitates rendering anything that produces a `JsCmd`
 * independently from a page request. All you have to do is create one and
 * send it a `Compute` message with the function that will produce the `JsCmd`.
 * `AsyncRenderComet` will take ownership of the function and run it in
 * a separate thread comet context, sending the results down using a
 * `partialUpdate` when it is done.
 *
 * Note that if you want to run a function that requires the context of the
 * request you're running in, you'll want to use `LiftSession`'s
 * `buildDeferredFunction` method to make sure that when the function is
 * executed in a separate thread, it will retain request context.
 *
 * In general, consider using one of:
 *  - `AsyncRenderComet.asyncRender`.
 *  - The `lazy` snippet.
 *  - The `CanBind` implicits in the `net.liftweb.http` package that allow using
 *    `LAFuture` and Scala `Future` objects as the right-hand-side of a CSS
 *    selector binding.
 * 
 * None of these requires explicit use of `buildDeferredFunction`.
 */
class AsyncRenderComet extends MessageCometActor {

  override def lifespan: Box[TimeSpan] = Full(90.seconds)

  // make this method visible so that we can initialize the actor
  override def initCometActor(creationInfo: CometCreationInfo): Unit = {
    super.initCometActor(creationInfo)
  }

  // FIXME add lifecycle management that will nuke the comet here and on the
  // FIXME client when nothing is left to compute
  override def lowPriority : PartialFunction[Any, Unit] = {
    // farm the request off to another thread
    case Compute(js) => 
      Schedule.schedule(() => this ! Render(js()), 0.seconds)

    // render it
    case Render(js) => 
      partialUpdate(js)
  }
}

object AsyncRenderComet {
  private object pageAsyncRenderer extends TransientRequestVar[Box[AsyncRenderComet]](
    S.findOrCreateComet[AsyncRenderComet](
      cometName = Full(s"lazy-${S.renderVersion}"),
      cometHtml = NodeSeq.Empty,
      cometAttributes = Map.empty,
      receiveUpdatesOnPage = true
    )
  )

  /**
   * If you're going to be managing the asynchronicity of the render externally,
   * make sure to call this so that the async plumbing will be set up on the
   * page when it gets sent down.
   *
   * When possible, prefer the use of the `lazy` snippet, the `asyncRender`
   * function, or the `CanBind` implicits for `Future` and `LAFuture`.
   *
   * Returns a `Failure` if something went wrong with setting up the
   * asynchronous render.
   */
  def setupAsync: Box[Unit] = {
    // Dereference to make sure the comet exists.
    pageAsyncRenderer.is.map(_ => ()) ?~! "Failed to create async renderer."
  }

  /**
   * If you're going to be managing the asynchronicity of the render externally
   * (e.g., with futures), call this when you're ready to render your results
   * and the rendering will be sent down to the client.
   *
   * When possible, prefer the use of the `lazy` snippet, the `asyncRender`
   * function, or the `CanBind` implicits for `Future` and `LAFuture`.
   *
   * Returns a `Failure` if something went wrong with looking up the
   * asynchronous renderer.
   */
  def completeAsyncRender(command: JsCmd): Box[Unit] = {
    pageAsyncRenderer.is.map(_ ! Render(command)) ?~! "Failed to create async renderer."
  }

  /**
   * Render the given function on a separate thread and send the resulting
   * JavaScript to the current page when the function completes. Wraps the
   * function so that it is executed in the current request and session context.
   *
   * Returns a `Failure` if something went wrong with setting up the
   * asynchronous render.
   */
  def asyncRender(renderFunction: ()=>JsCmd): Box[Unit] = {
    for {
      session <- S.session ?~ "Asynchronous rendering requires a session context."
      renderer <- pageAsyncRenderer.is ?~! "Failed to create async renderer."
    } yield {
      renderer ! Compute(session.buildDeferredFunction(renderFunction))
    }
  }

  /**
   * Similar to `asyncRender`, but any wrapping of the function in a request
   * context is expected to be done before `renderFunction` is passed to this,
   * while `asyncRender` takes care of the wrapping for you.
   */
  def asyncRenderDeferred(renderFunction: ()=>JsCmd): Box[Unit] = {
    pageAsyncRenderer.is.map { renderer =>
      renderer ! Compute(renderFunction)
    } ?~! "Failed to create async renderer."
  }
}
