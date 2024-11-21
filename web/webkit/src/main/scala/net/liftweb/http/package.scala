/*
 * Copyright 2011 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb

import scala.concurrent.{ExecutionContext,Future}
import scala.xml.{Comment,NodeSeq}

import actor.LAFuture
import builtin.comet.AsyncRenderComet
import http.js.JsCmds.Replace
import util._

package object http {
  /**
   * Provides support for binding anything that has a `CanResolveAsync`
   * implementation. Out of the box, that's just Scala `Future`s and
   * `LAFuture`s, but it could just as easily be, for example, Twitter `Future`s
   * if you're using Finagle; all you have to do is add a `CanResolveAsync`
   * implicit for it.
   */
  implicit def asyncResolvableTransform[ResolvableType, ResolvedType](
    implicit asyncResolveProvider: CanResolveAsync[ResolvableType,ResolvedType],
             innerTransform: CanBind[ResolvedType]
  ): CanBind[ResolvableType] = {
    new CanBind[ResolvableType] {
      def apply(resolvable: =>ResolvableType)(ns: NodeSeq): Seq[NodeSeq] = {
        val placeholderId = Helpers.nextFuncName
        AsyncRenderComet.setupAsync

        val concreteResolvable: ResolvableType = resolvable

        S.session.map { session =>
          // Capture context now.
          val deferredRender =
            session.buildDeferredFunction((resolved: ResolvedType) => {
              AsyncRenderComet.completeAsyncRender(
                Replace(placeholderId, innerTransform(resolved)(ns).flatten)
              )
            })

          // Actually complete the render once the future is fulfilled.
          asyncResolveProvider.resolveAsync(concreteResolvable, resolvedResult => deferredRender(resolvedResult))

          <div id={placeholderId}><img src={s"${LiftRules.assetRootPath}images/ajax-loader.gif"} alt="Loading" /></div>
        } openOr {
          Comment("FIX"+"ME: Asynchronous rendering failed for unknown reason.")
        }
      }
    }
  }
}

