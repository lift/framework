/*
 * Copyright 2015-2026 Lift Committers and Contributors
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
package util

import scala.concurrent.{ExecutionContext,Future}

import actor.LAFuture

/**
 * Represents a unifying class that can resolve an asynchronous container.  For
 * example, a `Future[String]` can be resolved by a
 * `CanResolveAsync[Future[String], String]`.
 *
 * Provides one method, `[[resolveAsync]]`, that takes the async container and
 * a function to run when the container resolves.
 */
trait CanResolveAsync[ResolvableType, ResolvedType] {
  /**
   * Should return a function that, when given the resolvable and a function
   * that takes the resolved value, attaches the function to the resolvable
   * so that it will asynchronously execute it when its value is resolved.
   *
   * See `CanResolveFuture` and `CanResolveLAFuture` in `lift-webkit` for
   * example usages.
   */
  def resolveAsync(resolvable: ResolvableType, onResolved: (ResolvedType)=>Unit): Unit
}

trait LowPriorityCanResolveAsyncImplicits {
  self: CanResolveAsync.type =>

  // Low priority implicit for resolving Scala Futures.
  implicit def resolveFuture[T](implicit executionContext: ExecutionContext): CanResolveAsync[Future[T], T] = {
    new CanResolveAsync[Future[T],T] {
      def resolveAsync(future: Future[T], onResolved: (T)=>Unit) = {
        future.foreach(onResolved)
      }
    }
  }

  // Low priority implicit for resolving Lift LAFutures.
  implicit def resolveLaFuture[T]: CanResolveAsync[LAFuture[T], T] = {
    new CanResolveAsync[LAFuture[T],T] {
      def resolveAsync(future: LAFuture[T], onResolved: (T)=>Unit) = {
        future.onSuccess(onResolved)
      }
    }
  }
}
object CanResolveAsync extends LowPriorityCanResolveAsyncImplicits
