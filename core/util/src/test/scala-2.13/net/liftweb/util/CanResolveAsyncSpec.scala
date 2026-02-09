/*
 * Copyright 2015 Lift Committers and Contributors
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

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

import org.specs2.mutable.Spec

import actor.LAFuture

object CanResolveAsyncSpec extends Spec {
  "CanResolveAsync" should {
    "resolve Scala Futures" in {
      val myPromise = Promise[String]()

      val resolver = implicitly[CanResolveAsync[Future[String], String]]

      val receivedResolution = new LAFuture[String]
      resolver.resolveAsync(myPromise.future, receivedResolution.satisfy _)

      myPromise.success("All done!")

      receivedResolution.get must_== "All done!"
    }

    "resolve LAFutures" in {
      val myFuture = new LAFuture[String]

      val resolver = implicitly[CanResolveAsync[LAFuture[String], String]]

      val receivedResolution = new LAFuture[String]
      resolver.resolveAsync(myFuture, receivedResolution.satisfy _)

      myFuture.satisfy("Got it!")

      receivedResolution.get must_== "Got it!"
    }
  }
}
