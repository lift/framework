/*
 * Copyright 2015 WorldWide Conferencing, LLC
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

import org.specs2.mutable.Specification

import actor.LAFuture

object CanResolveAsyncSpec extends Specification {
  "CanResolveAsync" should {
    "resolve Scala Futures" in {
      val myPromise = Promise[String]()

      val resolver = implicitly[CanResolveAsync[Future[String], String]]

      var receivedResolution: Option[String] = None
      resolver.resolveAsync(
        myPromise.future,
        { resolution => receivedResolution = Some(resolution) }
      )

      myPromise.success("All done!")

      receivedResolution must_== Some("All done!")
    }

    "resolve LAFutures" in {
      val myFuture = new LAFuture[String]

      val resolver = implicitly[CanResolveAsync[LAFuture[String], String]]

      var receivedResolution: Option[String] = None
      resolver.resolveAsync(
        myFuture,
        { resolution => receivedResolution = Some(resolution) }
      )

      myFuture.satisfy("Got it!")

      receivedResolution must_== Some("Got it!")
    }
  }
}
