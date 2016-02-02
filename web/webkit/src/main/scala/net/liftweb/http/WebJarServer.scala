/*
 * Copyright 2016 WorldWide Conferencing, LLC
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
package http

import net.liftweb.common._
import net.liftweb.util._
import Helpers._

/**
 * Serve WebJar assets. This a fallback for when not using the builtin functionality in
 * Servlet 3 containers.
 *
 * This adds ''Expires'', ''Last-Modified'', and ''Cache-Control'' headers. Servlet 3
 * containers do not include any of these headers.
 */
object WebJarServer extends ResourceCache with SimpleInjector {

  /**
   * The TimeSpan for the ''Expires'' header. Override with:
   *
   * {{{WebJarServer.expiresTimeSpan.default.set(30.days)}}}
   */
  lazy val expiresTimeSpan: Inject[TimeSpan] = new Inject[TimeSpan](180.days) {}

  def serveResource(request: Req, uri: List[String])(): Box[LiftResponse] = {
    for {
      auri <- Full(uri.filter(!_.startsWith(".")))
      path = auri.mkString("/", "/", "")
      url <- LiftRules.getResource(s"/META-INF/resources$path")
      lastModified = calcLastModified(url)
    } yield {
      val expiresHeader = "Expires" -> toInternetDate(millis + expiresTimeSpan.vend)

      request.testFor304(lastModified, expiresHeader) openOr {
        val stream = url.openStream
        val uc = url.openConnection
        val headers =
          (
            if (lastModified == 0L) {
              Nil
            } else {
              List("Last-Modified" -> toInternetDate(lastModified))
            }
          ) :::
          List(
            expiresHeader,
            "Date" -> nowAsInternetDate,
            "Pragma" -> "",
            "Cache-Control" -> "public",
            "Content-Type" -> detectContentType(auri.last)
          )

        StreamingResponse(
          stream,
          () => stream.close,
          uc.getContentLength,
          headers,
          Nil,
          200
        )
      }
    }
  }
}
