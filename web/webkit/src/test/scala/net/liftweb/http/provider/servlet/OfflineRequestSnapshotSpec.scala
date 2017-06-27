/*
 * Copyright 2010-2011 WorldWide Conferencing, LLC
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

package net.liftweb.http.provider.servlet

import net.liftweb.http.provider._
import net.liftweb.mockweb.WebSpec
import org.mockito.Mockito._
import org.specs2.mock.Mockito


object OfflineRequestSnapshotSpec extends WebSpec with Mockito {

  private[this] val X_SSL = "X-SSL"

  "OfflineRequestSnapshot" should {
    "have a 'headers' method that returns the list of headers with a given name" in {
      val req = getRequestSnapshot(originalPort = 80)
      req.headers("X-SSL") shouldEqual List("true")
      req.headers("Unknown") must beEmpty
    }

    "have the serverPort value" in {
      "443 when the 'X-SSL' header is set to the string 'true' (case-insensitive) and original port is 80" in {
        val port80Req = getRequestSnapshot(originalPort = 80)
        port80Req.serverPort shouldEqual 443
      }
      s"equal to the original request-port when the '$X_SSL' header is absent or not set to the string 'true' (case-insensitive), or if the original port is not 80" in {
        val req = getRequestSnapshot(originalPort = 90)
        val nonSSLReq = getRequestSnapshot(originalPort = 80, headers =  Nil)
        val falseSSLHeaderReq = getRequestSnapshot(originalPort = 90, headers =  HTTPParam(X_SSL, List("anything")) :: Nil)
        req.serverPort shouldEqual 90
        nonSSLReq.serverPort shouldEqual 80
        falseSSLHeaderReq.serverPort shouldEqual 90
      }
    }

    "have a 'param' method that returns the list of parameters with a given name (case-sensitive)" in {
      val tennisParams = List("Roger Federer", "Raphael Nadal")
      val swimmingParams = List("Michael Phelps", "Ian Thorpe")
      val params = HTTPParam("tennis", tennisParams) :: HTTPParam("swimming", swimmingParams) :: Nil
      val snapshot = getRequestSnapshot(80, params = params)

      snapshot.param("tennis") shouldEqual tennisParams
      snapshot.param("Tennis") should beEmpty
      snapshot.param("swimming") shouldEqual swimmingParams
    }
  }

  private[this] val xSSLHeader = HTTPParam(X_SSL, List("true")) :: Nil

  private[this] def getRequestSnapshot(originalPort: Int, headers: List[HTTPParam] = xSSLHeader, params: List[HTTPParam] = Nil) = {
    val mockHttpRequest = mock[HTTPRequest]
    val httpProvider = new HTTPProvider {
      override protected def context: HTTPContext = null
    }

    when(mockHttpRequest.headers).thenReturn(headers)
    when(mockHttpRequest.cookies).thenReturn(Nil)
    when(mockHttpRequest.params).thenReturn(params)
    when(mockHttpRequest.serverPort).thenReturn(originalPort)
    new OfflineRequestSnapshot(mockHttpRequest, httpProvider)
  }

}

