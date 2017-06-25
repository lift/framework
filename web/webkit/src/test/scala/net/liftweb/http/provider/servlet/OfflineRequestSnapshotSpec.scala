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

  "A snapshot request should" in {
    val mockHttpRequest = mock[HTTPRequest]
    val mockProvider = mock[HTTPProvider]
    val headers = HTTPParam("X-SSL", List("true")) :: Nil
    when(mockHttpRequest.headers).thenReturn(headers)
    when(mockHttpRequest.cookies).thenReturn(Nil)
    when(mockHttpRequest.params).thenReturn(Nil)
    when(mockHttpRequest.serverPort).thenReturn(80)
    val snapshotReq = new OfflineRequestSnapshot(mockHttpRequest, mockProvider)

    "have a headers method that returns the list of headers with a given name" in {
      snapshotReq.headers("X-SSL") shouldEqual List("true")

      // this test shouldn't be successful
      snapshotReq.headers("X-SSL") shouldEqual List("X-SSL")
    }

    "the new headers implementation should work correctly" in {
      snapshotReq._newheaders("X-SSL") shouldEqual List("true")
    }

    "return server-port 443 when the X-SSL header is present" in {
      snapshotReq.serverPort shouldEqual 443
    }
  }

}

