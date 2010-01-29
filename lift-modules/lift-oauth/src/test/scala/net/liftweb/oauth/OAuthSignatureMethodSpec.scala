/*
 * Copyright 2010 WorldWide Conferencing, LLC
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

package net.liftweb {
package oauth {

import _root_.org.specs._
import _root_.org.specs.runner._
import _root_.org.specs.Sugar._

import net.liftweb.http.GetRequest

import net.liftweb.oauth.OAuthUtil.Parameter

import net.liftweb.common.{Box,Full,Empty}
import java.util.Date
      
class OAuthSignatureMethodSpecTest extends Runner(OAuthSignatureMethodSpec) with JUnit with Console
object OAuthSignatureMethodSpec extends Specification {
  val oauthAccessor = OAuthAccessor(FakeConsumer("dpf43f3p2l4k3l03","kd94hf93k423kf44"), Full("pfkkdhi9sl3r4s00"), Empty)
  val hmacSha1 = HMAC_SHA1(oauthAccessor)
  
  "OAuthSignatureMethod" should {
    "normalizeUrl correctly" in {
      hmacSha1.normalizeUrl("htTp://a.com:80/b?foo=bar") must_== ("http://a.com/b")
      hmacSha1.normalizeUrl("htTp://a.com:123/b?foo=bar") must_== ("http://a.com:123/b")
      hmacSha1.normalizeUrl("htTps://a.com:443/b?foo=bar") must_== ("https://a.com/b")
      hmacSha1.normalizeUrl("htTp://a.com/b/f/") must_== ("http://a.com/b/f/")
      hmacSha1.normalizeUrl("htTp://a.com?foo=bar") must_== ("http://a.com/")
    }
  }
  
  "HMAC_SHA1" should {
    "Create expected signature from example at http://hueniverse.com/2008/10/beginners-guide-to-oauth-part-iv-signing-requests/" in {
      val oauthMessage = new OAuthMessage(GetRequest, "http://photos.example.net/photos", List(
        Parameter("file","vacation.jpg"),
        Parameter("size","original"),
        Parameter("oauth_consumer_key","dpf43f3p2l4k3l03"),
        Parameter("oauth_nonce","kllo9940pd9333jh"),
        Parameter("oauth_signature_method","HMAC-SHA1"),
        Parameter("oauth_timestamp","1191242096"),
        Parameter("oauth_token","nnch734d00sl2jdk"),
        Parameter("oauth_version","1.0")
      ))
      val baseString = hmacSha1.getBaseString(oauthMessage)
      hmacSha1.getSignature(baseString) must_== Full("tR3+Ty81lMeYAr/Fid0kMTYa/WM=")
    }
  }
  
  case class FakeConsumer(consumerKey:String, consumerSecret:String) extends OAuthConsumer {

    def reset{}

    def enabled: Int = 0

    def user: OAuthUser = new OAuthUser(){}

    def title: String = ""

    def applicationUri: String = ""

    def callbackUri: String = ""

    def xdatetime: Date = new Date(0)
  }
}

}
}