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
package ext_api {
package facebook {

import _root_.net.liftweb.http.{S}
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.common.{Box, Empty, Failure, Full}
import scala.xml.{Node}

object FacebookConnect extends FacebookConnect(FacebookRestApi.apiKey, FacebookRestApi.secret)

class FacebookConnect(apiKey:String, apiSecret:String) {

  private val prefix = apiKey + "_"

  def toSession(params: List[(String,String)]):Box[FacebookSession] = toSession(Map(params:_*))
  
  def toSession(asMap: Map[String,String]):Box[FacebookSession] = {
      for { key <- asMap.get("session_key")
            expirationStr <- asMap.get("expires")
            expiration <- asLong(expirationStr)
            uid <- asMap.get("user") } yield FacebookSession(key, expiration, uid)
  }

  def session:Box[FacebookSession] = for (p <- verifyCookiesSig; s <- toSession(p)) yield s
  
  def client:Box[FacebookClient[Node]] = session.map(session => new FacebookClient(apiKey, apiSecret, session, FacebookClient.xmlParser, FacebookFormat.xml))
  
  def verifyParams(params: List[(String,String)], sig: String):Box[List[(String,String)]] = {
    val actualSig = FacebookClient.genSignature(params, apiSecret)
    if (actualSig.equals(sig))
      Full(params)
    else {
      Failure("Invalid Signature expected %s but was %s".format(sig, actualSig))
    }
  }
  
  def verifyCookiesSig:Box[List[(String,String)]] = S.findCookie(apiKey) match {
    case Full(sigCookie) => 
      val fbCookies = for(c <- S.receivedCookies; value <- c.value;  if c.name.startsWith(prefix)) yield (c.name.substring(prefix.length), value)
      val ret = verifyParams(fbCookies, sigCookie.value.openOr(""))
      if (ret.isEmpty) {
        for(c <- S.receivedCookies if c.name.startsWith(apiKey)){
          S.addCookie(c.setMaxAge(0))
        }
      }
      ret
    case _ => Empty
  }
}

}
}
}
