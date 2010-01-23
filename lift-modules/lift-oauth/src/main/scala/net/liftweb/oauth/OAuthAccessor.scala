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

import net.liftweb.common._


final case class OAuthAccessor(oauthConsumer: OAuthConsumer, tokenSecret: Box[String], user: Box[OAuthUser]) {
  def consumerSecret = oauthConsumer.consumerSecret
}


trait OAuthAccessorMeta
{

  protected def oauthTokenMeta: OAuthTokenMeta

  def fromAccessToken(message: OAuthMessage): Box[OAuthAccessor] = Empty /* {
    message.requireParameters(OAuth.OAUTH_TOKEN)
    oauthTokenMeta.find(message.getToken) match {
      case Full(token) if token.hasExpired_? => throw OAuthProblemException(OAuth.Problems.TOKEN_EXPIRED)
      case Full(token) if token.authorized.is == 1 && token.token_type == "access" => {
          new OAuthAccessor(token.consumer, token.secret, token.user)
        }
      case Full(token) => throw OAuthProblemException(OAuth.Problems.PERMISSION_DENIED)
      case _ => throw OAuthProblemException(OAuth.Problems.TOKEN_EXPIRED)
    }
  } */

  def fromAuthorizedRequestToken(message: OAuthMessage): Box[OAuthAccessor] = Empty /* {
    message.requireParameters(OAuth.OAUTH_TOKEN)
    oauthTokenMeta.find(message.getToken) match {
      case Full(token) if token.hasExpired_? => throw OAuthProblemException(OAuth.Problems.TOKEN_EXPIRED)
      case Full(token) if token.authorized.is == 1 && token.token_type == "request" => {
          new OAuthAccessor(token.consumer.obj.open_!, token.secret.is, token.userid.obj.open_!)
        }
      case Full(auth) => throw OAuthProblemException(OAuth.Problems.PERMISSION_DENIED)
      case _ => throw OAuthProblemException(OAuth.Problems.TOKEN_EXPIRED)
    }
  }
  */

  def apply(oauthConsumer: OAuthConsumer): Box[OAuthAccessor] = {
    Full(new OAuthAccessor(oauthConsumer, Empty, Empty))
  }
}

}
}
