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

import net.liftweb._
import common._
import util._
import java.util.Date

trait OAuthNonce {
  def consumerKey: String
  def token: String
  def timestamp: Long
  def nonce: String
}

trait OAuthNonceMeta {
  def create(consumerKey: String, token: String, timestamp: Long, nonce: String): Unit
  def find(consumerKey: String, token: String, timestamp: Long, nonce: String): Box[OAuthNonce]
  def bulkDelete_!!(minTimestamp: Long): Unit
}

trait OAuthUser {

}

trait OAuthUserMeta {
  def findByUsernameAndPassword(username: String, password: String): Box[OAuthUser]
}

trait OAuthTokenMeta {
  def find(token: OAuthUtil.Parameter): Box[OAuthToken]
  def create(consumer: OAuthConsumer, user: Box[OAuthUser],
             tokenType: String, authorized: Int, ttl: Date): OAuthToken

  def create(consumer: OAuthConsumer, user: OAuthUser,
             tokenType: String): OAuthToken

  def bulkDelete_!!(token: Box[OAuthUtil.Parameter]): Unit
}

trait OAuthConsumer {

  def reset: Unit

  def enabled: Int

  def user: OAuthUser

  def consumerKey: String

  def consumerSecret: String

  def title: String

  def applicationUri: String

  def callbackUri: String

  def xdatetime: Date
}

trait OAuthConsumerMeta {
  def find(consumerKey: String): Box[OAuthConsumer]
}

trait OAuthToken {
  def consumer: OAuthConsumer

  def user: OAuthUser

  def token: String

  def secret: String

  def tokenType: String

  def authorized: Int

  def referrerHost: String

  def ttl: Date

  def xdatetime: Date

  def hasExpired_? = ttl.getTime < Helpers.millis
}

}
}
