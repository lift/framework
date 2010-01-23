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
package mapper {

  import net.liftweb.mapper._
  import net.liftweb.common._
  import oauth._

  class MOAuthNonce extends LongKeyedMapper[MOAuthNonce] with OAuthNonce {
    def getSingleton = MOAuthNonce

    def primaryKeyField = id
    object id extends MappedLongIndex(this) {
      override def dbColumnName = "osn_id"
    }

    object _consumer_key extends MappedString(this, 64) {
      override def dbColumnName = "osn_consumer_key"
    }

    def consumerKey = _consumer_key.is

    protected object _token extends MappedString(this, 64) {
      override def dbColumnName = "osn_token"
    }

    def token = _token.is

    protected object _timestamp extends MappedLong(this) {
      override def dbColumnName = "osn_timestamp"
      override def dbIndexed_? = true
    }

    def timestamp = _timestamp.is

    protected object _nonce extends MappedString(this, 80) {
      override def dbColumnName = "osn_nonce"
      override def dbIndexed_? = true
    }

    def nonce = _nonce.is
  }

  object MOAuthNonce extends MOAuthNonce with LongKeyedMetaMapper[MOAuthNonce] with OAuthNonceMeta {
    override def dbTableName = "oauth_server_nonce"

    def create(consumerKey: String, token: String, timestamp: Long, nonce: String): Unit =
    (new MOAuthNonce)._consumer_key(consumerKey)._token(token)._timestamp(timestamp)._nonce(nonce).save


    def find(consumerKey: String, token: String, timestamp: Long, nonce: String): Box[OAuthNonce] =
    find(By(_consumer_key, consumerKey), By(_token, token), By(_timestamp, timestamp),
         By(_nonce, nonce))

    def bulkDelete_!!(minTimestamp: Long): Unit = this.bulkDelete_!!(By_<(_timestamp, minTimestamp))


  }

}
}
}
