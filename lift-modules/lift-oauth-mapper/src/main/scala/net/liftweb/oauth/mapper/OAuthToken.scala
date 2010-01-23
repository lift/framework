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

  import java.util.Date
  import net.liftweb.mapper._
  import net.liftweb.common._
  import net.liftweb.util.Helpers._
  import oauth._

  trait MOAuthToken[ModelType <: MOAuthToken[ModelType]] extends LongKeyedMapper[ModelType] with OAuthToken {
    self: ModelType =>

    type UserType <: KeyedMapper[Long, UserType] with OAuthUser

    def getUserMeta: KeyedMetaMapper[Long, UserType]

    type MOAuthConsumerType <: MOAuthConsumer[MOAuthConsumerType] with LongKeyedMapper[MOAuthConsumerType]

    def getMOAuthConsumerMeta: MOAuthConsumerMeta[MOAuthConsumerType] with LongKeyedMetaMapper[MOAuthConsumerType]

    def primaryKeyField = id
    object id extends MappedLongIndex(this) {
      override def dbColumnName = "ost_id"
    }
    object _consumer extends MappedLongForeignKey[ModelType, MOAuthConsumerType](this, getMOAuthConsumerMeta) {
      override def dbColumnName = "ost_osr_id_ref"
    }

    def consumer = _consumer.obj.open_!

    object userid extends MappedLongForeignKey(this, getUserMeta) {
      override def dbColumnName = "ost_usa_id_ref"
    }

    def user: UserType = userid.obj.open_!

    object _token extends MappedUniqueId(this, 48) {
      override def dbColumnName = "ost_token"
    }

    def token = _token.is

    object _secret extends MappedUniqueId(this, 48) {
      override def dbColumnName = "ost_token_secret"
    }

    def secret = _secret.is

    object token_type extends MappedString(this, 32) {
      override def dbColumnName = "ost_token_type"
    }

    def tokenType = token_type.is

    object _authorized extends MappedInt(this) {
      override def dbColumnName = "ost_authorized"
      override def defaultValue = 0
    }

    def authorized = _authorized.is

    object referrer_host extends MappedString(this, 128) {
      override def dbColumnName = "ost_referrer_host"
      override def defaultValue = ""
    }

    def referrerHost = referrer_host.is

    object _ttl extends MappedDateTime(this) {
      override def dbColumnName = "ost_token_ttl"
      override def defaultValue = 60.minutes.later
    }

    def ttl = _ttl.is

    object _xdatetime extends MappedDateTime(this) {
      override def dbColumnName = "ost_timestamp"
      override def defaultValue = new Date
    }

    def xdatetime = _xdatetime.is
  }

  trait MOAuthTokenMeta[ModelType <: MOAuthToken[ModelType]] extends MOAuthToken[ModelType] with LongKeyedMetaMapper[ModelType] {
    self: ModelType =>
    override def dbTableName = "oauth_server_token"

    def bulkDelete_!!(what: Long): Unit =
    this.bulkDelete_!!(Cmp[ModelType, Long](_consumer,
                                            OprEnum.Eql, Full(what), Empty, Empty))
  }

}
}
}
