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

import net.liftweb.common.Full
import net.liftweb.mapper._
import net.liftweb.util.Helpers._
import scala.xml._
import oauth._

trait MOAuthConsumer[ModelType <: MOAuthConsumer[ModelType]] extends LongKeyedMapper[ModelType] with OAuthConsumer {
  self: ModelType =>

  type MOAuthTokenType <: MOAuthToken[MOAuthTokenType]
  def getMOAuthTokenMeta: MOAuthTokenMeta[MOAuthTokenType]

  type UserType <: KeyedMapper[Long, UserType] with OAuthUser

  def getUserMeta: KeyedMetaMapper[Long, UserType]

  override def delete_! = {
    getMOAuthTokenMeta.bulkDelete_!!(this.id.is)
    MOAuthNonce.bulkDelete_!!(By(MOAuthNonce._consumer_key, consumer_key.is))
    super.delete_!
  }

  def reset = {
    getMOAuthTokenMeta.bulkDelete_!!(this.id.is)
    MOAuthNonce.bulkDelete_!!(By(MOAuthNonce._consumer_key, consumer_key.is))
    consumer_key.reset
    consumer_secret.reset
    save
  }

  def primaryKeyField = id
  object id extends MappedLongIndex(this) {
    override def dbColumnName = "osr_id"
  }

  object _enabled extends MappedInt(this) {
    override def dbColumnName = "osr_enabled"
    override def defaultValue = 1
  }

  def enabled = _enabled.is

  object userid extends MappedLongForeignKey(this, getUserMeta) {
    override def dbColumnName = "osr_usa_id_ref"
  }

  def user: UserType = userid.obj.open_!

  object consumer_key extends MappedUniqueId(this, 48) {
    override def dbColumnName = "osr_consumer_key"
    override def writePermission_?  = true
  }

  def consumerKey = consumer_key.is

  object consumer_secret extends MappedUniqueId(this, 48) {
    override def dbColumnName = "osr_consumer_secret"
    override def writePermission_?  = true
  }

  def consumerSecret = consumer_secret.is

  object _title extends MappedString(this, 80) {
    override def dbColumnName = "osr_application_title"
    override def setFilter = trim _ :: super.setFilter
    override def validations =  valMinLen(1, "Application name is required") _ :: super.validations
  }

  def title = _title.is

  object application_uri extends MappedString(this, 255) {
    override def dbColumnName = "osr_application_uri"
    override def setFilter = trim _ :: super.setFilter
    override def validations =  valMinLen(1, "Application web site is required") _ :: super.validations
  }

  def applicationUri = application_uri.is

  object callback_uri extends MappedString(this, 255) {
    override def dbColumnName = "osr_callback_uri"
    override def setFilter = trim _ :: super.setFilter
    override def validations =  valMinLen(1, "Callback URL is required") _ :: super.validations
  }

  def callbackUri = callback_uri.is

  object _xdatetime extends MappedDateTime(this) {
    override def dbColumnName = "osr_issue_date"
    override def defaultValue = new java.util.Date
  }

  def xdatetime = _xdatetime.is
}

trait MOAuthConsumerMeta[ModelType <: MOAuthConsumer[ModelType]] extends MOAuthConsumer[ModelType] with
LongKeyedMetaMapper[ModelType] with OAuthConsumerMeta {
  self: ModelType =>
  override def dbTableName = "oauth_server_registry"
}

/*
 Example code

 class MyUser extends ProtoUser[MyUser] with OAuthUser {
 def getSingleton = MyUser
 }
 object MyUser extends MyUser with KeyedMetaMapper[Long, MyUser]

 class TestMOAuthConsumer extends MOAuthConsumer[TestMOAuthConsumer] {

 def getSingleton = TestMOAuthConsumer
 type UserType = MyUser
 def getUserMeta = MyUser
 type MOAuthTokenType = TestMOAuthToken
 def getMOAuthTokenMeta = TestMOAuthToken

 }

 object TestMOAuthConsumer extends TestMOAuthConsumer with MOAuthConsumerMeta[TestMOAuthConsumer]

 class TestMOAuthToken extends MOAuthToken[TestMOAuthToken] {
 def getSingleton = TestMOAuthToken

 type UserType = MyUser

 def getUserMeta = MyUser

 type MOAuthConsumerType = TestMOAuthConsumer
 def getMOAuthConsumerMeta = TestMOAuthConsumer
 }

 object TestMOAuthToken extends TestMOAuthToken with MOAuthTokenMeta[TestMOAuthToken]
 */

}
}
}
