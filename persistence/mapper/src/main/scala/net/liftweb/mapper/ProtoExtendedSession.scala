/*
 * Copyright 2006-2010 WorldWide Conferencing, LLC
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
package mapper {

import _root_.net.liftweb._
import _root_.net.liftweb.http.provider._
import common._
import util._
import http._
import Helpers._


trait ProtoExtendedSession[T <: ProtoExtendedSession[T]] extends
KeyedMapper[Long, T] {
  self: T =>

  override def primaryKeyField = id

  // the primary key for the database
  object id extends MappedLongIndex(this)

  // uniqueId
  object cookieId extends MappedUniqueId(this, 32) {
    override def dbIndexed_? = true
  }

  object userId extends MappedString(this, 64)

  object expiration extends MappedLong(this) {
    override def defaultValue = expirationTime
    override def dbColumnName = expirationColumnName
  }

  /**
   * Change this string to "experation" for compatibility with
   * old mis-spelling
   */
  protected def expirationColumnName = "expiration"

  def expirationTime: Long = millis + 180.days
}

trait UserIdAsString {
  def userIdAsString: String
}

trait MetaProtoExtendedSession[T <: ProtoExtendedSession[T]] extends
KeyedMetaMapper[Long, T] {
  self: T =>

  def CookieName = "ext_id"
  type UserType <: UserIdAsString

  private object myWrapper extends LoanWrapper {
    def apply[N](f: => N): N = {
      (recoverUserId, S.findCookie(CookieName)) match {
        case (Empty, Full(c)) =>
          find(By(cookieId, c.value openOr "")) match {
            case Full(es) if es.expiration.is < millis => es.delete_!
            case Full(es) => logUserIdIn(es.userId)
            case _ =>
          }

        case _ =>
      }
      f
    }
  }

  def logUserIdIn(uid: String): Unit

  def recoverUserId: Box[String]

  def userDidLogin(uid: UserType) {
    userDidLogout(Full(uid))
    val inst = create.userId(uid.userIdAsString).saveMe
    val cookie = HTTPCookie(CookieName, inst.cookieId).
    setMaxAge(((inst.expiration.is - millis) / 1000L).toInt).
    setPath("/")
    S.addCookie(cookie)
  }

  def userDidLogout(uid: Box[UserType]) {
    for (cook <- S.findCookie(CookieName)) {
      S.deleteCookie(cook)
      find(By(cookieId, cook.value openOr "")).foreach(_.delete_!)
    }
  }

  def requestLoans: List[LoanWrapper] = myWrapper :: Nil
}

}
}
