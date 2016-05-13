/**
  * Copyright 2014 WorldWide Conferencing, LLC
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

package net.liftweb
package mongodb

import json._
import util.Helpers.tryo
import JsonDSL._

import java.util.{Date, UUID}
import java.util.regex.Pattern

import org.bson.types.ObjectId
import org.joda.time.DateTime

object JsonObjectId {
  def unapply(json: JValue): Option[ObjectId] = {
    json match {
      case JObject(JField("$oid", JString(objectIdString)) :: Nil) if ObjectId.isValid(objectIdString) =>
        Some(new ObjectId(objectIdString))
      case _ =>
        None
    }
  }

  def apply(objectId: ObjectId): JValue = ("$oid" -> objectId.toString)

  def asJValue(objectId: ObjectId, formats: Formats): JValue =
    if (isObjectIdSerializerUsed(formats))
      apply(objectId)
    else
      JString(objectId.toString)

  /**
    * Check to see if the ObjectIdSerializer is being used.
    */
  private def isObjectIdSerializerUsed(formats: Formats): Boolean =
    formats.customSerializers.exists(_.getClass == objectIdSerializerClass)

  private val objectIdSerializerClass = classOf[net.liftweb.mongodb.ObjectIdSerializer]
}

object JsonRegex {
  def unapply(json: JValue): Option[Pattern] = {
    json match {
      case JObject(JField("$regex", JString(regex)) :: JField("$flags", JInt(f)) :: Nil) =>
        Some(Pattern.compile(regex, f.intValue))
      case _ =>
        None
    }
  }

  def apply(p: Pattern): JValue = ("$regex" -> p.pattern) ~ ("$flags" -> p.flags)
}

object JsonUUID {
  def unapply(json: JValue): Option[UUID] = {
    json match {
      case JObject(JField("$uuid", JString(s)) :: Nil) =>
        tryo(UUID.fromString(s))
      case _ =>
        None
    }
  }

  def apply(uuid: UUID): JValue = ("$uuid" -> uuid.toString)
}

object JsonDate {
  def unapply(json: JValue)(implicit formats: Formats): Option[Date] = {
    json match {
      case JObject(JField("$dt", JString(s)) :: Nil) =>
        formats.dateFormat.parse(s)
      case _ =>
        None
    }
  }

  def apply(dt: Date)(implicit formats: Formats): JValue = ("$dt" -> formats.dateFormat.format(dt))
}

object JsonDateTime {
  def unapply(json: JValue)(implicit formats: Formats): Option[DateTime] = {
    json match {
      case JObject(JField("$dt", JString(s)) :: Nil) =>
        formats.dateFormat.parse(s).map(dt => new DateTime(dt))
      case _ =>
        None
    }
  }

  def apply(dt: DateTime)(implicit formats: Formats): JValue = ("$dt" -> formats.dateFormat.format(dt.toDate))
}
