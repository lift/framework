/*
 * Copyright 2011 WorldWide Conferencing, LLC
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

import scala.util.matching.Regex
import java.util.{Date, UUID}
import java.util.regex.Pattern

import org.bson.types.ObjectId
import org.joda.time.DateTime

object BsonDSL extends JsonDSL {
  implicit def objectid2jvalue(oid: ObjectId): JValue = JsonObjectId(oid)
  implicit def pattern2jvalue(p: Pattern): JValue = JsonRegex(p)
  implicit def regex2jvalue(r: Regex): JValue = JsonRegex(r.pattern)
  implicit def uuid2jvalue(u: UUID): JValue = JsonUUID(u)
  implicit def date2jvalue(d: Date)(implicit formats: Formats): JValue = JsonDate(d)
  implicit def datetime2jvalue(d: DateTime)(implicit formats: Formats): JValue = JsonDateTime(d)
}
