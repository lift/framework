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

import BsonDSL._
import json._

import scala.util.matching.Regex

import java.util.{Date, UUID}
import java.util.regex.Pattern

import org.bson.types.ObjectId
import org.specs.Specification

import com.mongodb.{DBObject}

object BsonDSLSpec extends Specification("BsonDSL Specification") {
  "BsonDSL" should {
    "Convert ObjectId properly" in {
      val oid: ObjectId = ObjectId.get
      val qry: JObject = ("id" -> oid)
      val dbo: DBObject = JObjectParser.parse(qry)(DefaultFormats)

      dbo.get("id") must_== oid
    }

    "Convert Pattern properly" in {
      val ptrn: Pattern = Pattern.compile("^Mongo", Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)
      val qry: JObject = ("ptrn" -> ptrn)
      val dbo: DBObject = JObjectParser.parse(qry)(DefaultFormats)
      val ptrn2: Pattern = dbo.get("ptrn").asInstanceOf[Pattern]

      ptrn2.pattern must_== ptrn.pattern
      ptrn2.flags must_== ptrn.flags
    }

    "Convert Regex properly" in {
      val regex: Regex = "^Mongo".r
      val qry: JObject = ("regex" -> regex)
      val dbo: DBObject = JObjectParser.parse(qry)(DefaultFormats)
      val ptrn: Pattern = dbo.get("regex").asInstanceOf[Pattern]

      regex.pattern.pattern must_== ptrn.pattern
      regex.pattern.flags must_== ptrn.flags
    }

    "Convert UUID properly" in {
      val uuid: UUID = UUID.randomUUID
      val qry: JObject = ("uuid" -> uuid)
      val dbo: DBObject = JObjectParser.parse(qry)(DefaultFormats)

      dbo.get("uuid") must_== uuid
    }

    "Convert Date properly" in {
      implicit val formats = DefaultFormats.lossless
      val dt: Date = new Date
      val qry: JObject = ("now" -> dt)
      val dbo: DBObject = JObjectParser.parse(qry)

      dbo.get("now") must_== dt
    }
  }
}
