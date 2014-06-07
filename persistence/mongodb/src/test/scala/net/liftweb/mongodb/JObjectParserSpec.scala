/*
 * Copyright 2012 WorldWide Conferencing, LLC
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
import JsonDSL._
import util.Helpers._

import org.bson.types.ObjectId
import org.specs2.mutable.Specification

import com.mongodb.DBObject

object JObjectParserSpec extends Specification  {
  "JObjectParser Specification".title

  def buildTestData: (ObjectId, DBObject) = {
    val oid = ObjectId.get
    val dbo = JObjectParser.parse(("x" -> oid.toString))(DefaultFormats)
    (oid, dbo)
  }

  "JObjectParser" should {
    "convert strings to ObjectId by default" in {
      val (oid, dbo) = buildTestData
      val xval = tryo(dbo.get("x").asInstanceOf[ObjectId])

      xval.toList map { x =>
        x must_== oid
      }

      xval.isDefined must_== true
    }
    "not convert strings to ObjectId when configured not to" in {
      JObjectParser.stringProcessor.doWith((s: String) => s) {
        val (oid, dbo) = buildTestData
        val xval = tryo(dbo.get("x").asInstanceOf[String])

        xval.toList map { x =>
          x must_== oid.toString
        }

        xval.isDefined must_== true
      }
    }
  }
}
