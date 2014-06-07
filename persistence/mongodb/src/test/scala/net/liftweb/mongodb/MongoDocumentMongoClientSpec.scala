/*
 * Copyright 2010-2014 WorldWide Conferencing, LLC
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

import java.util.{Calendar, Date, UUID}
import java.util.regex.Pattern

import org.bson.types.ObjectId
import com.mongodb._

import org.specs2.mutable.Specification

import json._

package mongoclienttestdocs {
  case class SessCollection(_id: ObjectId, name: String, dbtype: String, count: Int)
    extends MongoDocument[SessCollection] {

    def meta = SessCollection
  }

  object SessCollection extends MongoDocumentMeta[SessCollection] {
    override def formats = super.formats + new ObjectIdSerializer
    // create a unique index on name
    ensureIndex(("name" -> 1), true)
  }
}

/**
 * Systems under specification for MongoDocumentMongoClient.
 */
class MongoDocumentMongoClientSpec extends Specification with MongoTestKit {
  "MongoDocumentMongoClient Specification".title

  import mongoclienttestdocs._

  override def mongo = new MongoClient("127.0.0.1", 27017)

  "MongoClient example" in {

    checkMongoIsRunning

    val tc = SessCollection(ObjectId.get, "MongoSession", "db", 1)
    val tc2 = SessCollection(ObjectId.get, "MongoSession", "db", 1)
    val tc3 = SessCollection(ObjectId.get, "MongoDB", "db", 1)

    // save to db
    SessCollection.save(tc)
    SessCollection.save(tc2) must throwA[MongoException]
    SessCollection.save(tc3)

    success
  }

}
