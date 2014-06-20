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

import common._
import util.Helpers._

import org.specs2.mutable._

import org.bson.types.ObjectId

case class CollectionNameTestDoc(_id: ObjectId) extends MongoDocument[CollectionNameTestDoc] {
  def meta = CollectionNameTestDoc
}
object CollectionNameTestDoc extends MongoDocumentMeta[CollectionNameTestDoc]

/**
  * Systems under specification for MongoRules.
  */
object MongoRulesSpec extends Specification {
  "Mongo Rules Specification".title
  sequential

  "MongoRules" should {
    "default collection name" in {
      CollectionNameTestDoc.collectionName must_== "collectionnametestdocs"
    }
    "snakify collection name" in {
      MongoRules.collectionName.doWith((_, name) => snakify(name)+"s") {
        CollectionNameTestDoc.collectionName must_== "net.liftweb.mongodb.collection_name_test_docs"
      }
    }
  }
}
