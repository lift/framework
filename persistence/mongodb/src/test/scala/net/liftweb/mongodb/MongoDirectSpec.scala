/*
 * Copyright 2010-2020 WorldWide Conferencing, LLC
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

import net.liftweb.util.{Helpers, DefaultConnectionIdentifier}

import scala.collection.JavaConverters._
import java.util.UUID
import java.util.regex.Pattern

import org.bson.Document
import com.mongodb.{WriteConcern, BasicDBObject, BasicDBObjectBuilder, MongoException}
import com.mongodb.client.model.{IndexOptions, ReplaceOptions, UpdateOptions}
import com.mongodb.client.model.Filters.{and, eq => eqs}

import org.specs2.mutable.Specification

import json.DefaultFormats
import net.liftweb.common.Failure


/**
 * System under specification for MongoDirect.
 */
class MongoDirectSpec extends Specification with MongoTestKit {
  "MongoDirect Specification".title

  def date(s: String) = DefaultFormats.dateFormat.parse(s).get

  "Mongo tutorial example" in {

    checkMongoIsRunning

    // build the DBObject
    val doc = new Document

    doc.put("name", "MongoDB")
    doc.put("type", "database")
    doc.put("count", 1: java.lang.Integer)

    val info = new Document

    info.put("x", 203: java.lang.Integer)
    info.put("y", 102: java.lang.Integer)

    doc.put("info", info)

    // use the Mongo instance directly
    MongoDB.useDatabase(DefaultConnectionIdentifier) { db =>
      val coll = db.getCollection("testCollection")

      // save the doc to the db
      coll.insertOne(doc)

      // get the doc back from the db and compare them
      coll.find().first() must_== doc

      // upsert
      doc.put("type", "document")
      doc.put("count", 2: java.lang.Integer)
      val q = new Document("name", "MongoDB") // the query to select the document(s) to update
      val o = doc // the new object to update with, replaces the entire document, except possibly _id
      val ropts = new ReplaceOptions().upsert(false)
      coll.replaceOne(q, o, ropts)

      // get the doc back from the db and compare
      coll.find.first.get("type") must_== "document"
      coll.find.first.get("count") must_== 2

      // modifier operations $inc, $set, $push...
      val o2 = new Document
      o2.put("$inc", new Document("count", 1)) // increment count by 1
      o2.put("$set", new Document("type", "docdb")) // set type
      val uopts = new UpdateOptions().upsert(false)
      coll.updateOne(q, o2, uopts)

      // get the doc back from the db and compare
      coll.find.first.get("type") must_== "docdb"
      coll.find.first.get("count") must_== 3

      if (!debug) {
        // delete it
        coll.deleteOne(new Document("_id", doc.get("_id")))
        coll.countDocuments() must_== 0
        coll.drop
      }

      success
    }
  }

  "Mongo tutorial 2 example" in {

    checkMongoIsRunning

    // use a DBCollection directly
    MongoDB.useMongoCollection("iDoc", classOf[Document]) { coll =>
      // insert multiple documents
      for (i <- List.range(1, 101)) {
        coll.insertOne(new Document().append("i", i))
      }

      // create an index
      coll.createIndex(new Document("i", 1))  // create index on "i", ascending

      // count the docs
      coll.countDocuments() must_== 100

      // get the count using a query
      coll.countDocuments(new Document("i", new Document("$gt", 50))) must_== 50

      // use a cursor to get all docs
      val cur = coll.find

      cur.iterator.asScala.toList.size must_== 100

      // get a single document with a query ( i = 71 )
      val query = new Document("i", 71)
      val cur2 = coll.find(query)

      cur2.iterator.asScala.toList.size must_== 1
      cur2.first.get("i") must_== 71

      // get a set of documents with a query
      // e.g. find all where i > 50
      val cur3 = coll.find(new Document("i", new Document("$gt", 50)))

      cur3.iterator.asScala.toList.size must_== 50

      // range - 20 < i <= 30
      val cur4 = coll.find(new Document("i", new Document("$gt", 20).append("$lte", 30)))

      cur4.iterator.asScala.toList.size must_== 10

      // limiting result set
      val cur5 = coll.find(new Document("i", new Document("$gt", 50))).limit(3).iterator

      var cntr5 = 0
      while(cur5.hasNext) {
        cur5.next
        cntr5 += 1
      }
      cntr5 must_== 3

      // skip
      val cur6 = coll.find(new Document("i", new Document("$gt", 50))).skip(10).iterator

      var cntr6 = 0
      while(cur6.hasNext) {
        cntr6 += 1
        cur6.next.get("i") must_== 60+cntr6
      }
      cntr6 must_== 40

      /* skip and limit */
      val cur7 = coll.find.skip(10).limit(20).iterator

      var cntr7 = 0
      while(cur7.hasNext) {
        cntr7 += 1
        cur7.next.get("i") must_== 10+cntr7
      }
      cntr7 must_== 20

      // sorting
      val cur8 = coll.find.sort(new Document("i", -1)).iterator // descending

      var cntr8 = 100
      while(cur8.hasNext) {
        cur8.next.get("i") must_== cntr8
        cntr8 -= 1
      }

      // remove some docs by a query
      coll.deleteMany(new Document("i", new Document("$gt", 50)))

      coll.countDocuments() must_== 50

      if (!debug) {
        // delete the rest of the rows
        coll.deleteMany(new Document("i", new Document("$lte", 50)))
        coll.countDocuments() must_== 0
        coll.drop
      }
    }
    success
  }

  "Mongo more examples" in {

    checkMongoIsRunning

    // use a Mongo instance directly
    MongoDB.useDefaultDatabase { db =>
      val coll = db.getCollection("testCollection")

      // create a unique index on name
      coll.createIndex(new Document("name", 1), (new IndexOptions).unique(true))

      // build the DBObjects
      val doc = new Document
      val doc2 = new Document
      val doc3 = new Document

      doc.put("name", "MongoSession")
      doc.put("type", "db")
      doc.put("count", 1: java.lang.Integer)

      doc2.put("name", "MongoSession")
      doc2.put("type", "db")
      doc2.put("count", 1: java.lang.Integer)

      doc3.put("name", "MongoDB")
      doc3.put("type", "db")
      doc3.put("count", 1: java.lang.Integer)

      // save the docs to the db
      Helpers.tryo(coll.withWriteConcern(WriteConcern.ACKNOWLEDGED).insertOne(doc)).toOption must beSome
      coll.withWriteConcern(WriteConcern.ACKNOWLEDGED).insertOne(doc2) must throwA[MongoException]
      Helpers.tryo(coll.withWriteConcern(WriteConcern.ACKNOWLEDGED).insertOne(doc2)) must beLike {
        case Failure(msg, _, _) =>
          msg must contain("E11000")
      }
      Helpers.tryo(coll.withWriteConcern(WriteConcern.ACKNOWLEDGED).insertOne(doc3)).toOption must beSome

      // query for the docs by type
      val qry = eqs("type", "db")
      coll.countDocuments(qry) must_== 2

      // modifier operations $inc, $set, $push...
      val o2 = new Document
      o2.put("$inc", new Document("count", 1)) // increment count by 1
      coll.updateOne(qry, o2).getModifiedCount must_== 1
      coll.updateOne(qry, o2).getMatchedCount must_== 1

      // this update query won't find any docs to update
      coll.updateOne(eqs("name", "None"), o2).getModifiedCount must_== 0

      // regex query example
      val key = "name"
      val regex = "^Mongo"
      coll.countDocuments(eqs(key, Pattern.compile(regex))) must_== 2

      // use regex and another dbobject
      coll.countDocuments(and(eqs(key, Pattern.compile(regex)), eqs("count", 1))) must_== 1

      if (!debug) {
        // delete them
        coll.deleteMany(eqs("type", "db")).getDeletedCount must_== 2
        coll.countDocuments must_== 0
        coll.drop
      }
    }
    success
  }

  "UUID Example" in {

    checkMongoIsRunning

    MongoDB.useMongoCollection("examples.uuid", classOf[Document]) { coll =>
      val uuid = UUID.randomUUID
      val doc = new Document("_id", uuid).append("name", "doc")
      coll.insertOne(doc)

      val qry = eqs("_id", uuid)
      val fromDb = coll.find(qry).first

      fromDb.get("_id") must_== doc.get("_id")
      fromDb.get("name") must_== doc.get("name")
    }
  }
}
