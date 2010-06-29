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
package mongodb {

import java.util.{Date, UUID}
import java.util.regex.Pattern

import org.specs.Specification
import org.specs.runner.JUnit4

import net.liftweb.json.DefaultFormats
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonParser._
import net.liftweb.json.JsonDSL._

import com.mongodb.{BasicDBObject, BasicDBObjectBuilder, DBObject}

class MongoDirectExamplesTest extends JUnit4(MongoDirectExamples)

object MongoDirectExamples extends Specification with MongoTestKit {

  def date(s: String) = DefaultFormats.dateFormat.parse(s).get

  "Mongo tutorial example" in {

    checkMongoIsRunning

    // build the DBObject
    val doc = new BasicDBObject

    doc.put("name", "MongoDB")
    doc.put("type", "database")
    doc.put("count", 1)

    val info = new BasicDBObject

    info.put("x", 203)
    info.put("y", 102)

    doc.put("info", info)

    // use the Mongo instance directly
    MongoDB.use(DefaultMongoIdentifier) ( db => {
      val coll = db.getCollection("testCollection")

      // save the doc to the db
      coll.save(doc)

      // get the doc back from the db and compare them
      coll.findOne must_== doc

      // upsert
      doc.put("type", "document")
      doc.put("count", 2)
      val q = new BasicDBObject("name", "MongoDB") // the query to select the document(s) to update
      val o = doc // the new object to update with, replaces the entire document, except possibly _id
      val upsert = false // if the database should create the element if it does not exist
      val apply = false // if an _id field should be added to the new object
      coll.update(q, o, upsert, apply)

      // get the doc back from the db and compare
      coll.findOne.get("type") must_== "document"
      coll.findOne.get("count") must_== 2

      // modifier operations $inc, $set, $push...
      val o2 = new BasicDBObject
      o2.put("$inc", new BasicDBObject("count", 1)) // increment count by 1
      o2.put("$set", new BasicDBObject("type", "docdb")) // set type
      coll.update(q, o2, false, false)

      // get the doc back from the db and compare
      coll.findOne.get("type") must_== "docdb"
      coll.findOne.get("count") must_== 3

      if (!debug) {
        // delete it
        coll.remove(new BasicDBObject("_id", doc.get("_id")))
        coll.find.count must_== 0
        coll.drop
      }

      // server-side eval
      val six = db.eval(" function() { return 3+3; } ")
      six must_== 6
    })
  }

  "Mongo tutorial 2 example" in {

    checkMongoIsRunning

    // use a DBCollection directly
    MongoDB.useCollection("iDoc") ( coll => {
      // insert multiple documents
      for (i <- List.range(1, 101)) {
        coll.insert(new BasicDBObject().append("i", i))
      }

      // create an index
      coll.createIndex(new BasicDBObject("i", 1))  // create index on "i", ascending

      // count the docs
      coll.getCount must_== 100

      // get the count using a query
      coll.getCount(new BasicDBObject("i", new BasicDBObject("$gt", 50))) must_== 50

      // use a cursor to get all docs
      val cur = coll.find

      cur.count must_== 100

      // get a single document with a query ( i = 71 )
      val query = new BasicDBObject
      query.put("i", 71)
      val cur2 = coll.find(query)

      cur2.count must_== 1
      cur2.next.get("i") must_== 71

      // get a set of documents with a query
      // e.g. find all where i > 50
      val cur3 = coll.find(new BasicDBObject("i", new BasicDBObject("$gt", 50)))

      cur3.count must_== 50

      // range - 20 < i <= 30
      val cur4 = coll.find(new BasicDBObject("i", new BasicDBObject("$gt", 20).append("$lte", 30)))

      cur4.count must_== 10

      // limiting result set
      val cur5 = coll.find(new BasicDBObject("i", new BasicDBObject("$gt", 50))).limit(3)

      var cntr5 = 0
      while(cur5.hasNext) {
        cur5.next
        cntr5 += 1
      }
      cntr5 must_== 3

      // skip
      val cur6 = coll.find(new BasicDBObject("i", new BasicDBObject("$gt", 50))).skip(10)

      var cntr6 = 0
      while(cur6.hasNext) {
        cntr6 += 1
        cur6.next.get("i") must_== 60+cntr6
      }
      cntr6 must_== 40

      /* skip and limit */
      val cur7 = coll.find.skip(10).limit(20)

      var cntr7 = 0
      while(cur7.hasNext) {
        cntr7 += 1
        cur7.next.get("i") must_== 10+cntr7
      }
      cntr7 must_== 20

      // sorting
      val cur8 = coll.find.sort(new BasicDBObject("i", -1)) // descending

      var cntr8 = 100
      while(cur8.hasNext) {
        cur8.next.get("i") must_== cntr8
        cntr8 -= 1
      }

      // remove some docs by a query
      coll.remove(new BasicDBObject("i", new BasicDBObject("$gt", 50)))

      coll.find.count must_== 50

      if (!debug) {
        // delete the rest of the rows
        coll.remove(new BasicDBObject("i", new BasicDBObject("$lte", 50)))
        coll.find.count must_== 0
        coll.drop
      }
    })
  }

  "Mongo useSession example" in {

    checkMongoIsRunning

    // use a Mongo instance directly with a session
    MongoDB.useSession ( db => {
      val coll = db.getCollection("testCollection")

      // create a unique index on name
      coll.ensureIndex(new BasicDBObject("name", 1), new BasicDBObject("unique", true))

      // build the DBObjects
      val doc = new BasicDBObject
      val doc2 = new BasicDBObject
      val doc3 = new BasicDBObject

      doc.put("name", "MongoSession")
      doc.put("type", "db")
      doc.put("count", 1)

      doc2.put("name", "MongoSession")
      doc2.put("type", "db")
      doc2.put("count", 1)

      doc3.put("name", "MongoDB")
      doc3.put("type", "db")
      doc3.put("count", 1)

      // save the docs to the db
      coll.save(doc)
      db.getLastError.get("err") must beNull
      coll.save(doc2) // this should return an error
      db.getLastError.get("err").toString must startWith("E11000 duplicate key error index")
      coll.save(doc3)
      db.getLastError.get("err") must beNull

      // query for the docs by type
      val qry = new BasicDBObject("type", "db")
      coll.find(qry).count must_== 2

      // modifier operations $inc, $set, $push...
      val o2 = new BasicDBObject
      o2.put("$inc", new BasicDBObject("count", 1)) // increment count by 1
      //o2.put("$set", new BasicDBObject("type", "docdb")) // set type
      coll.update(qry, o2, false, false)
      db.getLastError.get("updatedExisting") must_== true
      /* The update method only updates one document. see:
      http://jira.mongodb.org/browse/SERVER-268
      */
      db.getLastError.get("n") must_== 1

      // this update query won't find any docs to update
      coll.update(new BasicDBObject("name", "None"), o2, false, false)
      db.getLastError.get("updatedExisting") must_== false
      db.getLastError.get("n") must_== 0

      // regex query example
      val key = "name"
      val regex = "^Mongo"
      val cur = coll.find(
          BasicDBObjectBuilder.start.add(key, Pattern.compile(regex)).get)
      cur.count must_== 2

      // use regex and another dbobject
      val cur2 = coll.find(
          BasicDBObjectBuilder.start.add(key, Pattern.compile(regex)).add("count", 1).get)
      cur2.count must_== 1

      if (!debug) {
        // delete them
        coll.remove(new BasicDBObject("type", "db"))
        db.getLastError.get("n") must_== 2
        coll.find.count must_== 0
        coll.drop
      }
    })
  }

  "UUID Example" in {

    checkMongoIsRunning

    MongoDB.useCollection("examples.uuid") { coll =>
      val uuid = UUID.randomUUID
      val dbo = new BasicDBObject("_id", uuid).append("name", "dbo")
      coll.save(dbo)

      val qry = new BasicDBObject("_id", uuid)
      val dbo2 = coll.findOne(qry)

      dbo2.get("_id") must_== dbo.get("_id")
      dbo2.get("name") must_== dbo.get("name")
    }
  }
}

}
}
