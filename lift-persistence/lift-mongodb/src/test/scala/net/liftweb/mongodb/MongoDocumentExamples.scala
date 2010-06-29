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

import java.util.{Calendar, Date, UUID}
import java.util.regex.Pattern

import org.specs._
import org.specs.runner.JUnit4
import net.liftweb.json.DefaultFormats
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonParser._
import net.liftweb.json.JsonDSL._

import com.mongodb.{BasicDBList, BasicDBObject, DBObject, MongoException}
import org.bson.types.ObjectId

class MongoDocumentExamplesTest extends JUnit4(MongoDocumentExamples)

package mongotestdocs {
  /*
  * MongoIdentifiers
  */
  object TstDBa extends MongoIdentifier {
    val jndiName = "test_a"
  }
  object TstDBb extends MongoIdentifier {
    val jndiName = "test_b"
  }

  /*
  * _id as a ObjectId
  */
  case class SimplePerson(_id: ObjectId, name: String, age: Int) extends MongoDocument[SimplePerson] {
    def meta = SimplePerson
  }
  object SimplePerson extends MongoDocumentMeta[SimplePerson] {
    override val collectionName = "simplepersons"
    override def mongoIdentifier = DefaultMongoIdentifier
    override def formats = super.formats + new ObjectIdSerializer
    // index name
    ensureIndex(("name" -> 1))
  }

  case class Address(street: String, city: String)
  case class Child(name: String, age: Int, birthdate: Option[Date])

  /*
  * _id as UUID
  */
  case class Person(_id: UUID, name: String, age: Int, address: Address, children: List[Child], dob: Date)
    extends MongoDocument[Person] {

    def meta = Person
  }

  object Person extends MongoDocumentMeta[Person] {
    override def mongoIdentifier = TstDBa
    override def collectionName = "mypersons"
    override def formats = super.formats + new UUIDSerializer
  }

  /*
  * _id as ObjectId.toString
  */
  case class TCInfo(x: Int, y: Int, uuid: UUID)
  case class TstCollection(_id: String, name: String, dbtype: String, count: Int, info: TCInfo)
    extends MongoDocument[TstCollection] {

    def meta = TstCollection
  }

  object TstCollection extends MongoDocumentMeta[TstCollection] {
    override def formats = super.formats + new UUIDSerializer
    // create a unique index on name
    ensureIndex(("name" -> 1), true)
  }

  case class IDoc(_id: ObjectId, i: Int) extends MongoDocument[IDoc] {

    def meta = IDoc
  }

  object IDoc extends MongoDocumentMeta[IDoc] {
    override def formats = super.formats + new ObjectIdSerializer
    // create an index on "i", descending with custom name
    ensureIndex(("i" -> -1), ("name" -> "i_ix1"))
  }

  case class SessCollection(_id: ObjectId, name: String, dbtype: String, count: Int)
    extends MongoDocument[SessCollection] {

    def meta = SessCollection
  }

  object SessCollection extends MongoDocumentMeta[SessCollection] {
    override def formats = super.formats + new ObjectIdSerializer
    // create a unique index on name
    ensureIndex(("name" -> 1), true)
  }

  /*
  * mongo-java-driver is not compatible with numbers that have an e in them
  */
  case class Primitive(
    _id: ObjectId,
    intfield: Int,
    longfield: Long,
    doublefield: Double,
    floatfield: Float,
    bigintfield: BigInt,
    bytefield: Byte,
    booleanfield: Boolean,
    shortfield: Short,
    datefield: Date
  ) extends MongoDocument[Primitive] {

    def meta = Primitive
  }

  object Primitive extends MongoDocumentMeta[Primitive] {
    override def formats = super.formats + new ObjectIdSerializer
  }

  case class MainJDoc(_id: ObjectId, name: String, refdoc: Option[MongoRef], refId: Option[ObjectId]) extends MongoDocument[MainJDoc] {

    def meta = MainJDoc
  }

  object MainJDoc extends MongoDocumentMeta[MainJDoc] {
    override def formats = super.formats + new ObjectIdSerializer
  }

  case class RefJDoc(_id: ObjectId) extends MongoDocument[RefJDoc] {
    def meta = RefJDoc
  }

  object RefJDoc extends MongoDocumentMeta[RefJDoc] {
    override def formats = super.formats + new ObjectIdSerializer
  }

  case class PatternDoc(_id: ObjectId, regx: Pattern) extends MongoDocument[PatternDoc] {
    def meta = PatternDoc
  }
  object PatternDoc extends MongoDocumentMeta[PatternDoc] {
    override def formats = super.formats + new ObjectIdSerializer + new PatternSerializer
  }

  case class StringDateDoc(_id: ObjectId, dt: Date) extends MongoDocument[StringDateDoc] {
    def meta = StringDateDoc
  }
  object StringDateDoc extends MongoDocumentMeta[StringDateDoc] {
    override def formats = new DefaultFormats {
      override def dateFormatter = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm'Z'")
    } + new DateSerializer + new ObjectIdSerializer
  }

}

object MongoDocumentExamples extends Specification with MongoTestKit {
  import mongotestdocs._

  override def dbName = "lift_mongodocumentexamples"

  override def dbs = (TstDBa, defaultHost, "lift_mongodocumentexamples_a") :: super.dbs

  "Simple Person example" in {

    checkMongoIsRunning

    // create a new SimplePerson
    val pid = ObjectId.get
    val p = SimplePerson(pid, "Tim", 38)

    // save it
    p.save

    // retrieve it
    def pFromDb = SimplePerson.find(pid)

    pFromDb.isDefined must_== true
    p mustEqual pFromDb.get

    // retrieve it using a Json query
    def pFromDbViaJson = SimplePerson.find(("_id" -> ("$oid" -> p._id.toString)))

    pFromDbViaJson.isDefined must_== true

    p mustEqual pFromDbViaJson.get

    // modify and save the person
    // with scala 2.8 you can use the copy function to do this
    // val p3 = p.copy(name = "Tim3")
    val p2 = SimplePerson(p._id, "Timm", 27)
    p2.save
    pFromDb.isDefined must_== true
    p2 must_== pFromDb.get
    p2.name must_== pFromDb.get.name

    // find all documents
    val all = SimplePerson.findAll

    all.isEmpty must_== false

    if (!debug) {
      all.size must_== 1
      all.first must_== p2

      // delete it
      p2.delete

      pFromDb.isEmpty must_== true
      pFromDbViaJson.isEmpty must_== true

      SimplePerson.drop
    }
  }

  "Multiple Simple Person example" in {

    checkMongoIsRunning

    // create new SimplePersons
    val p = SimplePerson(ObjectId.get, "Jill", 27)
    val p2 = SimplePerson(ObjectId.get, "Bob", 25)
    val p3 = SimplePerson(ObjectId.get, "Bob", 29)

    // save them
    p.save
    p2.save
    p3.save

    // retrieve them
    def pFromDb = SimplePerson.find(p._id)
    def p2FromDb = SimplePerson.find(p2._id)
    def p3FromDb = SimplePerson.find(("_id" -> ("$oid" -> p3._id.toString)))

    pFromDb.isDefined must_== true
    p2FromDb.isDefined must_== true
    p3FromDb.isDefined must_== true

    p mustEqual pFromDb.get
    p2 mustEqual p2FromDb.get
    p3 mustEqual p3FromDb.get

    // find all persons named 'Bob'
    val allBobs = SimplePerson.findAll(("name" -> "Bob"))
    val allBobs2 = SimplePerson.findAll("name", "Bob")

    allBobs.isEmpty must_== false
    allBobs2.isEmpty must_== false

    if (!debug) {
      allBobs.size must_== 2
      allBobs2.size must_== 2

      // delete them
      p.delete
      p2.delete
      p3.delete

      pFromDb.isEmpty must_== true
      p2FromDb.isEmpty must_== true
      p3FromDb.isEmpty must_== true

      SimplePerson.drop
    }
  }

  "Person example" in {

    checkMongoIsRunning

    def date(s: String) = Person.formats.dateFormat.parse(s).get

    val cal = Calendar.getInstance
    cal.set(2009, 10, 2)

    // create a new Person UUID.randomUUID.toString
    val p = Person(UUID.randomUUID, "joe", 27, Address("Bulevard", "Helsinki"), List(Child("Mary", 5, Some(cal.getTime)), Child("Mazy", 3, None)), date("2004-09-04T18:06:22.000Z"))

    // save it
    p.save

    // retrieve it
    def pFromDb = Person.find(p._id)

    // compare to original
    val p2 = pFromDb
    p2.isDefined must_== true
    p must_== p2.get

    Person.count must_== 1

    if (!debug) {
      // delete it
      p.delete

      pFromDb.isEmpty must_== true

      Person.drop
    }
  }

  "Mongo tutorial example" in {

    checkMongoIsRunning

    // build a TstCollection
    val info = TCInfo(203, 102, UUID.randomUUID)
    val tc = TstCollection(ObjectId.get.toString, "MongoDB", "database", 1, info)
    val tc2 = TstCollection(ObjectId.get.toString, "OtherDB", "database", 1, info)

    // save to db
    tc.save
    tc2.save

    // Query
    def tcFromDb = TstCollection.find(tc._id)
    def tc2FromDb = TstCollection.find(tc2._id)

    tcFromDb.isDefined must_== true
    tcFromDb.get must_== tc
    tc2FromDb.isDefined must_== true
    tc2FromDb.get must_== tc2

    // update
    val tc3 = TstCollection(tc._id, "MongoDB", "document", 2, info) // the new object to update with, replaces the entire document, except possibly _id
    val q = ("name" -> "MongoDB") // the query to select the document(s) to update
    TstCollection.update(q, tc3)
    tcFromDb.isDefined must_== true
    tcFromDb.get must_== tc3

    // Upsert - this should add a new row
    val tc4 = TstCollection(ObjectId.get.toString, "nothing", "document", 1, info)
    TstCollection.update(("name" -> "nothing"), tc4, Upsert)
    TstCollection.findAll.length must_== 3

    // modifier operations $inc, $set, $push...
    val o2 = (("$inc" -> ("count" -> 1)) ~ ("$set" -> ("dbtype" -> "docdb")))
    TstCollection.update(q, o2)
    tcFromDb.isDefined must_== true
    tcFromDb.get must_== TstCollection(tc._id, tc.name, "docdb", 3, info)

    // this one shouldn't update anything
    val o3 = (("$inc" -> ("count" -> 1)) ~ ("$set" -> ("dbtype" -> "docdb")))
    // when using $ modifiers, apply has to be false
    TstCollection.update(("name" -> "nothing"), o3)
    TstCollection.findAll.length must_== 3

    if (!debug) {
      // delete them
      tc.delete
      tc2.delete
      tc4.delete

      TstCollection.findAll.size must_== 0
    }

    // insert multiple documents
    for (i <- List.range(1, 101)) {
      IDoc(ObjectId.get, i).save
    }

    // count the docs
    IDoc.count must_== 100

    // get the count using a query
    IDoc.count(("i" -> ("$gt" -> 50))) must_== 50

    // get a List of all documents
    val all = IDoc.findAll
    all.length must_== 100

    // get a single document with a query ( i = 71 )
    val doc = IDoc.find(("i" -> 71))

    doc.isDefined must_== true
    doc.get.i must_== 71

    // get a set of documents with a query
    // e.g. find all where i > 50
    val list1 = IDoc.findAll(("i" -> ("$gt" -> 50)))

    list1.length must_== 50

    // range - 20 < i <= 30
    val list2 = IDoc.findAll(("i" -> ("$gt" -> 20) ~ ("$lte" -> 30)))

    list2.length must_== 10

    // limiting result set
    val list3 = IDoc.findAll(("i" -> ("$gt" -> 50)), Limit(3))

    list3.length must_== 3

    // skip
    val list4 = IDoc.findAll(("i" -> ("$gt" -> 50)), ("i" -> 1), Skip(10))
    list4.size must_== 40
    var cntr4 = 0
    for (idoc <- list4) {
      cntr4 += 1
      idoc.i must_== 60+cntr4
    }

    // skip and limit (get first 10, skipping the first 5, where i > 50)
    val list5 = IDoc.findAll(("i" -> ("$gt" -> 50)), ("i" -> 1), Limit(10), Skip(5))
    var cntr5 = 0
    for (idoc <- list5) {
      cntr5 += 1
      idoc.i must_== 55+cntr5
    }
    list5.length must_== 10

    // sorting (it's also easy to sort the List after it's returned)
    val list6 = IDoc.findAll(("i" -> ("$gt" -> 0)), ("i" -> -1)) // descending
    var cntr6 = 100
    for (idoc <- list6) {
      idoc.i must_== cntr6
      cntr6 -= 1
    }
    list6.length must_== 100

    // remove some docs by a query
    IDoc.delete(("i" -> ("$gt" -> 50)))
    IDoc.findAll.length must_== 50

    IDoc.drop
  }

  "Mongo useSession example" in {

    checkMongoIsRunning

    val tc = SessCollection(ObjectId.get, "MongoSession", "db", 1)
    val tc2 = SessCollection(ObjectId.get, "MongoSession", "db", 1)
    val tc3 = SessCollection(ObjectId.get, "MongoDB", "db", 1)

    // use a Mongo instance directly with a session
    MongoDB.useSession(DefaultMongoIdentifier) ( db => {

      // save to db
      SessCollection.save(tc, db)
      db.getLastError.get("err") must beNull
      SessCollection.save(tc2, db) // this should return an error
      db.getLastError.get("err").toString must startWith("E11000 duplicate key error index")
      SessCollection.save(tc3, db)
      db.getLastError.get("err") must beNull

      // query for the docs by type
      val qry = ("dbtype" -> "db")
      SessCollection.findAll(qry).size must_== 2

      // modifier operations $inc, $set, $push...
      val o2 = ("$inc" -> ("count" -> 1)) // increment count by 1
      //("$set" -> ("dbtype" -> "docdb")) // set dbtype
      SessCollection.update(qry, o2, db)
      db.getLastError.get("updatedExisting") must_== true
      /* The update method only updates one document. */
      db.getLastError.get("n") must_== 1

      /* Multiple documents now supported */
      SessCollection.update(qry, o2, db, Multi)
      db.getLastError.get("updatedExisting") must_== true
      db.getLastError.get("n") must_== 2

      // regex query example
      val lst = SessCollection.findAll(new BasicDBObject("name", Pattern.compile("^Mongo")))
      lst.size must_== 2

      // jobject query now also works
      val lstjobj = SessCollection.findAll(("name" -> (("$regex" -> "^Mon") ~ ("$flags" -> 0))))
      lstjobj.size must_== 2

      // use regex and another clause
      val lst2 = SessCollection.findAll(new BasicDBObject("name", Pattern.compile("^Mon")).append("count", 2))
      lst2.size must_== 1

      val lstjobj2 = SessCollection.findAll(("name" -> (("$regex" -> "^Mongo") ~ ("$flags" -> 0))) ~ ("count" -> 3))
      lstjobj2.size must_== 1

      if (!debug) {
        // delete them
        SessCollection.delete(qry)
        SessCollection.findAll.size must_== 0

        SessCollection.drop
      }

    })
  }

  "Primitives example" in {

    checkMongoIsRunning

    def date(s: String) = Primitive.formats.dateFormat.parse(s).get

    val p = Primitive(ObjectId.get, 2147483647, 2147483648L, 1797693, 3.4028235F, 1000, 0, true, 512, date("2004-09-04T18:06:22.000Z"))

    // save it
    p.save

    // retrieve it
    def pFromDb = Primitive.find(p._id)

    pFromDb.isDefined must_== true

    p mustEqual pFromDb.get

    if (!debug) {
      // delete it
      p.delete

      pFromDb.isEmpty must_== true
      Primitive.drop
    }
  }

  "Ref example" in {

    checkMongoIsRunning

    val ref1 = RefJDoc(ObjectId.get)
    val ref2 = RefJDoc(ObjectId.get)

    ref1.save
    ref2.save

    val md1 = MainJDoc(ObjectId.get, "md1", ref1.getRef, Some(ref1._id))
    val md2 = MainJDoc(ObjectId.get, "md2", ref1.getRef, None)
    val md3 = MainJDoc(ObjectId.get, "md3", ref2.getRef, None)
    val md4 = MainJDoc(ObjectId.get, "md4", ref2.getRef, None)

    md1.save
    md2.save
    md3.save
    md4.save

    MainJDoc.count must_== 4
    RefJDoc.count must_== 2

    // query for a single doc with a JObject query
    val md1a = MainJDoc.find(("name") -> "md1")
    md1a.isDefined must_== true
    md1a.foreach(o => o._id must_== md1._id)

    // query for a single doc with a k, v query
    val md1b = MainJDoc.find(md1._id)
    md1b.isDefined must_== true
    md1b.foreach(o => o._id must_== md1._id)

    // find all documents
    MainJDoc.findAll.size must_== 4
    RefJDoc.findAll.size must_== 2

    // find all documents with JObject query
    val mdq1 = MainJDoc.findAll(("name" -> "md1"))
    mdq1.size must_== 1

    // find all documents with $in query, sorted
    val qry = ("name" -> ("$in" -> List("md1", "md2")))
    val mdq2 = MainJDoc.findAll(qry, ("name" -> -1))
    mdq2.size must_== 2
    mdq2.first._id must_== md2._id

    // Find all documents using a k, v query
    val mdq3 = MainJDoc.findAll("_id", md1._id)
    mdq3.size must_== 1

    MainJDoc.drop
    RefJDoc.drop
  }

  "Pattern example" in {

    checkMongoIsRunning

    val pdoc1 = PatternDoc(ObjectId.get, Pattern.compile("^Mo", Pattern.CASE_INSENSITIVE))
    pdoc1.save

    PatternDoc.find(pdoc1._id).map {
      pdoc =>
        pdoc._id must_== pdoc1._id
        pdoc.regx.pattern must_== pdoc1.regx.pattern
        pdoc.regx.flags must_== pdoc1.regx.flags
    }
  }

  "Issue 586 Date test" in {

    checkMongoIsRunning

    def date(s: String): Date = StringDateDoc.formats.dateFormat.parse(s).get

    val newId = ObjectId.get
    val dtStr = "2004-09-04T18:06Z"
    val newDt = date(dtStr)

    // create a document manually with a String for the Date field
    MongoDB.useCollection("stringdatedocs") { coll =>
      coll.save(new BasicDBObject("_id", newId).append("dt", dtStr))
    }

    val fromDb = StringDateDoc.find(newId)
    fromDb.isDefined must_== true
    fromDb.foreach {
      sdd =>
        sdd._id must_== newId
        sdd.dt must_== newDt
        sdd.save

        StringDateDoc.find(newId).foreach {
          sdd2 =>
            sdd2.dt must_== sdd.dt
        }
    }

  }
}

}
}
