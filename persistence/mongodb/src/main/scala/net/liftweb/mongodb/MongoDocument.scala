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

import net.liftweb.common.Box
import net.liftweb.json._
import net.liftweb.util.{ConnectionIdentifier, DefaultConnectionIdentifier}
import net.liftweb.util.Helpers.tryo

import scala.collection.JavaConverters._

import java.util.UUID

import org.bson.{BsonDocument, Document, UuidRepresentation}
import org.bson.codecs.{PatternCodec, UuidCodecProvider}
import org.bson.codecs.configuration.{CodecRegistries, CodecRegistry}
import org.bson.conversions.Bson
import org.bson.types.ObjectId

import com.mongodb._
import com.mongodb.client.{MongoCollection, MongoDatabase}
import com.mongodb.client.model.{DeleteOptions, IndexOptions, InsertOneOptions, ReplaceOptions, UpdateOptions}
import com.mongodb.client.model.Filters.{eq => eqs}
import com.mongodb.client.result.{DeleteResult, UpdateResult}

/**
 * extend case class with this trait
 */
trait MongoDocument[BaseDocument] extends JsonObject[BaseDocument] {
  self: BaseDocument =>

  def _id: Any

  def meta: MongoDocumentMeta[BaseDocument]

  def delete: Box[DeleteResult] = {
    meta.deleteOne("_id", _id)
  }

  def save: UpdateResult = meta.save(this)

  def getRef: Option[MongoRef] = _id match {
    case oid: ObjectId => Some(MongoRef(meta.collectionName, oid))
    case _ => None
  }
}

/**
 * extend case class companion objects with this trait
 */
trait MongoDocumentMeta[BaseDocument] extends JsonObjectMeta[BaseDocument] with MongoMeta[BaseDocument, BsonDocument] {

  private val bsonDocumentClass = classOf[BsonDocument]

  def codecRegistry: CodecRegistry = CodecRegistries.fromRegistries(
    MongoClientSettings.getDefaultCodecRegistry(),
    CodecRegistries.fromProviders(new UuidCodecProvider(UuidRepresentation.JAVA_LEGACY)),
    CodecRegistries.fromCodecs(new PatternCodec())
  )

  /**
   * Override this to specify a ConnectionIdentifier.
   */
  def connectionIdentifier: ConnectionIdentifier = DefaultConnectionIdentifier

  /**
   * Use the collection associated with this Meta.
   */
  def useCollection[T](f: MongoCollection[BsonDocument] => T): T =
    MongoDB.useMongoCollection(connectionIdentifier, collectionName, bsonDocumentClass) { mc =>
      f(mc.withCodecRegistry(codecRegistry).withWriteConcern(writeConcern))
    }

  def useCollection[T](db: MongoDatabase)(f: MongoCollection[BsonDocument] => T): T = {
    val mc = db.getCollection(collectionName, bsonDocumentClass)

    f(mc.withCodecRegistry(codecRegistry).withWriteConcern(writeConcern))
  }

  @deprecated("Use useCollection instead", "3.4.2")
  def useColl[T](f: DBCollection => T): T =
    MongoDB.useCollection(connectionIdentifier, collectionName)(f)

  /**
   * Use the db associated with this Meta.
   */
  def useDatabase[T](f: MongoDatabase => T): T =
    MongoDB.useDatabase(connectionIdentifier) { md =>
      f(md.withCodecRegistry(codecRegistry).withWriteConcern(writeConcern))
    }

  @deprecated("Use useDatabase instead", "3.4.2")
  def useDb[T](f: DB => T): T = MongoDB.use(connectionIdentifier)(f)

  def create(dbo: Bson): BaseDocument = {
    val jv = BsonParser.serialize(dbo)
    create(jv.asInstanceOf[JObject])
  }

  /**
   * Find a single row by a qry, using a Bson.
   */
  def find(qry: Bson): Option[BaseDocument] = {
    useCollection { coll =>
      coll.find(qry).limit(1).first match {
        case null => None
        case dbo => {
          Some(create(dbo))
        }
      }
    }
  }

  /**
   * Find a single document by _id using a String.
   */
  def find(s: String): Option[BaseDocument] =
    if (ObjectId.isValid(s))
      find(eqs("_id", new ObjectId(s)))
    else
      find(eqs("_id", s))

  /**
   * Find a single document by _id using an ObjectId.
   */
  def find(oid: ObjectId): Option[BaseDocument] = find(eqs("_id", oid))

  /**
   * Find a single document by _id using a UUID.
   */
  def find(uuid: UUID): Option[BaseDocument] = find(eqs("_id", uuid))

  /**
   * Find a single document by a qry using String, Any inputs
   */
  def find(k: String, v: Any): Option[BaseDocument] = find(eqs(k, v))

  /**
   * Find a single document by a qry using a json query
   */
  def find(json: JObject): Option[BaseDocument] = find(BsonParser.parse(json))

  /**
   * Find all documents in this collection
   */
  def findAll: List[BaseDocument] = {
    useCollection { coll =>
      /** Mongo Cursors are both Iterable and Iterator,
       * so we need to reduce ambiguity for implicits
       */
      coll.find.iterator.asScala.map(create).toList
    }
  }

  /**
   * Find all documents using a Bson query.
   */
  def findAll(qry: Bson, sort: Option[Bson], opts: FindOption*): List[BaseDocument] = {
    val findOpts = opts.toList

    useCollection { coll =>
      val cur = coll.find(qry).limit(
        findOpts.find(_.isInstanceOf[Limit]).map(x => x.value).getOrElse(0)
      ).skip(
        findOpts.find(_.isInstanceOf[Skip]).map(x => x.value).getOrElse(0)
      )
      sort.foreach(s => cur.sort(s))
      /** Mongo Cursors are both Iterable and Iterator,
       * so we need to reduce ambiguity for implicits
       */
      cur.iterator.asScala.map(create).toList
    }
  }

  /**
   * Find all documents using a Bson query.
   */
  def findAll(qry: Bson, opts: FindOption*): List[BaseDocument] =
    findAll(qry, None, opts :_*)

  /**
   * Find all documents using a Bson query with sort
   */
  def findAll(qry: Bson, sort: Bson, opts: FindOption*): List[BaseDocument] =
    findAll(qry, Some(sort), opts :_*)

  /**
   * Find all documents using a JObject query
   */
  def findAll(qry: JObject, opts: FindOption*): List[BaseDocument] =
    findAll(BsonParser.parse(qry), None, opts :_*)

  /**
   * Find all documents using a JObject query with sort
   */
  def findAll(qry: JObject, sort: JObject, opts: FindOption*): List[BaseDocument] =
    findAll(BsonParser.parse(qry), Some(BsonParser.parse(sort)), opts :_*)

  /**
   * Find all documents using a k, v query
   */
  def findAll(k: String, o: Any, opts: FindOption*): List[BaseDocument] =
    findAll(eqs(k, o), None, opts :_*)

  /**
   * Find all documents using a k, v query with JObject sort
   */
  def findAll(k: String, o: Any, sort: JObject, opts: FindOption*): List[BaseDocument] =
    findAll(eqs(k, o), Some(BsonParser.parse(sort)), opts :_*)

  def insertOne(inst: BaseDocument, opts: InsertOneOptions = new InsertOneOptions): Box[BaseDocument] = tryo {
    useCollection { coll =>
      val bson = BsonParser.parse(toJObject(inst))
      coll.insertOne(bson, opts)
      inst
    }
  }

  def replaceOne(inst: BaseDocument, opts: ReplaceOptions = new ReplaceOptions): Box[UpdateResult] = tryo {
    useCollection { coll =>
      val bson = BsonParser.parse(toJObject(inst))
      val id = bson.get("_id")
      coll.replaceOne(eqs("_id", id), bson, opts)
    }
  }

  def replaceOne(qry: Bson, inst: BaseDocument, opts: ReplaceOptions): Box[UpdateResult] = tryo {
    useCollection { coll =>
      val bson = BsonParser.parse(toJObject(inst))
      coll.replaceOne(qry, bson, opts)
    }
  }

  def replaceOne(qry: Bson, inst: BaseDocument): Box[UpdateResult] =
    replaceOne(qry, inst, new ReplaceOptions)

  def replaceOne(qry: JObject, inst: BaseDocument, opts: ReplaceOptions): Box[UpdateResult] = tryo {
    useCollection { coll =>
      val bson = BsonParser.parse(toJObject(inst))
      coll.replaceOne(BsonParser.parse(qry), bson, opts)
    }
  }

  def replaceOne(qry: JObject, inst: BaseDocument): Box[UpdateResult] =
    replaceOne(qry, inst, new ReplaceOptions)

  /**
   * Save a document to the db
   */
  def save(inst: BaseDocument): UpdateResult = {
    val opts = new ReplaceOptions().upsert(true)
    useCollection { coll =>
      val bson = BsonParser.parse(toJObject(inst))
      val id = bson.get("_id")
      coll.replaceOne(eqs("_id", id), bson, opts)
    }
  }

  @deprecated("Use save instead", "3.4.2")
  def save(in: BaseDocument, db: DB) {
    db.getCollection(collectionName).save(JObjectParser.parse(toJObject(in)))
  }

  @deprecated("Use updateOne, updateMany, or replaceOne instead", "3.4.2")
  def update(qry: JObject, newbd: BaseDocument, db: DB, opts: UpdateOption*) {
    update(qry, toJObject(newbd), db, opts :_*)
  }

  @deprecated("Use updateOne, updateMany, or replaceOne instead", "3.4.2")
  def update(qry: JObject, newbd: BaseDocument, opts: UpdateOption*) {
    MongoDB.use(connectionIdentifier) ( db => {
      update(qry, newbd, db, opts :_*)
    })
  }

}

