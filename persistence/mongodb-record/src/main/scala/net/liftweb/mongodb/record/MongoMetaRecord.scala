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
package record

import scala.reflect.ClassTag
import java.util.UUID

import net.liftweb.common._
import net.liftweb.json._
import net.liftweb.mongodb.record.codecs.CollectibleRecordCodec
import net.liftweb.mongodb.record.field._
import net.liftweb.record.MandatoryTypedField
import net.liftweb.util.Helpers.tryo

import org.bson.Document
import org.bson.codecs.Codec
import org.bson.codecs.configuration.CodecRegistries
import org.bson.conversions.Bson
import org.bson.types.ObjectId

import com.mongodb._
import com.mongodb.async.SingleResultCallback
import com.mongodb.client.{FindIterable, MongoCollection, MongoDatabase}
import com.mongodb.client.model.Filters.{eq => eqs, in}
import com.mongodb.client.model.Updates._
import com.mongodb.client.result.{DeleteResult, UpdateResult}

import org.mongodb.scala.{Completed, Observer}
import org.mongodb.scala.model.{DeleteOptions, IndexOptions, InsertOneOptions, ReplaceOptions, UpdateOptions}

import scala.collection.JavaConverters._
import scala.concurrent.{Future, Promise}

trait MongoMetaRecord[BaseRecord <: MongoRecord[BaseRecord]]
  extends BsonMetaRecord[BaseRecord]
  with MongoMeta[BaseRecord, BaseRecord]
{
  self: BaseRecord =>

  lazy val recordClass: Class[BaseRecord] = createRecord.getClass.asInstanceOf[Class[BaseRecord]]
  lazy val recordClassTag: ClassTag[BaseRecord] = ClassTag(recordClass)

  override def codec =
    CollectibleRecordCodec(this, introspectedCodecRegistry, bsonTypeClassMap, bsonTransformer)

  /**
   * Utility method for determining the value of _id.
   * This is needed for backwards compatibility with MongoId. This is
   * due to the fact that MongoRecord.id is of type Any. That will
   * be changed to type MandatoryTypedField in a future version. When
   * that happens this will no longer be necessary.
   */
  private def idValue(inst: BaseRecord): Any = inst.id match {
    case f: MandatoryTypedField[_] => f.value
    case x => x
  }

  @deprecated("Use useCollection instead", "3.4.2")
  def useColl[T](f: DBCollection => T): T =
    MongoDB.useCollection(connectionIdentifier, collectionName)(f)

  /**
   * A CodecRegistry that contains the codec for this Record. Used by `useCollection` and `useDatabase`.
   */
  private lazy val recordCodecRegistry = CodecRegistries.fromRegistries(
    CodecRegistries.fromCodecs(codec),
    codecRegistry
  )

  /**
   * Use the collection associated with this Meta.
   */
  def useCollection[T](f: MongoCollection[BaseRecord] => T): T = {
    MongoDB.useMongoCollection(connectionIdentifier, collectionName, recordClass) { mc =>
      f(mc.withCodecRegistry(recordCodecRegistry).withWriteConcern(writeConcern))
    }
  }

  /**
   * Use the db associated with this Meta.
   */
  def useDatabase[T](f: MongoDatabase => T): T = {
    MongoDB.useDatabase(connectionIdentifier) { md =>
      f(md.withCodecRegistry(recordCodecRegistry).withWriteConcern(writeConcern))
    }
  }

  @deprecated("Use useDatabase instead", "3.4.2")
  def useDb[T](f: DB => T): T = MongoDB.use(connectionIdentifier)(f)

  @deprecated("Use useCollectionAsync instead", "3.4.2")
  def useCollAsync[T](f: com.mongodb.async.client.MongoCollection[Document] => T): T = {
    MongoAsync.useCollection[T](connectionIdentifier, collectionName)(f)
  }

  def useCollectionAsync[T](f: org.mongodb.scala.MongoCollection[BaseRecord] => T): T = {
    MongoScalaAsync.useCollection(connectionIdentifier, collectionName, recordClassTag) { mc =>
      f(mc.withCodecRegistry(recordCodecRegistry).withWriteConcern(writeConcern))
    }
  }

  /**
   * Delete the instance from backing store
   */
  def delete_!(inst: BaseRecord): Boolean = {
    foreachCallback(inst, _.beforeDelete)
    deleteOne("_id", idValue(inst))
    foreachCallback(inst, _.afterDelete)
    true
  }

  def deleteOne(inst: BaseRecord, opts: DeleteOptions): Box[DeleteResult] = {
    foreachCallback(inst, _.beforeDelete)
    val result = deleteOne("_id", idValue(inst))
    foreachCallback(inst, _.afterDelete)
    result
  }

  def deleteOne(inst: BaseRecord): Box[DeleteResult] = {
    deleteOne(inst, new DeleteOptions)
  }

  def bulkDelete_!!(qry: Bson): Unit = {
    useCollection(_.deleteMany(qry))
  }

  def bulkDelete_!!(k: String, o: Any): Unit = bulkDelete_!!(eqs(k, o))

  /**
   * Find a single row by a qry, using a Bson.
   */
  def find(qry: Bson): Box[BaseRecord] = {
    useCollection { coll =>
      coll.find(qry).limit(1).first match {
        case null => Empty
        case doc => Full(doc)
      }
    }
  }

  /**
   * Find a single row by an ObjectId
   */
  def find(oid: ObjectId): Box[BaseRecord] = find(eqs("_id", oid))

  /**
   * Find a single row by a UUID
   */
  def find(uid: UUID): Box[BaseRecord] = find(eqs("_id", uid))

  /**
   * Find a single row by Any
   * This doesn't work as find because we need JObject's to be implicitly converted.
   */
  def findAny(a: Any): Box[BaseRecord] = find(eqs("_id", a))

  /**
   * Find a single row by a String id
   */
  def find(s: String): Box[BaseRecord] =
    if (ObjectId.isValid(s))
      find(eqs("_id", new ObjectId(s)))
    else
      find(eqs("_id", s))

  /**
   * Find a single row by an Int id
   */
  def find(id: Int): Box[BaseRecord] = find(eqs("_id", id))

  /**
   * Find a single row by a Long id
   */
  def find(id: Long): Box[BaseRecord] = find(eqs("_id", id))

  /**
   * Find a single document by a qry using a json value
   */
  def find(json: JObject): Box[BaseRecord] = find(BsonParser.parse(json))

  /**
   * Find a single row by a qry using String key and Any value
   */
  def find(k: String, o: Any): Box[BaseRecord] = find(eqs(k, o))

  /**
   * Find all rows in this collection.
   * Retrieves all documents and puts them in memory.
   */
  def findAll: List[BaseRecord] = useCollection { coll =>
    /** Mongo Cursors are both Iterable and Iterator,
     * so we need to reduce ambiguity for implicits
     */
    coll.find.iterator.asScala.toList
  }

  /**
   * Find all rows using a Bson query.
   */
  def findAll(qry: Bson, sort: Option[Bson], opts: FindOption*): List[BaseRecord] = {
    findAll(sort, opts:_*) { coll => coll.find(qry) }
  }

  /**
   * Find all rows and retrieve only keys fields.
   */
  def findAll(qry: Bson, keys: Bson, sort: Option[Bson], opts: FindOption*): List[BaseRecord] = {
    findAll(sort, opts:_*) { coll => coll.find(qry).projection(keys) }
  }

  protected def findAll(sort: Option[Bson], opts: FindOption*)(f: (MongoCollection[BaseRecord]) => FindIterable[BaseRecord]): List[BaseRecord] = {
    val findOpts = opts.toList

    useCollection { coll =>
      val cur = f(coll).limit(
        findOpts.find(_.isInstanceOf[Limit]).map(_.value).getOrElse(0)
      ).skip(
        findOpts.find(_.isInstanceOf[Skip]).map(_.value).getOrElse(0)
      )
      sort.foreach(s => cur.sort(s))
      // This retrieves all documents and puts them in memory.
      cur.iterator.asScala.toList
    }
  }

  /**
   * Find all rows and retrieve only keys fields.
   */
  def findAll(qry: JObject, keys: JObject, sort: Option[JObject], opts: FindOption*): List[BaseRecord] = {
    val s = sort.map(BsonParser.parse(_))
    findAll(BsonParser.parse(qry), BsonParser.parse(keys), s, opts :_*)
  }

  /**
   * Find all documents using a Bson query. These are for passing in regex queries.
   */
  def findAll(qry: Bson, opts: FindOption*): List[BaseRecord] =
    findAll(qry, None, opts :_*)

  /**
   * Find all documents using a Bson query with Bson sort
   */
  def findAll(qry: Bson, sort: Bson, opts: FindOption*): List[BaseRecord] =
    findAll(qry, Some(sort), opts :_*)

  /**
   * Find all documents using a JObject query
   */
  def findAll(qry: JObject, opts: FindOption*): List[BaseRecord] = {
    findAll(BsonParser.parse(qry), None, opts :_*)
  }

  /**
   * Find all documents using a JObject query with sort
   */
  def findAll(qry: JObject, sort: JObject, opts: FindOption*): List[BaseRecord] =
    findAll(BsonParser.parse(qry), Some(BsonParser.parse(sort)), opts :_*)

  /**
   * Find all documents using a k, v query
   */
  def findAll(k: String, o: Any, opts: FindOption*): List[BaseRecord] =
    findAll(eqs(k, o), None, opts :_*)

  /**
   * Find all documents using a k, v query with JOBject sort
   */
  def findAll(k: String, o: Any, sort: JObject, opts: FindOption*): List[BaseRecord] =
    findAll(eqs(k, o), Some(BsonParser.parse(sort)), opts :_*)

  /**
   * Find all documents with the given ids
   */
  def findAllByList[T](ids: List[T]): List[BaseRecord] = if (ids.isEmpty) Nil else {
    val list = new java.util.ArrayList[T]()
    for (id <- ids.distinct) list.add(id)
    findAll(in("_id", list))
  }

  def findAll(ids: List[ObjectId]): List[BaseRecord] = findAllByList[ObjectId](ids)

  @deprecated("Use saveOperation instead", "3.4.2")
  protected def saveOp[T](inst: BaseRecord)(f: => T): Boolean = {
    foreachCallback(inst, _.beforeSave)
    f
    foreachCallback(inst, _.afterSave)
    inst.allFields.foreach { _.resetDirty }
    true
  }

  protected def saveOperation[T](inst: BaseRecord)(f: => T): T = {
    foreachCallback(inst, _.beforeSave)
    val result = f
    foreachCallback(inst, _.afterSave)
    inst.allFields.foreach { _.resetDirty }
    result
  }

  protected def updateOp[T](inst: BaseRecord)(f: => T): T = {
    foreachCallback(inst, _.beforeUpdate)
    val res = f
    foreachCallback(inst, _.afterUpdate)
    inst.allFields.foreach { _.resetDirty }
    res
  }

  /**
   * Save the instance in the appropriate backing store.
   */
  def save(inst: BaseRecord): UpdateResult = saveOperation(inst) {
    val opts = new ReplaceOptions().upsert(true)
    useCollection { coll =>
      val id = idValue(inst)
      coll.replaceOne(eqs("_id", id), inst, opts)
    }
  }

  /**
   * Insert multiple records
   */
  def insertAll(insts: List[BaseRecord]): Unit = {
    val list = new java.util.ArrayList[BaseRecord]()
    insts.foreach { rec => list.add(rec) }

    insts.foreach(inst => foreachCallback(inst, _.beforeSave))
    useCollection(_.insertMany(list))
    insts.foreach(inst => foreachCallback(inst, _.afterSave))
  }

  @deprecated("Use insertOneAsync instead", "3.4.2")
  def insertAsync(inst: BaseRecord): Future[Boolean] = {
    useCollAsync { coll =>
      val cb = new SingleBooleanVoidCallback( () => {
        foreachCallback(inst, _.afterSave)
        inst.allFields.foreach { _.resetDirty }
      })
      foreachCallback(inst, _.beforeSave)
      coll.insertOne(inst.asDocument, cb)
      cb.future
    }
  }

  /**
   * Insert a single record asynchronously.
   */
  def insertOneAsync(inst: BaseRecord): Future[Boolean] = {
    useCollectionAsync { coll =>
      val p = Promise[Boolean]()

      foreachCallback(inst, _.beforeSave)

      coll.insertOne(inst).subscribe(new Observer[Completed]() {
        override def onNext(result: Completed): Unit = p.success(true)
        override def onError(e: Throwable): Unit = p.failure(e)

        override def onComplete(): Unit = {
          foreachCallback(inst, _.afterSave)
          inst.allFields.foreach { _.resetDirty }
        }
      })

      p.future
    }
  }

  def insertOne(inst: BaseRecord, opts: InsertOneOptions = new InsertOneOptions): Box[BaseRecord] = tryo {
    useCollection { coll =>
      coll.insertOne(inst, opts)
      inst
    }
  }

  def replaceOne(inst: BaseRecord, opts: ReplaceOptions = new ReplaceOptions): Box[UpdateResult] = tryo {
    useCollection { coll =>
      coll.replaceOne(eqs("_id", idValue(inst)), inst, opts)
    }
  }

  /**
   * Replaces document with new one with given id. if `upsert` is set to true inserts eqs
   * in similar way as save() from sync api.
   *
   */
  @deprecated("Use replaceOneScalaAsync instead.", "3.4.2")
  def replaceOneAsync(inst: BaseRecord, upsert: Boolean = true, concern: WriteConcern = MongoRules.defaultWriteConcern.vend): Future[BaseRecord] = {
    useCollAsync { coll =>
      val p = Promise[BaseRecord]
      val doc: Document = inst.asDocument
      val options = new UpdateOptions().upsert(upsert)
      foreachCallback(inst, _.beforeSave)
      val filter = new Document("_id", doc.get("_id"))
      coll.withWriteConcern(concern).replaceOne(filter, doc, options, new SingleResultCallback[UpdateResult] {
        override def onResult(result: UpdateResult, t: Throwable): Unit = {
          if (Option(t).isEmpty) {
            Option(result.getUpsertedId).filter(_.isObjectId).foreach { upsertedId =>
              inst.fieldByName("_id").foreach(fld => fld.setFromAny(upsertedId.asObjectId().getValue))
            }
            foreachCallback(inst, _.afterSave)
            inst.allFields.foreach { _.resetDirty }
            p.success(inst)
          } else {
            p.failure(t)
          }
        }
      })
      p.future
    }
  }

  /**
   * Replaces Record with new one with given id. If `upsert` is set to true inserts
   * in similar way as save() from sync api. Uses the WriteConcern set on this Record.
   */
  def replaceOneScalaAsync(inst: BaseRecord, options: ReplaceOptions): Future[BaseRecord] = {
    useCollectionAsync { coll =>
      val p = Promise[BaseRecord]
      foreachCallback(inst, _.beforeSave)

      coll.replaceOne(eqs("_id", idValue(inst)), inst, options).subscribe(new Observer[UpdateResult]() {
        override def onNext(result: UpdateResult): Unit = {
          Option(result.getUpsertedId).filter(_.isObjectId).foreach { upsertedId =>
            inst.fieldByName("_id").foreach(fld => fld.setFromAny(upsertedId.asObjectId().getValue))
          }
        }

        override def onError(e: Throwable): Unit = p.failure(e)

        override def onComplete(): Unit = {
          foreachCallback(inst, _.afterSave)
          inst.allFields.foreach { _.resetDirty }
          p.success(inst)
        }
      })

      p.future
    }
  }

  /**
   * Replaces Record with new one with given id. If `upsert` is set to true inserts
   * in similar way as save() from sync api. Uses the WriteConcern set on this Record.
   */
  def replaceOneScalaAsync(inst: BaseRecord, upsert: Boolean = true): Future[BaseRecord] = {
    val options = ReplaceOptions().upsert(upsert)
    replaceOneScalaAsync(inst, options)
  }


  /**
   * Save the instance in the appropriate backing store
   */
  @deprecated("Set WriteConcern in MongoClientOptions or on this MongoMetaRecord", "3.4.2")
  def save(inst: BaseRecord, concern: WriteConcern): Boolean = saveOp(inst) {
    useColl { coll =>
      coll.save(inst.asDBObject, concern)
    }
  }

  /**
   * Save a document to the db using the given Mongo instance
   */
  @deprecated("Set WriteConcern in MongoClientOptions or on this MongoMetaRecord", "3.4.2")
  def save(inst: BaseRecord, db: DB, concern: WriteConcern): Boolean = saveOp(inst) {
    db.getCollection(collectionName).save(inst.asDBObject, concern)
  }

  /**
   * Update records with a JObject query using the given Mongo instance
   */
  @deprecated("Use updateOne, updateMany, or replaceOne instead", "3.4.2")
  def update(qry: JObject, newbr: BaseRecord, db: DB, opts: UpdateOption*): Unit = {
    update(JObjectParser.parse(qry), newbr.asDBObject, db, opts :_*)
  }

  /**
   * Update records with a JObject query
   */
  @deprecated("Use updateOne, updateMany, or replaceOne instead", "3.4.2")
  def update(qry: JObject, newbr: BaseRecord, opts: UpdateOption*): Unit =  {
    useDb ( db =>
      update(qry, newbr, db, opts :_*)
    )
  }

  /**
   * Upsert records with a DBObject query
   */
  @deprecated("Use updateOne, updateMany, or replaceOne instead", "3.4.2")
  def upsert(query: DBObject, update: DBObject): Unit = {
    useColl( coll =>
      coll.update(query, update, true, false)
    )
  }

  /**
   * Update one record with a DBObject query
   */
  @deprecated("Use updateOne, updateMany, or replaceOne instead", "3.4.2")
  def update(query: DBObject, update: DBObject): Unit = {
    useColl( coll =>
      coll.update(query, update)
    )
  }

  /**
   * Update multiple records with a DBObject query
   */
  @deprecated("Use updateMany instead", "3.4.2")
  def updateMulti(query: DBObject, update: DBObject): Unit = {
    useColl( coll =>
      coll.updateMulti(query, update)
    )
  }

  /**
   * Update a record with a DBObject query
   */
  @deprecated("Use updateOne, or replaceOne instead", "3.4.2")
  def update(obj: BaseRecord, update: DBObject): Unit = {
    val query = (BasicDBObjectBuilder.start
                      .add("_id", idValue(obj))
                      .get)
    this.update(query, update)
  }

  def updateOne(inst: BaseRecord, update: Bson, opts: UpdateOptions = new UpdateOptions): Box[UpdateResult] = tryo {
    updateOp(inst) {
      val id = idValue(inst)
      useCollection(_.updateOne(eqs("_id", id), update, opts))
    }
  }

  /**
   * Update only the dirty fields.
   *
   * Note: PatternField will always set the dirty flag when set.
   */
  def update(inst: BaseRecord): Unit = updateOp(inst) {
    val dirtyFields = fields(inst).filter(_.dirty_?)
    if (dirtyFields.length > 0) {
      val (fullFields, otherFields) = dirtyFields
        .map(field => (field.name, fieldDbValue(field)))
        .partition(pair => pair._2.isDefined)

      val fieldsToSet = fullFields.map(pair => (pair._1, pair._2.openOrThrowException("these are all Full")))

      val fieldsToUnset: List[String] = otherFields.filter(
        pair => pair._2 match {
          case Empty => true
          case _ => false
        }
      ).map(_._1)

      if (fieldsToSet.length > 0 || fieldsToUnset.length > 0) {
        val setQry = combine(fieldsToSet.map { case (k, v) => set(k, v) }.asJava)
        val unsetQry = combine(fieldsToUnset.map { k => set(k, 1) }.asJava)

        updateOp(inst) {
          val id = idValue(inst)
          useCollection(_.updateOne(eqs("_id", id), combine(setQry, unsetQry)))
        }
      }
    }
  }
}
