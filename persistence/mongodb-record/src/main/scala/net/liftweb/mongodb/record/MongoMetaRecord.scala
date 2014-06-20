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
package record

import java.util.{Calendar, UUID}
import java.util.regex.Pattern

import scala.collection.JavaConversions._

import net.liftweb.common._
import net.liftweb.json.{Formats, JsonParser}
import net.liftweb.json.JsonAST._
import net.liftweb.mongodb._
import net.liftweb.mongodb.record.field._
import net.liftweb.record.{MandatoryTypedField, MetaRecord, Record}
import net.liftweb.record.FieldHelpers.expectedA
import net.liftweb.record.field._
import net.liftweb.util.ConnectionIdentifier

import com.mongodb._
import com.mongodb.util.JSON
import org.bson.types.ObjectId

trait MongoMetaRecord[BaseRecord <: MongoRecord[BaseRecord]]
  extends BsonMetaRecord[BaseRecord] with MongoMeta[BaseRecord] {

  self: BaseRecord =>

  /*
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

  /*
   * Use the collection associated with this Meta.
   */
  def useColl[T](f: DBCollection => T): T =
    MongoDB.useCollection(connectionIdentifier, collectionName)(f)

  /*
   * Use the db associated with this Meta.
   */
  def useDb[T](f: DB => T): T = MongoDB.use(connectionIdentifier)(f)

  /**
  * Delete the instance from backing store
  */
  def delete_!(inst: BaseRecord): Boolean = {
    foreachCallback(inst, _.beforeDelete)
    delete("_id", idValue(inst))
    foreachCallback(inst, _.afterDelete)
    true
  }

  def bulkDelete_!!(qry: DBObject): Unit = {
    useColl(coll =>
      coll.remove(qry)
    )
  }

  def bulkDelete_!!(k: String, o: Any): Unit = bulkDelete_!!(new BasicDBObject(k, o))

  /**
  * Find a single row by a qry, using a DBObject.
  */
  def find(qry: DBObject): Box[BaseRecord] = {
    useColl( coll =>
      coll.findOne(qry) match {
        case null => Empty
        case dbo => Full(fromDBObject(dbo))
      }
    )
  }

  /**
  * Find a single row by an ObjectId
  */
  def find(oid: ObjectId): Box[BaseRecord] = find(new BasicDBObject("_id", oid))

  /**
  * Find a single row by a UUID
  */
  def find(uid: UUID): Box[BaseRecord] = find(new BasicDBObject("_id", uid))

  /**
  * Find a single row by Any
  * This doesn't work as find because we need JObject's to be implicitly converted.
  *
  */
  def findAny(a: Any): Box[BaseRecord] = find(new BasicDBObject("_id", a))

  /**
  * Find a single row by a String id
  */
  def find(s: String): Box[BaseRecord] =
    if (ObjectId.isValid(s))
      find(new BasicDBObject("_id", new ObjectId(s)))
    else
      find(new BasicDBObject("_id", s))

  /**
  * Find a single row by an Int id
  */
  def find(id: Int): Box[BaseRecord] = find(new BasicDBObject("_id", id))

  /**
  * Find a single row by a Long id
  */
  def find(id: Long): Box[BaseRecord] = find(new BasicDBObject("_id", id))

  /**
  * Find a single document by a qry using a json value
  */
  def find(json: JObject): Box[BaseRecord] = find(JObjectParser.parse(json))

  /**
  * Find a single row by a qry using String key and Any value
  */
  def find(k: String, o: Any): Box[BaseRecord] = find(new BasicDBObject(k, o))

  /**
    * Find all rows in this collection.
    * Retrieves all documents and puts them in memory.
    */
  def findAll: List[BaseRecord] = useColl { coll =>
    /** Mongo Cursors are both Iterable and Iterator,
      * so we need to reduce ambiguity for implicits
      */
    (coll.find: Iterator[DBObject]).map(fromDBObject).toList
  }

  /**
  * Find all rows using a DBObject query.
  */
  def findAll(qry: DBObject, sort: Option[DBObject], opts: FindOption*): List[BaseRecord] = {
    findAll(sort, opts:_*) { coll => coll.find(qry) }
  }

  /**
   * Find all rows and retrieve only keys fields.
   */
  def findAll(qry: DBObject, keys: DBObject, sort: Option[DBObject], opts: FindOption*): List[BaseRecord] = {
    findAll(sort, opts:_*) { coll => coll.find(qry, keys) }
  }

  protected def findAll(sort: Option[DBObject], opts: FindOption*)(f: (DBCollection) => DBCursor): List[BaseRecord] = {
    val findOpts = opts.toList

    useColl { coll =>
      val cur = f(coll).limit(
        findOpts.find(_.isInstanceOf[Limit]).map(_.value).getOrElse(0)
      ).skip(
        findOpts.find(_.isInstanceOf[Skip]).map(_.value).getOrElse(0)
      )
      sort.foreach(s => cur.sort(s))
      // This retrieves all documents and puts them in memory.
      (cur: Iterator[DBObject]).map(fromDBObject).toList
    }
  }

  /**
   * Find all rows and retrieve only keys fields.
   */
  def findAll(qry: JObject, keys: JObject, sort: Option[JObject], opts: FindOption*): List[BaseRecord] = {
    val s = sort.map(JObjectParser.parse(_))
    findAll(JObjectParser.parse(qry), JObjectParser.parse(keys), s, opts :_*)
  }

  /**
  * Find all documents using a DBObject query. These are for passing in regex queries.
  */
  def findAll(qry: DBObject, opts: FindOption*): List[BaseRecord] =
    findAll(qry, None, opts :_*)

  /**
  * Find all documents using a DBObject query with sort
  */
  def findAll(qry: DBObject, sort: DBObject, opts: FindOption*): List[BaseRecord] =
    findAll(qry, Some(sort), opts :_*)

  /**
  * Find all documents using a JObject query
  */
  def findAll(qry: JObject, opts: FindOption*): List[BaseRecord] = {
    findAll(JObjectParser.parse(qry), None, opts :_*)
  }

  /**
  * Find all documents using a JObject query with sort
  */
  def findAll(qry: JObject, sort: JObject, opts: FindOption*): List[BaseRecord] =
    findAll(JObjectParser.parse(qry), Some(JObjectParser.parse(sort)), opts :_*)

  /**
  * Find all documents using a k, v query
  */
  def findAll(k: String, o: Any, opts: FindOption*): List[BaseRecord] =
    findAll(new BasicDBObject(k, o), None, opts :_*)

  /**
  * Find all documents using a k, v query with JOBject sort
  */
  def findAll(k: String, o: Any, sort: JObject, opts: FindOption*): List[BaseRecord] =
    findAll(new BasicDBObject(k, o), Some(JObjectParser.parse(sort)), opts :_*)


  /**
  * Find all documents with the given ids
  */
  def findAllByList[T](ids: List[T]): List[BaseRecord] = if (ids.isEmpty) Nil else {
    val list = new java.util.ArrayList[T]()
    for (id <- ids.distinct) list.add(id)
    val query = QueryBuilder.start("_id").in(list).get()
    findAll(query)
  }

  def findAll(ids: List[ObjectId]): List[BaseRecord] = findAllByList[ObjectId](ids)

  protected def saveOp(inst: BaseRecord)(f: => Unit): Boolean = {
    foreachCallback(inst, _.beforeSave)
    f
    foreachCallback(inst, _.afterSave)
    inst.allFields.foreach { _.resetDirty }
    true
  }

  protected def updateOp(inst: BaseRecord)(f: => Unit): Unit = {
    foreachCallback(inst, _.beforeUpdate)
    f
    foreachCallback(inst, _.afterUpdate)
    inst.allFields.foreach { _.resetDirty }
  }

  /**
    * Save the instance in the appropriate backing store. Uses the WriteConcern set on the MongoClient instance.
    */
  def save(inst: BaseRecord): Boolean = saveOp(inst) {
    useColl { coll =>
      coll.save(inst.asDBObject)
    }
  }

  /**
  * Save the instance in the appropriate backing store
  */
  def save(inst: BaseRecord, concern: WriteConcern): Boolean = saveOp(inst) {
    useColl { coll =>
      coll.save(inst.asDBObject, concern)
    }
  }

  /*
  * Save a document to the db using the given Mongo instance
  */
  def save(inst: BaseRecord, db: DB, concern: WriteConcern): Boolean = saveOp(inst) {
    db.getCollection(collectionName).save(inst.asDBObject, concern)
  }

  /**
   * Insert multiple records
   */
  def insertAll(insts: List[BaseRecord]): Unit = {
    insts.foreach(inst => foreachCallback(inst, _.beforeSave))
    useColl( coll =>
      coll.insert(insts.map(_.asDBObject).toArray:_*)
    )
    insts.foreach(inst => foreachCallback(inst, _.afterSave))
  }

  /*
  * Update records with a JObject query using the given Mongo instance
  */
  def update(qry: JObject, newbr: BaseRecord, db: DB, opts: UpdateOption*): Unit = {
    update(JObjectParser.parse(qry), newbr.asDBObject, db, opts :_*)
  }

  /*
  * Update records with a JObject query
  */
  def update(qry: JObject, newbr: BaseRecord, opts: UpdateOption*): Unit =  {
    useDb ( db =>
      update(qry, newbr, db, opts :_*)
    )
  }

  /**
  * Upsert records with a DBObject query
  */
  def upsert(query: DBObject, update: DBObject): Unit = {
    useColl( coll =>
      coll.update(query, update, true, false)
    )
  }

  /**
  * Update one record with a DBObject query
  */
  def update(query: DBObject, update: DBObject): Unit = {
    useColl( coll =>
      coll.update(query, update)
    )
  }

  /**
  * Update multiple records with a DBObject query
  */
  def updateMulti(query: DBObject, update: DBObject): Unit = {
    useColl( coll =>
      coll.updateMulti(query, update)
    )
  }

  /**
  * Update a record with a DBObject query
  */
  def update(obj: BaseRecord, update: DBObject): Unit = {
    val query = (BasicDBObjectBuilder.start
                      .add("_id", idValue(obj))
                      .get)
    this.update(query, update)
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
        val dbo = BasicDBObjectBuilder.start

        if (fieldsToSet.length > 0) {
          dbo.add(
            "$set",
            fieldsToSet.foldLeft(BasicDBObjectBuilder.start) {
              (builder, pair) => builder.add(pair._1, pair._2)
            }.get
          )
        }

        if (fieldsToUnset.length > 0) {
          dbo.add(
            "$unset",
            fieldsToUnset.foldLeft(BasicDBObjectBuilder.start) {
              (builder, fieldName) => builder.add(fieldName, 1)
            }.get
          )
        }

        update(inst, dbo.get)
      }
    }
  }
}
