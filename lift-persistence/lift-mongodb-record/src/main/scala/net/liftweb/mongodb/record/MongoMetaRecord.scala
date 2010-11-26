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
package record {

import java.util.{Calendar, UUID}
import java.util.regex.Pattern

import scala.collection.JavaConversions._

import net.liftweb.common.{Box, Empty, Full}
import net.liftweb.json.{Formats, JsonParser}
import net.liftweb.json.JsonAST._
import net.liftweb.mongodb._
import net.liftweb.mongodb.record.field._
import net.liftweb.record.{MetaRecord, Record}
import net.liftweb.record.FieldHelpers.expectedA
import net.liftweb.record.field._

import com.mongodb._
import com.mongodb.util.JSON
import org.bson.types.ObjectId

trait MongoMetaRecord[BaseRecord <: MongoRecord[BaseRecord]]
  extends MetaRecord[BaseRecord] with MongoMeta[BaseRecord] {

  self: BaseRecord =>

  /**
  * Delete the instance from backing store
  */
  def delete_!(inst: BaseRecord): Boolean = {
    foreachCallback(inst, _.beforeDelete)
    delete("_id", inst.id)
    foreachCallback(inst, _.afterDelete)
    true
  }

  protected def useColl[T](f: DBCollection => T) =
    MongoDB.useCollection(mongoIdentifier, collectionName)(f)

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
  * Find a single document by a qry using a json value
  */
  def find(json: JObject): Box[BaseRecord] = find(JObjectParser.parse(json))

  /**
  * Find a single row by a qry using String key and Any value
  */
  def find(k: String, o: Any): Box[BaseRecord] = find(new BasicDBObject(k, o))

  /**
  * Find all rows in this collection
  */
  def findAll: List[BaseRecord] = {
    /*
    * The call to toArray retrieves all documents and puts them in memory.
    */
    useColl( coll => {
      coll.find.toArray.map(dbo => fromDBObject(dbo)).toList
    })
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

    useColl( coll => {
      val cur = f(coll).limit(
        findOpts.find(_.isInstanceOf[Limit]).map(x => x.value).getOrElse(0)
      ).skip(
        findOpts.find(_.isInstanceOf[Skip]).map(x => x.value).getOrElse(0)
      )
      sort.foreach( s => cur.sort(s))
      // The call to toArray retrieves all documents and puts them in memory.
      cur.toArray.map(dbo => fromDBObject(dbo)).toList
    })
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
  def findAll(ids: List[ObjectId]): List[BaseRecord] = if (ids.isEmpty) Nil else {
    val list = new java.util.ArrayList[ObjectId]()
    for (id <- ids.distinct) list.add(id)
    val query = QueryBuilder.start("_id").in(list).get()
    findAll(query)
  }

  private def saveOp(inst: BaseRecord)(f: => Unit): Boolean = {
    foreachCallback(inst, _.beforeSave)
    f
    foreachCallback(inst, _.afterSave)
    true
  }

  /**
  * Save the instance in the appropriate backing store
  */
  def save(inst: BaseRecord, strict: Boolean): Boolean = saveOp(inst) {
    if (strict) // strict mode will call getLastError and throw an exception if there was an error
      MongoDB.useSession(mongoIdentifier) {
        db =>
          val coll = db.getCollection(collectionName)
          coll.setWriteConcern(DB.WriteConcern.STRICT)
          coll.save(inst.asDBObject)
          coll.setWriteConcern(DB.WriteConcern.NORMAL)
      }
    else
      useColl {
        coll => coll.save(inst.asDBObject)
      }
  }

  def save(inst: BaseRecord): Boolean = save(inst, false)

  /*
  * Save a document to the db using the given Mongo instance
  */
  def save(inst: BaseRecord, db: DB): Boolean = saveOp(inst) {
    db.getCollection(collectionName).save(inst.asDBObject)
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
  def update(qry: JObject, newbr: BaseRecord, db: DB, opts: UpdateOption*) {
    update(JObjectParser.parse(qry), newbr.asDBObject, db, opts :_*)
  }

  /*
  * Update records with a JObject query
  */
  def update(qry: JObject, newbr: BaseRecord, opts: UpdateOption*) {
    MongoDB.use(mongoIdentifier) ( db =>
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
                      .add("_id", obj.id)
                      .get)
    this.update(query, update)
  }

  /**
  * Create a BasicDBObject from the field names and values.
  * - MongoFieldFlavor types (List) are converted to DBObjects
  *   using asDBObject
  */
  def asDBObject(inst: BaseRecord): DBObject = {

    import Meta.Reflection._
    import field.MongoFieldFlavor

    val dbo = BasicDBObjectBuilder.start // use this so regex patterns can be stored.

    for (f <- fields(inst)) {
      f match {
        case field if (field.optional_? && field.valueBox.isEmpty) => // don't add to DBObject
        case field: EnumTypedField[Enumeration] =>
          field.asInstanceOf[EnumTypedField[Enumeration]].valueBox foreach {
            v => dbo.add(f.name, v.id)
          }
        case field: EnumNameTypedField[Enumeration] =>
          field.asInstanceOf[EnumNameTypedField[Enumeration]].valueBox foreach {
            v => dbo.add(f.name, v.toString)
          }
        case field: MongoFieldFlavor[Any] =>
          dbo.add(f.name, field.asInstanceOf[MongoFieldFlavor[Any]].asDBObject)
        case field => field.valueBox foreach (_.asInstanceOf[AnyRef] match {
          case null => dbo.add(f.name, null)
          case x if primitive_?(x.getClass) => dbo.add(f.name, x)
          case x if mongotype_?(x.getClass) => dbo.add(f.name, x)
          case x if datetype_?(x.getClass) => dbo.add(f.name, datetype2dbovalue(x))
          case o => dbo.add(f.name, o.toString)
        })
      }
    }
    dbo.get
  }

  /**
  * Creates a new record, then sets the fields with the given DBObject.
  *
  * @param dbo - the DBObject
  * @return Box[BaseRecord]
  */
  def fromDBObject(dbo: DBObject): BaseRecord = {
    val inst: BaseRecord = createRecord
    setFieldsFromDBObject(inst, dbo)
    inst
  }

  /**
  * Populate the inst's fields with the values from a DBObject. Values are set
  * using setFromAny passing it the DBObject returned from Mongo.
  *
  * @param inst - the record that will be populated
  * @param obj - The DBObject
  * @return Box[BaseRecord]
  */
  def setFieldsFromDBObject(inst: BaseRecord, dbo: DBObject): Unit = {
    for (k <- dbo.keySet; field <- inst.fieldByName(k.toString)) {
      field.setFromAny(dbo.get(k.toString))
    }
    inst.runSafe {
      inst.fields.foreach(_.resetDirty)
    }
  }
}

}
}
}
