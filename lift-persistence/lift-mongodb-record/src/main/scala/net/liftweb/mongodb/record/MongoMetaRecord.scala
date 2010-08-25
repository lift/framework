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

import java.util.Calendar
import java.util.regex.Pattern

import scala.collection.JavaConversions._

import net.liftweb.common.{Box, Empty, Full}
import net.liftweb.json.Formats
import net.liftweb.json.JsonAST.JObject
import net.liftweb.mongodb._
import net.liftweb.record.{MetaRecord, Record}
import net.liftweb.record.field._

import com.mongodb._
import com.mongodb.util.JSON
import org.bson.types.ObjectId

trait MongoMetaRecord[BaseRecord <: MongoRecord[BaseRecord]]
  extends MetaRecord[BaseRecord] with MongoMeta[BaseRecord] {

  self: BaseRecord =>

  //def afterCommit: List[BaseRecord => Unit] = Nil

  /**
  * Delete the instance from backing store
  */
  def delete_!(inst: BaseRecord): Boolean = {
    foreachCallback(inst, _.beforeDelete)
    delete("_id", inst.id)
    foreachCallback(inst, _.afterDelete)
    true
  }

  def bulkDelete_!!(qry: DBObject): Unit = {
  	MongoDB.useCollection(mongoIdentifier, collectionName)(coll => {
  		coll.remove(qry)
  	})
  }

  def bulkDelete_!!(k: String, o: Any): Unit = bulkDelete_!!(new BasicDBObject(k, o))

  /**
  * Find a single row by a qry, using a DBObject.
  */
  def find(qry: DBObject): Box[BaseRecord] = {
    MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
      coll.findOne(qry) match {
        case null => Empty
        case dbo => fromDBObject(dbo)
      }
    )
  }

  /**
  * Find a single row by an ObjectId
  */
  def find(oid: ObjectId): Box[BaseRecord] = find(new BasicDBObject("_id", oid))

  /**
  * Find a single row by Any
  * This doesn't work as find because we need JObject's to be implicitly converted.
  *
  */
  def findAny(a: Any): Box[BaseRecord] = find(new BasicDBObject("_id", a))

  /**
  * Find a single row by a String id
  */
  def find(s: String): Box[BaseRecord] = ObjectId.isValid(s) match {
    case true => find(new BasicDBObject("_id", new ObjectId(s)))
    case false => find(new BasicDBObject("_id", s))
  }

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
    MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
      coll.find.toArray.flatMap(dbo => fromDBObject(dbo).toList).toList
    })
  }

  /**
  * Find all rows using a DBObject query.
  */
  def findAll(qry: DBObject, sort: Option[DBObject], opts: FindOption*): List[BaseRecord] = {
    val findOpts = opts.toList

    MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
      val cur = coll.find(qry).limit(
        findOpts.find(_.isInstanceOf[Limit]).map(x => x.value).getOrElse(0)
      ).skip(
        findOpts.find(_.isInstanceOf[Skip]).map(x => x.value).getOrElse(0)
      )
      sort.foreach( s => cur.sort(s))
      // The call to toArray retrieves all documents and puts them in memory.
      cur.toArray.flatMap(dbo => fromDBObject(dbo).toList).toList
    })
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
      MongoDB.useCollection(mongoIdentifier, collectionName) {
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
    MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
      coll.insert(insts.map(_.asDBObject).toArray)
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
  * Create a BasicDBObject from the field names and values.
  * - MongoFieldFlavor types (List) are converted to DBObjects
  *   using asDBObject
  */
  def asDBObject(inst: BaseRecord): DBObject = {

    import Meta.Reflection._
    import field.MongoFieldFlavor

    val dbo = BasicDBObjectBuilder.start // use this so regex patterns can be stored.

    for (f <- fields) {
      fieldByName(f.name, inst) match {
        case Full(field) if (field.optional_? && field.valueBox.isEmpty) => // don't add to DBObject
        /* FIXME: Doesn't work
        case Full(field) if field.isInstanceOf[CountryField[Any]] =>
          dbo.put(f.name, field.asInstanceOf[CountryField[Any]].value)
        */
        case Full(field: MongoFieldFlavor[Any]) =>
          dbo.add(f.name, field.asInstanceOf[MongoFieldFlavor[Any]].asDBObject)
        case Full(field) => field.valueBox foreach (_.asInstanceOf[AnyRef] match {
          case x if primitive_?(x.getClass) => dbo.add(f.name, x)
          case x if datetype_?(x.getClass) => dbo.add(f.name, datetype2dbovalue(x))
          case x if mongotype_?(x.getClass) => dbo.add(f.name, mongotype2dbovalue(x, formats))
          case o => dbo.add(f.name, o.toString)
        })
        case _ => //dbo.markAsPartialObject // so we know it's only partial
      }
    }
    dbo.get
  }

  /**
  * Creates a new record from a then sets the fields with the given DBObject.
  *
  * @param dbo - the DBObject
  * @return Box[BaseRecord]
  */
  def fromDBObject(dbo: DBObject): Box[BaseRecord] = {
    val inst: BaseRecord = createRecord
    setFieldsFromDBObject(inst, dbo) map (_ => inst)
  }

  /**
  * Populate the inst's fields with the values from a DBObject. Values are set
  * using setFromAny passing it the DBObject returned from Mongo.
  *
  * @param inst - the record that will be populated
  * @param obj - The DBObject
  * @return Box[BaseRecord]
  */
  def setFieldsFromDBObject(inst: BaseRecord, dbo: DBObject): Box[Unit] = {
    dbo.keySet.toList.foreach {
      k => inst.fieldByName(k.toString).map {
        field => field.setFromAny(dbo.get(k.toString))
      }
    }
    Full(())
  }

}

}
}
}
