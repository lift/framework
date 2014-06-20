/*
* Copyright 2010-2011 WorldWide Conferencing, LLC
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

package net.liftweb
package mongodb

import org.bson.types.ObjectId

import json.{DefaultFormats, Formats}
import json.JsonAST.JObject
import util.ConnectionIdentifier

import com.mongodb.{BasicDBObject, DB, DBCollection, DBObject}

trait JsonFormats {
  // override this for custom Formats
  def formats: Formats = DefaultFormats.lossless

  implicit lazy val _formats: Formats = formats

  lazy val allFormats = DefaultFormats.lossless + new ObjectIdSerializer + new DateSerializer + new DateTimeSerializer + new PatternSerializer + new UUIDSerializer
}

/*
* This is used by both MongoDocumentMeta and MongoMetaRecord
*/
trait MongoMeta[BaseDocument] extends JsonFormats {

  def connectionIdentifier: ConnectionIdentifier

  // class name has a $ at the end.
  private lazy val _collectionName = getClass.getName.replaceAllLiterally("$", "")

  /*
  * Collection names should begin with letters or an underscore and may include
  * numbers; $ is reserved. Collections can be organized in namespaces; these
  * are named groups of collections defined using a dot notation. For example,
  * you could define collections blog.posts and blog.authors, both reside under
  * "blog". Note that this is simply an organizational mechanism for the user
  * -- the collection namespace is flat from the database's perspective.
  * From: http://www.mongodb.org/display/DOCS/Collections
  */
  def fixCollectionName = {
    val colName = MongoRules.collectionName.vend.apply(connectionIdentifier, _collectionName)

    if (colName.contains("$")) colName.replaceAllLiterally("$", "_d_")
    else colName
  }

  /**
  * The name of the database collection.  Override this method if you
  * want to change the collection to something other than the name of
  * the class with an 's' appended to the end.
  */
  def collectionName: String = fixCollectionName

  /*
   * Use the collection associated with this Meta.
   */
  def useColl[T](f: DBCollection => T): T

  /*
   * Use the db associated with this Meta.
   */
  def useDb[T](f: DB => T): T

  /*
  * Count all documents
  */
  def count: Long = useColl { coll => coll.getCount }

  /*
  * Count documents by DBObject query
  */
  def count(qry: DBObject):Long = useColl { coll => coll.getCount(qry) }

  /*
  * Count documents by JObject query
  */
  def count(qry: JObject):Long = count(JObjectParser.parse(qry))

  /*
  * Count distinct records on a given field
  */
  def countDistinct(key: String, query: DBObject): Long =
    useColl { coll => coll.distinct(key, query).size }

  /*
  * Delete documents by a DBObject query
  */
  def delete(qry: DBObject): Unit =
    useColl { coll => coll.remove(qry) }

  // delete a document
  def delete(k: String, v: Any) {
    delete(new BasicDBObject(k, v match {
      case s: String if (ObjectId.isValid(s)) => new ObjectId(s)
      case _ => v
    }))
  }

  /*
  * Delete documents by a JObject query
  */
  def delete(qry: JObject): Unit = delete(JObjectParser.parse(qry))

  /* drop this document collection */
  def drop: Unit =  useColl { coll => coll.drop }

  /*
  * Ensure an index exists
  */
  @deprecated("use createIndex(JObject) instead.", "2.6")
  def ensureIndex(keys: JObject): Unit =
    useColl { coll => coll.ensureIndex(JObjectParser.parse(keys)) }

  /*
  * Ensure an index exists and make unique
  */
  @deprecated("use createIndex(JObject, Boolean) instead.", "2.6")
  def ensureIndex(keys: JObject, unique: Boolean): Unit = {
    val options = new BasicDBObject
    if (unique) options.put("unique", true)
    useColl { coll =>
      coll.ensureIndex(JObjectParser.parse(keys), options)
    }
  }

  def createIndex(keys: JObject, unique: Boolean = false): Unit = {
    val options = new BasicDBObject
    if (unique) options.put("unique", true)
    useColl { coll =>
      coll.createIndex(JObjectParser.parse(keys), options)
    }
  }

  /*
  * Ensure an index exists with options
  */
  @deprecated("use createIndex(JObject, JObject) instead.", "2.6")
  def ensureIndex(keys: JObject, opts: JObject): Unit =
    useColl { coll =>
      coll.ensureIndex(JObjectParser.parse(keys), JObjectParser.parse(opts))
    }

  def createIndex(keys: JObject, opts: JObject): Unit =
    useColl { coll =>
      coll.createIndex(JObjectParser.parse(keys), JObjectParser.parse(opts))
    }

  /*
  * Update document with a DBObject query using the given Mongo instance.
  */
  def update(qry: DBObject, newobj: DBObject, db: DB, opts: UpdateOption*) {
    val dboOpts = opts.toList
    db.getCollection(collectionName).update(
      qry,
      newobj,
      dboOpts.find(_ == Upsert).map(x => true).getOrElse(false),
      dboOpts.find(_ == Multi).map(x => true).getOrElse(false)
    )
  }

  /*
  * Update document with a JObject query using the given Mongo instance.
  */
  def update(qry: JObject, newobj: JObject, db: DB, opts: UpdateOption*) {
    update(
      JObjectParser.parse(qry),
      JObjectParser.parse(newobj),
      db,
      opts :_*
    )
  }

  /*
  * Update document with a JObject query.
  */
  def update(qry: JObject, newobj: JObject, opts: UpdateOption*) {
    useDb { db => update(qry, newobj, db, opts :_*) }
  }
}

/*
* For passing in options to the find function
*/
abstract sealed class FindOption {
  def value: Int
}
case class Limit(value: Int) extends FindOption
case class Skip(value: Int) extends FindOption

/*
* For passing in options to the update function
*/
abstract sealed class UpdateOption
case object Upsert extends UpdateOption
case object Multi extends UpdateOption

