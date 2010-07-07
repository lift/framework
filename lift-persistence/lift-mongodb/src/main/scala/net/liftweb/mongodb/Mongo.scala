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

import java.util.concurrent.ConcurrentHashMap

import scala.collection.immutable.HashSet
import scala.reflect.Manifest

import net.liftweb.json.Formats
import net.liftweb.json.JsonAST.JObject

import com.mongodb._
import org.bson.types.ObjectId

/*
* The default MongoIdentifier
*/
case object DefaultMongoIdentifier extends MongoIdentifier {
  val jndiName = "test"
}

/*
* Wrapper for getting a reference to a db from the given Mongo instance
*/
case class MongoAddress(host: MongoHost, name: String) {
  def db = host.mongo.getDB(name)

  override def toString = host.host+":"+host.port+"/"+name
}

/*
* Wrapper for creating a Mongo instance
*/
abstract class MongoHostBase {
  def mongo: Mongo
}
case class MongoHost(host: String, port: Int) extends MongoHostBase {
  val mongo = new Mongo(host, port)
}
object MongoHost {
  def apply(): MongoHost = MongoHost("localhost", 27017)
  def apply(host: String): MongoHost = MongoHost(host, 27017)
}

/*
* Wrapper for creating a Paired Mongo instance
*/
case class MongoPair(left: DBAddress, right: DBAddress) extends MongoHostBase {
  val mongo = new Mongo(left, right)
}

/*
* Main Mongo object
*/
object MongoDB {

  /*
  * HashMap of MongoAddresses, keyed by MongoIdentifier
  */
  private val dbs = new ConcurrentHashMap[MongoIdentifier, MongoAddress]

  /*
  * Define a Mongo db
  */
  def defineDb(name: MongoIdentifier, address: MongoAddress) {
    dbs.put(name, address)
  }

  /*
  * Define and authenticate a Mongo db
  */
  def defineDbAuth(name: MongoIdentifier, address: MongoAddress, username: String, password: String) {
    if (!address.db.authenticate(username, password.toCharArray))
      throw new MongoException("Authorization failed: "+address.toString)

    dbs.put(name, address)
  }

  /*
  * Get a DB reference
  */
  private def getDb(name: MongoIdentifier): Option[DB] = dbs.get(name) match {
    case null => None
    case ma: MongoAddress => Some(ma.db)
  }

  /*
  * Get a Mongo collection. Gets a Mongo db first.
  */
  private def getCollection(name: MongoIdentifier, collectionName: String): Option[DBCollection] = getDb(name) match {
    case Some(mongo) if mongo != null => Some(mongo.getCollection(collectionName))
    case _ => None
  }

  /**
  * Executes function {@code f} with the mongo db named {@code name}.
  */
  def use[T](name: MongoIdentifier)(f: (DB) => T): T = {

    val db = getDb(name) match {
      case Some(mongo) => mongo
      case _ => throw new MongoException("Mongo not found: "+name.toString)
    }

    f(db)
  }

  /**
  * Executes function {@code f} with the mongo named {@code name}. Uses the default mongoIdentifier
  */
  def use[T](f: (DB) => T): T = {

    val db = getDb(DefaultMongoIdentifier) match {
      case Some(mongo) => mongo
      case _ => throw new MongoException("Mongo not found: "+DefaultMongoIdentifier.toString)
    }

    f(db)
  }

  /**
  * Executes function {@code f} with the mongo named {@code name} and collection names {@code collectionName}.
  * Gets a collection for you.
  */
  def useCollection[T](name: MongoIdentifier, collectionName: String)(f: (DBCollection) => T): T = {
    val coll = getCollection(name, collectionName) match {
      case Some(collection) => collection
      case _ => throw new MongoException("Collection not found: "+collectionName+". MongoIdentifier: "+name.toString)
    }

    f(coll)
  }

  /**
  * Same as above except uses DefaultMongoIdentifier
  */
  def useCollection[T](collectionName: String)(f: (DBCollection) => T): T = {
    val coll = getCollection(DefaultMongoIdentifier, collectionName) match {
      case Some(collection) => collection
      case _ => throw new MongoException("Collection not found: "+collectionName+". MongoIdentifier: "+DefaultMongoIdentifier.toString)
    }

    f(coll)
  }

  /**
  * Executes function {@code f} with the mongo db named {@code name}. Uses the same socket
  * for the entire function block. Allows multiple operations on the same thread/socket connection
  * and the use of getLastError.
  * See: http://www.mongodb.org/display/DOCS/Java+Driver+Concurrency
  */
  def useSession[T](name: MongoIdentifier)(f: (DB) => T): T = {

    val db = getDb(name) match {
      case Some(mongo) => mongo
      case _ => throw new MongoException("Mongo not found: "+name.toString)
    }

    // start the request
    db.requestStart
    try {
      f(db)
    }
    finally {
      // end the request
      db.requestDone
    }
  }

  /**
  * Same as above except uses DefaultMongoIdentifier
  */
  def useSession[T](f: (DB) => T): T = {

    val db = getDb(DefaultMongoIdentifier) match {
      case Some(mongo) => mongo
      case _ => throw new MongoException("Mongo not found: "+DefaultMongoIdentifier.toString)
    }

    // start the request
    db.requestStart
    try {
      f(db)
    }
    finally {
      // end the request
      db.requestDone
    }
  }

  //
  def close {
    dbs.clear
  }
}

/*
* A trait for identfying Mongo instances
*/
trait MongoIdentifier {
  def jndiName: String
  override def toString() = "MongoIdentifier("+jndiName+")"
  override def hashCode() = jndiName.hashCode()
  override def equals(other: Any): Boolean = other match {
    case mi: MongoIdentifier => mi.jndiName == this.jndiName
    case _ => false
  }
}

/*
* This is used by both MongoDocumentMeta and MongoMetaRecord
*/
trait MongoMeta[BaseDocument] {

  // class name has a $ at the end. because it's an object(?)
  private lazy val _collectionName = {
    getClass.getName.split("\\.").toList.last.replace("$", "")+"s"
  }

  /*
  * Collection names should begin with letters or an underscore and may include
  * numbers; $ is reserved. Collections can be organized in namespaces; these
  * are named groups of collections defined using a dot notation. For example,
  * you could define collections blog.posts and blog.authors, both reside under
  * "blog". Note that this is simply an organizational mechanism for the user
  * -- the collection namespace is flat from the database's perspective.
  * From: http://www.mongodb.org/display/DOCS/Collections
  */
  def fixCollectionName = _collectionName.toLowerCase match {
    case name if (name.contains("$")) => name.replace("$", "_d_")
    case name => name
  }

  /**
  * The name of the database collection.  Override this method if you
  * want to change the collection to something other than the name of
  * the class with an 's' appended to the end.
  */
  def collectionName: String = fixCollectionName

  // override this to specify a MongoIdentifier for this MongoDocument type
  def mongoIdentifier: MongoIdentifier = DefaultMongoIdentifier

  import net.liftweb.json.{NoTypeHints, Serialization, ShortTypeHints, DefaultFormats} //.{read, write => swrite}
  import net.liftweb.json.JsonAST.{JField, JString, JObject}

  // override this for custom Formats
  def formats: Formats = DefaultFormats.lossless

  implicit lazy val _formats: Formats = formats

  /*
  * Count all documents
  */
  def count: Long = {
    MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
      coll.getCount
    )
  }

  /*
  * Count documents by DBObject query
  */
  private def count(qry: DBObject):Long = {
    MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
      coll.getCount(qry)
    )
  }

  /*
  * Count documents by JObject query
  */
  def count(qry: JObject):Long = count(JObjectParser.parse(qry))

  /*
  * Delete documents by a DBObject query
  */
  def delete(qry: DBObject) {
    MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
      coll.remove(qry)
    )
  }

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
  def delete(qry: JObject) {
    delete(JObjectParser.parse(qry))
  }

  /* drop this document collection */
  def drop {
    MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
      coll.drop
    )
  }

  /*
  * Ensure an index exists
  */
  def ensureIndex(keys: JObject) {
    MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
      coll.ensureIndex(JObjectParser.parse(keys))
    })
  }

  /*
  * Ensure an index exists and make unique
  */
  def ensureIndex(keys: JObject, unique: Boolean) {
    MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
      coll.ensureIndex(JObjectParser.parse(keys), new BasicDBObject("unique", true))
    })
  }

  /*
  * Ensure an index exists with options
  */
  def ensureIndex(keys: JObject, opts: JObject) {
    MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
      coll.ensureIndex(JObjectParser.parse(keys), JObjectParser.parse(opts))
    })
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
    MongoDB.use(mongoIdentifier) ( db => {
      update(qry, newobj, db, opts :_*)
    })
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

/*
* These traits provide lift-json related conveniece methods for case classes
* and their companion objects
*/
trait JsonObject[BaseDocument] {
  self: BaseDocument =>

  def meta: JsonObjectMeta[BaseDocument]

  // convert class to a json value
  def asJObject()(implicit formats: Formats): JObject = meta.toJObject(this)

}

class JsonObjectMeta[BaseDocument](implicit mf: Manifest[BaseDocument]) {

  import net.liftweb.json.Extraction._

  // create an instance of BaseDocument from a JObject
  def create(in: JObject)(implicit formats: Formats): BaseDocument =
    extract(in)(formats, mf)

  // convert class to a JObject
  def toJObject(in: BaseDocument)(implicit formats: Formats): JObject =
    decompose(in)(formats).asInstanceOf[JObject]
}

/*
* Case class for a db reference (foreign key).
* ref = collection name, id is the value of the reference
*/
case class MongoRef(ref: String, id: String) {
  def objectId = new ObjectId(id)
}

}
}
