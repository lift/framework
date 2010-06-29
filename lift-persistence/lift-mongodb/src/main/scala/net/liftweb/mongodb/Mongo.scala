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

import com.mongodb._

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
  def getDb(name: MongoIdentifier): Option[DB] = dbs.get(name) match {
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

}
}
