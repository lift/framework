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

import util.{ConnectionIdentifier, DefaultConnectionIdentifier}

import java.util.concurrent.ConcurrentHashMap

import scala.collection.immutable.HashSet

import com.mongodb.{DB, DBCollection, Mongo, MongoClient, MongoException, MongoOptions, ServerAddress}

/*
* Wrapper for getting a reference to a db from the given Mongo instance
*/
@deprecated("Use MongoClient to define your dbs.", "2.6")
case class MongoAddress(host: MongoHostBase, name: String) {
  def db = host.mongo.getDB(name)
}

/*
* Wrapper for creating a Mongo instance
*/
@deprecated("Use MongoClient to define your dbs.", "2.6")
abstract class MongoHostBase {
  def mongo: Mongo
}

@deprecated("Use MongoClient to define your dbs.", "2.6")
case class MongoHost(server: ServerAddress = new ServerAddress, options: MongoOptions = new MongoOptions) extends MongoHostBase {
  lazy val mongo = new Mongo(server, options)
}

@deprecated("Use MongoClient to define your dbs.", "2.6")
object MongoHost {
  def apply(host: String): MongoHost = MongoHost(new ServerAddress(host, 27017))
  def apply(host: String, port: Int): MongoHost = MongoHost(new ServerAddress(host, port))
  def apply(host: String, port: Int, options: MongoOptions): MongoHost = MongoHost(new ServerAddress(host, port), options)
}

/*
 * Wrapper for creating a Replica Set
 */
@deprecated("Use MongoClient to define your dbs.", "2.6")
case class MongoSet(dbs: List[ServerAddress], options: MongoOptions = new MongoOptions) extends MongoHostBase {
  import scala.collection.JavaConversions._
  lazy val mongo = new Mongo(dbs, options)
}

/*
* Main Mongo object
*/
object MongoDB {

  /*
  * HashMap of Mongo instance and db name tuples, keyed by ConnectionIdentifier
  */
  private val dbs = new ConcurrentHashMap[ConnectionIdentifier, (Mongo, String)]

  /*
  * Define a Mongo db
  */
  @deprecated("Use defineDb that takes a MongoClient instance.", "2.6")
  def defineDb(name: ConnectionIdentifier, address: MongoAddress) {
    dbs.put(name, (address.host.mongo, address.name))
  }
  /*
   * Define a Mongo db using a Mongo instance.
   */
  @deprecated("Use defineDb that takes a MongoClient instance.", "2.6")
  def defineDb(name: ConnectionIdentifier, mngo: Mongo, dbName: String) {
    dbs.put(name, (mngo, dbName))
  }

  /**
    * Define a Mongo db using a MongoClient instance.
    */
  def defineDb(name: ConnectionIdentifier, mngo: MongoClient, dbName: String) {
    dbs.put(name, (mngo, dbName))
  }

  /*
  * Define and authenticate a Mongo db
  */
  @deprecated("Use defineDbAuth that takes a MongoClient instance.", "2.6")
  def defineDbAuth(name: ConnectionIdentifier, address: MongoAddress, username: String, password: String) {
    if (!address.db.authenticate(username, password.toCharArray))
      throw new MongoException("Authorization failed: "+address.toString)

    dbs.put(name, (address.host.mongo, address.name))
  }

  /*
  * Define and authenticate a Mongo db using a Mongo instance.
  */
  @deprecated("Use defineDbAuth that takes a MongoClient instance.", "2.6")
  def defineDbAuth(name: ConnectionIdentifier, mngo: Mongo, dbName: String, username: String, password: String) {
    if (!mngo.getDB(dbName).authenticate(username, password.toCharArray))
      throw new MongoException("Authorization failed: "+mngo.toString)

    dbs.put(name, (mngo, dbName))
  }

  /**
    * Define and authenticate a Mongo db using a MongoClient instance.
    */
  def defineDbAuth(name: ConnectionIdentifier, mngo: MongoClient, dbName: String, username: String, password: String) {
    if (!mngo.getDB(dbName).authenticate(username, password.toCharArray))
      throw new MongoException("Authorization failed: "+mngo.toString)

    dbs.put(name, (mngo, dbName))
  }

  /*
  * Get a DB reference
  */
  def getDb(name: ConnectionIdentifier): Option[DB] = dbs.get(name) match {
    case null => None
    case (mngo, db) => Some(mngo.getDB(db))
  }

  /*
  * Get a Mongo collection. Gets a Mongo db first.
  */
  private def getCollection(name: ConnectionIdentifier, collectionName: String): Option[DBCollection] = getDb(name) match {
    case Some(mongo) if mongo != null => Some(mongo.getCollection(collectionName))
    case _ => None
  }

  /**
  * Executes function {@code f} with the mongo db named {@code name}.
  */
  def use[T](name: ConnectionIdentifier)(f: (DB) => T): T = {

    val db = getDb(name) match {
      case Some(mongo) => mongo
      case _ => throw new MongoException("Mongo not found: "+name.toString)
    }

    f(db)
  }

  /**
  * Executes function {@code f} with the mongo named {@code name}. Uses the default ConnectionIdentifier
  */
  def use[T](f: (DB) => T): T = {

    val db = getDb(DefaultConnectionIdentifier) match {
      case Some(mongo) => mongo
      case _ => throw new MongoException("Mongo not found: "+DefaultConnectionIdentifier.toString)
    }

    f(db)
  }

  /**
  * Executes function {@code f} with the mongo named {@code name} and collection names {@code collectionName}.
  * Gets a collection for you.
  */
  def useCollection[T](name: ConnectionIdentifier, collectionName: String)(f: (DBCollection) => T): T = {
    val coll = getCollection(name, collectionName) match {
      case Some(collection) => collection
      case _ => throw new MongoException("Mongo not found: "+collectionName+". ConnectionIdentifier: "+name.toString)
    }

    f(coll)
  }

  /**
  * Same as above except uses DefaultConnectionIdentifier
  */
  def useCollection[T](collectionName: String)(f: (DBCollection) => T): T = {
    val coll = getCollection(DefaultConnectionIdentifier, collectionName) match {
      case Some(collection) => collection
      case _ => throw new MongoException("Mongo not found: "+collectionName+". ConnectionIdentifier: "+DefaultConnectionIdentifier.toString)
    }

    f(coll)
  }

  /**
  * Executes function {@code f} with the mongo db named {@code name}. Uses the same socket
  * for the entire function block. Allows multiple operations on the same thread/socket connection
  * and the use of getLastError.
  * See: http://docs.mongodb.org/ecosystem/drivers/java-concurrency/
  */
  def useSession[T](name: ConnectionIdentifier)(f: (DB) => T): T = {

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
  * Same as above except uses DefaultConnectionIdentifier
  */
  def useSession[T](f: (DB) => T): T = {

    val db = getDb(DefaultConnectionIdentifier) match {
      case Some(mongo) => mongo
      case _ => throw new MongoException("Mongo not found: "+DefaultConnectionIdentifier.toString)
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
    * Clears HashMap of mongo instances.
    */
  @deprecated("Use clearAll instead.", "2.6")
  def close: Unit = {
    dbs.clear
  }

  /**
    * Calls close on each Mongo instance and clears the HashMap.
    */
  def closeAll(): Unit = {
    import scala.collection.JavaConversions._
    dbs.values.foreach { case (mngo, _) =>
      mngo.close()
    }
    dbs.clear()
  }
}
