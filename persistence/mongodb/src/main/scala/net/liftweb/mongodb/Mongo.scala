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

import net.liftweb.json.{Formats, JObject}
import net.liftweb.util.{ConnectionIdentifier, DefaultConnectionIdentifier}

import java.util.concurrent.ConcurrentHashMap

import scala.collection.immutable.HashSet

import org.bson.Document
import org.bson.conversions.Bson
import com.mongodb.{DB, DBCollection, Mongo, MongoClient, MongoException, MongoOptions, ServerAddress}
import com.mongodb.client.{MongoCollection, MongoDatabase}
import com.mongodb.client.model.{IndexModel, IndexOptions}


/**
  * Main Mongo object
  */
object MongoDB {

  /**
    * HashMap of Mongo instance and db name tuples, keyed by ConnectionIdentifier
    */
  private[this] val dbs = new ConcurrentHashMap[ConnectionIdentifier, (MongoClient, String)]

  /**
    * Define a MongoClient db using a MongoClient instance.
    */
  def defineDb(name: ConnectionIdentifier, mngo: MongoClient, dbName: String) {
    dbs.put(name, (mngo, dbName))
  }

  /**
    * Get a MongoClient reference
    */
  private[this] def getClient(name: ConnectionIdentifier): Option[MongoClient] = {
    Option(dbs.get(name)).map { case (mngo, db) => mngo }
  }

  /**
    * Get a DB reference
    */
  @deprecated("Use useDatabase instead", "3.4.3")
  def getDb(name: ConnectionIdentifier): Option[DB] = dbs.get(name) match {
    case null => None
    case (mngo, db) => Some(mngo.getDB(db))
  }

  /**
    * Get a MongoDatabase reference
    */
  private[this] def getDatabase(name: ConnectionIdentifier): Option[MongoDatabase] = {
    Option(dbs.get(name)).map { case (mngo, db) => mngo.getDatabase(db) }
  }

  // for legacy purposes
  @deprecated("Use getCollection instead", "3.4.3")
  private[this] def getColl(name: ConnectionIdentifier, collectionName: String): Option[DBCollection] =
    getDb(name) match {
      case Some(mongo) if mongo != null =>
        Some(mongo.getCollection(collectionName))
      case _ =>
        None
    }

  /**
    * Get a Mongo collection. Gets a Mongo db first.
    */
  private[this] def getCollection[TDocument](name: ConnectionIdentifier, collectionName: String, documentClass: Class[TDocument]): Option[MongoCollection[TDocument]] =
    getDatabase(name) match {
      case Some(mongo) if mongo != null =>
        Some(mongo.getCollection(collectionName, documentClass))
      case _ =>
        None
    }

  /**
    * Executes function {@code f} with the MongoClient.
    */
  def useClient[T](name: ConnectionIdentifier)(f: (MongoClient) => T): T = {
    val client = getClient(name) match {
      case Some(mongo) =>
        mongo
      case _ =>
        throw new MongoException("Mongo not found: "+name.toString)
    }

    f(client)
  }

  /**
    * Executes function {@code f} with the mongo db named {@code name}.
    */
  @deprecated("Use useDatabase instead", "3.4.3")
  def use[T](name: ConnectionIdentifier)(f: (DB) => T): T = {

    val db = getDb(name) match {
      case Some(mongo) => mongo
      case _ => throw new MongoException("Mongo not found: "+name.toString)
    }

    f(db)
  }

  /**
    * Executes function {@code f} with the mongo db named {@code name}.
    */
  def useDatabase[T](name: ConnectionIdentifier)(f: (MongoDatabase) => T): T = {

    val db = getDatabase(name) match {
      case Some(mongo) =>
        mongo
      case _ =>
        throw new MongoException("Mongo not found: "+name.toString)
    }

    f(db)
  }

  /**
    * Executes function {@code f} with the mongo named {@code name}.
    * Uses the default ConnectionIdentifier
    */
  @deprecated("Use useDefaultDatabase instead", "3.4.3")
  def use[T](f: (DB) => T): T = {

    val db = getDb(DefaultConnectionIdentifier) match {
      case Some(mongo) => mongo
      case _ => throw new MongoException("Mongo not found: "+DefaultConnectionIdentifier.toString)
    }

    f(db)
  }

   /**
    * Executes function {@code f} with the DefaultConnectionIdentifier
    */
  def useDefaultDatabase[T](f: (MongoDatabase) => T): T = {
    val db = getDatabase(DefaultConnectionIdentifier) match {
      case Some(mongo) =>
        mongo
      case _ =>
        throw new MongoException("Mongo not found: "+DefaultConnectionIdentifier.toString)
    }

    f(db)
  }

  /**
    * Executes function {@code f} with the mongo named {@code name} and
    * collection names {@code collectionName}. Gets a collection for you.
    */
  @deprecated("Use useMongoCollection instead", "3.4.3")
  def useCollection[T](name: ConnectionIdentifier, collectionName: String)(f: (DBCollection) => T): T = {
    val coll = getColl(name, collectionName) match {
      case Some(collection) => collection
      case _ => throw new MongoException("Mongo not found: "+collectionName+". ConnectionIdentifier: "+name.toString)
    }

    f(coll)
  }

  /**
    * Executes function {@code f} with the mongo named {@code name} and
    * collection names {@code collectionName}. Gets a collection for you.
    */
  def useMongoCollection[TDocument, T](name: ConnectionIdentifier, collectionName: String, documentClass: Class[TDocument])(f: (MongoCollection[TDocument]) => T): T = {
    val coll = getCollection[TDocument](name, collectionName, documentClass) match {
      case Some(collection) =>
        collection
      case _ =>
        throw new MongoException("Mongo not found: "+collectionName+". ConnectionIdentifier: "+name.toString)
    }

    f(coll)
  }

  /**
    * Same as above except uses DefaultConnectionIdentifier
    */
  @deprecated("Use useMongoCollection instead", "3.4.3")
  def useCollection[T](collectionName: String)(f: (DBCollection) => T): T = {
    val coll = getColl(DefaultConnectionIdentifier, collectionName) match {
      case Some(collection) => collection
      case _ => throw new MongoException("Mongo not found: "+collectionName+". ConnectionIdentifier: "+DefaultConnectionIdentifier.toString)
    }

    f(coll)
  }

  /**
   * Same as above except uses DefaultConnectionIdentifier
   */
  def useMongoCollection[TDocument, T](collectionName: String, documentClass: Class[TDocument])(f: (MongoCollection[TDocument]) => T): T = {
    val coll = getCollection[TDocument](DefaultConnectionIdentifier, collectionName, documentClass) match {
      case Some(collection) =>
        collection
      case _ =>
        throw new MongoException("Mongo not found: "+collectionName+". ConnectionIdentifier: "+DefaultConnectionIdentifier.toString)
    }

    f(coll)
  }

  def useMongoCollection[T](collectionName: String)(f: (MongoCollection[Document]) => T): T = {
    val coll = getCollection[Document](DefaultConnectionIdentifier, collectionName, classOf[Document]) match {
      case Some(collection) =>
        collection
      case _ =>
        throw new MongoException("Mongo not found: "+collectionName+". ConnectionIdentifier: "+DefaultConnectionIdentifier.toString)
    }

    f(coll)
  }

  /**
    * Calls close on each MongoClient instance and clears the HashMap.
    */
  def closeAll(): Unit = {
    import scala.collection.JavaConverters._
    dbs.values.asScala.foreach { case (mngo, _) =>
      mngo.close()
    }
    dbs.clear()
  }

  /**
    * Clear the HashMap.
    */
  def clear(): Unit = {
    dbs.clear()
  }

  /**
    * Remove a specific ConnectionIdentifier from the HashMap.
    */
  def remove(id: ConnectionIdentifier): Option[MongoDatabase] = {
    val db = getDatabase(id)
    dbs.remove(id)
    db
  }
}
