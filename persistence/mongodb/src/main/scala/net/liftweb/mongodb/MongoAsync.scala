/*
 * Copyright 2017 WorldWide Conferencing, LLC
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

package net.liftweb.mongodb

import java.util.concurrent.ConcurrentHashMap

import com.mongodb.MongoException
import com.mongodb.async.client.{MongoClient, MongoCollection, MongoDatabase}
import com.mongodb.async.SingleResultCallback
import net.liftweb.util.ConnectionIdentifier
import org.bson.Document

import scala.concurrent.Promise

private[mongodb] class SingleBooleanVoidCallback(f: () => Unit) extends SingleResultCallback[Void] {
  private[this] val p = Promise[Boolean]()

  override def onResult(result: java.lang.Void, error: Throwable): Unit = {
    Option(error) match {
      case None =>
        f()
        p.success(true)
      case Some(t) =>
        p.failure(t)
    }
  }

  def future = p.future
}

/**
  * Async version of MongoDB.
  *
  * You should only have one instance of MongoClient in a JVM.
  *
  * Example:
  *
  * {{{
  * import com.mongodb.async.client.MongoClients
  * import net.liftweb.util.{ConnectionIdentifier, DefaultConnectionIdentifier}
  * import org.bson.codecs.configuration.CodecRegistries
  *
  * val client = MongoClients.create("mongodb://127.0.0.1:27017")
  *
  * // main database
  * MongoAsync.defineDb(DefaultConnectionIdentifier, client.getDatabase("mydb"))
  *
  * // admin database
  * case object AdminIdentifier extends ConnectionIdentifier {
  *   val jndiName = "admin"
  * }
  *
  * val codecRegistry = CodecRegistries.fromRegistries(
  *   com.mongodb.MongoClient.getDefaultCodecRegistry(),
  *   CodecRegistries.fromCodecs(new LongPrimitiveCodec, new IntegerPrimitiveCodec)
  * )

  * val admin = client.getDatabase("admin").withCodecRegistry(codecRegistry)
  * MongoAsync.defineDb(AdminIdentifier, admin)
  *
  * }}}
  */
object MongoAsync {

  /**
    * HashMap of MongoDatabase instances keyed by ConnectionIdentifier
    */
  private[this] val dbs = new ConcurrentHashMap[ConnectionIdentifier, MongoDatabase]

  /**
    * Define a Mongo db using a MongoDatabase instance.
    */
  def defineDb(id: ConnectionIdentifier, db: MongoDatabase): Unit = {
    dbs.put(id, db)
  }

  /**
    * Get a MongoDatabase reference
    */
  private[this] def getDatabase(name: ConnectionIdentifier): Option[MongoDatabase] = {
    Option(dbs.get(name))
  }

  def use[T](ci: ConnectionIdentifier)(f: (MongoDatabase) => T): T = {
    val db = getDatabase(ci) match {
      case Some(mongo) => mongo
      case _ => throw new MongoException("Mongo not found: "+ci.toString)
    }
    f(db)
  }

  /**
    * Executes function {@code f} with the mongo named {@code name} and collection named {@code collectionName}.
    * Gets a collection for you.
    */
  def useCollection[T](name: ConnectionIdentifier, collectionName: String)(f: (MongoCollection[Document]) => T): T = {
    val coll = getCollection(name, collectionName) match {
      case Some(collection) => collection
      case _ => throw new MongoException("Mongo not found: "+collectionName+". ConnectionIdentifier: "+name.toString)
    }

    f(coll)
  }

  /**
    * Get a Mongo collection. Gets a Mongo db first.
    */
  private[this] def getCollection(name: ConnectionIdentifier, collectionName: String): Option[MongoCollection[Document]] = getDatabase(name) match {
    case Some(mongo) if mongo != null => Some(mongo.getCollection(collectionName))
    case _ => None
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
    Option(dbs.remove(id))
  }
}
