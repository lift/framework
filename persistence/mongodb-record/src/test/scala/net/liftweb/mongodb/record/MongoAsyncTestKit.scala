/*
 * Copyright 2010-2017 WorldWide Conferencing, LLC
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

import util.{ConnectionIdentifier, DefaultConnectionIdentifier, Props}

import scala.jdk.CollectionConverters._
import scala.concurrent.{Await, Promise}
import scala.concurrent.duration._
import java.util.concurrent.TimeoutException

import org.specs2.mutable.Specification
import org.specs2.specification.BeforeAfterEach

import org.bson.Document

import com.mongodb.Block
import com.mongodb.async.SingleResultCallback
import com.mongodb.async.client.{MongoClients, MongoDatabase}

// The sole mongo object for testing async
object TestMongoAsync {
  val mongo = {
    val uri = Props.get("mongo.test.uri", "127.0.0.1:27017")
    MongoClients.create(s"mongodb://$uri")
  }

  class SingleResultCallbackF[A]() extends SingleResultCallback[A] {
    private[this] val p = Promise[A]()

    override def onResult(result: A, error: Throwable): Unit = {
      Option(error) match {
        case None =>
          p.success(result)
        case Some(t) =>
          p.failure(t)
      }
    }

    def future = p.future
  }

  lazy val isMongoRunning: Boolean =
    try {
      val res = mongo.listDatabaseNames
      val cb = new SingleResultCallbackF[Void]()

      res.forEach(
        new Block[String]() {
          override def apply(name: String): Unit = { }
        },
        cb
      )

      // this will throw an exception if it can't connect to the db
      Await.result(cb.future, Duration(2000, MILLISECONDS))
      true
    } catch {
      case _: TimeoutException =>
        false
    }
}

trait MongoAsyncTestKit extends Specification with BeforeAfterEach {
  sequential

  protected def dbName = "lift_record_"+this.getClass.getName
    .replace("$", "")
    .replace("net.liftweb.mongodb.record.", "")
    .replace(".", "_")
    .toLowerCase

  // If you need more than one db, override this
  protected def dbs: List[(ConnectionIdentifier, String)] =
    (DefaultConnectionIdentifier, dbName) :: Nil

  def debug: Boolean = false

  def before = {
    // define the dbs
    dbs.foreach { case (id, db) =>
      MongoAsync.defineDb(id, TestMongoAsync.mongo.getDatabase(db))
      MongoDB.defineDb(id, TestMongo.mongo, db)
    }
  }

  def checkMongoIsRunning = {
    TestMongoAsync.isMongoRunning must beEqualTo(true).orSkip
    TestMongo.isMongoRunning must beEqualTo(true).orSkip
  }

  def after = {
    if (!debug && TestMongoAsync.isMongoRunning) {
      val cb = new SingleResultCallback[Void] {
        override def onResult(result: Void, t: Throwable) = { }
      }
      // drop the databases
      dbs.foreach { case (id, _) =>
        MongoAsync.use(id) { _.drop(cb) }
      }
    }

    // clear the mongo instances
    dbs.foreach { case (id, _) =>
      MongoAsync.remove(id)
    }

    if (!debug && TestMongo.isMongoRunning) {
      // drop the databases
      dbs.foreach { case (id, _) =>
        MongoDB.use(id) { db => db.dropDatabase }
      }
    }

    // clear the mongo instances
    dbs.foreach { case (id, _) =>
      MongoDB.remove(id)
    }
  }
}

