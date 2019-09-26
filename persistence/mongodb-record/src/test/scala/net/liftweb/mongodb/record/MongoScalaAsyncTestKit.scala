/*
 * Copyright 2020 WorldWide Conferencing, LLC
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

import net.liftweb.util.{ConnectionIdentifier, DefaultConnectionIdentifier, Props}

import scala.collection.JavaConverters._
import scala.concurrent.{Await, Promise}
import scala.concurrent.duration._
import scala.collection.mutable

import java.util.concurrent.TimeoutException

import org.specs2.mutable.Specification
import org.specs2.specification.BeforeAfterEach

import org.bson.Document

import com.mongodb.Block

import org.mongodb.scala._

// The sole mongo object for testing async
object TestMongoScalaAsync {
  val mongo = {
    val uri = Props.get("mongo.test.uri", "127.0.0.1:27017")
    MongoClient(s"mongodb://$uri")
  }

  lazy val isMongoRunning: Boolean =
    try {
      val p = Promise[List[String]]()

      mongo.listDatabaseNames().subscribe(new Observer[String]() {
        val buffer = mutable.ListBuffer.empty[String]

        override def onNext(result: String): Unit = buffer += result
        override def onError(e: Throwable): Unit = p.failure(e)
        override def onComplete(): Unit = p.success(buffer.toList)
      })

      // this will throw an exception if it can't connect to the db
      Await.result(p.future, Duration(2000, MILLISECONDS))
      true
    } catch {
      case _: TimeoutException =>
        false
    }
}

trait MongoScalaAsyncTestKit extends Specification with BeforeAfterEach {
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
      MongoScalaAsync.defineDb(id, TestMongoScalaAsync.mongo.getDatabase(db))
      MongoDB.defineDb(id, TestMongo.mongo, db)
    }
  }

  def checkMongoIsRunning = {
    TestMongoScalaAsync.isMongoRunning must beEqualTo(true).orSkip
    TestMongo.isMongoRunning must beEqualTo(true).orSkip
  }

  def after = {
    if (!debug && TestMongoScalaAsync.isMongoRunning) {
      // drop the databases
      dbs.foreach { case (id, _) =>
        MongoScalaAsync.use(id) { db =>
          db.drop().subscribe(new Observer[Completed]() {
            override def onNext(result: Completed): Unit = { }
            override def onError(e: Throwable): Unit = { }
            override def onComplete(): Unit = { }
          })
        }
      }
    }

    // clear the mongo instances
    dbs.foreach { case (id, _) =>
      MongoScalaAsync.remove(id)
    }

    if (!debug && TestMongo.isMongoRunning) {
      // drop the databases
      dbs.foreach { case (id, _) =>
        MongoDB.useDatabase(id) { db => db.drop() }
      }
    }

    // clear the mongo instances
    dbs.foreach { case (id, _) =>
      MongoDB.remove(id)
    }
  }
}
