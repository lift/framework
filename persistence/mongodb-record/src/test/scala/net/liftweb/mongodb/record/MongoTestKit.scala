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
package record

import util.{ConnectionIdentifier, DefaultConnectionIdentifier, Props}


import org.specs2.mutable.Specification
import org.specs2.specification.BeforeAfterEach

import com.mongodb._

// The sole mongo object for testing
object TestMongo {
  val mongo = {
    val uri = Props.get("mongo.test.uri", "127.0.0.1:27017")
    val opts = MongoClientOptions.builder.serverSelectionTimeout(2000)
    new MongoClient(new MongoClientURI(s"mongodb://$uri", opts))
  }

  lazy val isMongoRunning: Boolean =
    try {
      // this will throw an exception if it can't connect to the db
      mongo.listDatabases
      true
    } catch {
      case _: MongoTimeoutException =>
        false
    }
}

trait MongoTestKit extends Specification with BeforeAfterEach {
  sequential

  def dbName = "lift_record_"+this.getClass.getName
    .replace("$", "")
    .replace("net.liftweb.mongodb.record.", "")
    .replace(".", "_")
    .toLowerCase

  // If you need more than one db, override this
  def dbs: List[(ConnectionIdentifier, String)] =
    (DefaultConnectionIdentifier, dbName) :: Nil

  def debug = false

  def before = {
    // define the dbs
    dbs.foreach { case (id, db) =>
      MongoDB.defineDb(id, TestMongo.mongo, db)
    }
  }

  def checkMongoIsRunning = {
    TestMongo.isMongoRunning must beEqualTo(true).orSkip
  }

  def after = {
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

