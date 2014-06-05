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

import org.specs2.mutable.Specification
import org.specs2.specification.BeforeAfterExample

import com.mongodb.MongoClient

trait MongoTestKit extends Specification with BeforeAfterExample {
  sequential

  def dbName = "lift_"+this.getClass.getName
    .replace("$", "")
    .replace("net.liftweb.mongodb.", "")
    .replace(".", "_")
    .toLowerCase

  def mongo = new MongoClient("127.0.0.1", 27017)

  // If you need more than one db, override this
  def dbs: List[(ConnectionIdentifier, String)] = List((DefaultConnectionIdentifier, dbName))

  def debug = false

  def before = {
    // define the dbs
    dbs foreach { case (id, db) =>
      MongoDB.defineDb(id, mongo, db)
    }
  }

  def isMongoRunning: Boolean =
    try {
      if (dbs.length < 1)
        false
      else {
        dbs foreach { case (id, _) =>
          MongoDB.use(id) ( db => { db.getLastError } )
        }
        true
      }
    } catch {
      case e: Exception => false
    }

  def checkMongoIsRunning = isMongoRunning must beEqualTo(true).orSkip

  def after = {
    if (!debug && isMongoRunning) {
      // drop the databases
      dbs foreach { case (id, _) =>
        MongoDB.use(id) { db => db.dropDatabase }
      }
    }

    // clear the mongo instances
    MongoDB.closeAll()
  }
}

