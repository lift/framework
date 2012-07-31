/*
 * Copyright 2011 WorldWide Conferencing, LLC
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

import org.specs2.mutable.Specification
import org.specs2.execute.Result

import com.mongodb._

class MongoSpec extends Specification  {
  "Mongo Specification".title

  case object TestMongoIdentifier extends MongoIdentifier {
    val jndiName = "test_a"
  }

  def passDefinitionTests(id: MongoIdentifier, ma: MongoAddress): Result = {
    // define the db
    MongoDB.close
    MongoDB.defineDb(id, ma)

    // make sure mongo is running
    try {
      MongoDB.use(id) { db =>
        db.getLastError.ok must beEqualTo(true)
      }
    }
    catch {
      case e: Exception => skipped("MongoDB is not running")
    }

    // using an undefined identifier throws an exception
    MongoDB.use(DefaultMongoIdentifier) { db =>
      db.getLastError.ok must beEqualTo(true)
    } must throwA(new MongoException("Mongo not found: MongoIdentifier(test)"))
    // remove defined db
    MongoDB.close
    success
  }

  "Mongo" should {
    "Define DB with default host and port" in {
      passDefinitionTests(TestMongoIdentifier, MongoAddress(MongoHost(), "test_default"))
    }
    "Define DB with default port" in {
      passDefinitionTests(TestMongoIdentifier, MongoAddress(MongoHost("127.0.0.1"), "test_default"))
    }
    "Define DB with specified host and port" in {
      passDefinitionTests(TestMongoIdentifier, MongoAddress(MongoHost("127.0.0.1", 27017), "test_default"))
    }
    "Define DB with ServerAddress and MongoOptions" in {
      passDefinitionTests(TestMongoIdentifier, MongoAddress(MongoHost(new ServerAddress, new MongoOptions), "test_default"))
    }
    "Define DB with ServerAddress" in {
      passDefinitionTests(TestMongoIdentifier, MongoAddress(MongoHost(new ServerAddress), "test_default"))
    }
    "Define DB with MongoOptions" in {
      val mo = new MongoOptions
      mo.connectionsPerHost = 12
      passDefinitionTests(TestMongoIdentifier, MongoAddress(MongoHost(options=mo), "test_default"))
    }
    /* These need all of the Mongo instances to be running to be useful.
    "Define DB pair with DBAddress" in {
      val dba = new DBAddress("127.0.0.1", 27017, "test_a")
      val dbb = new DBAddress("127.0.0.1", 27018, "test_b")
      passDefinitionTests(TestMongoIdentifier, MongoAddress(MongoPair(dba, dbb), "test_default"))
    }
    "Define DB pair with ServerAddress" in {
      val dba = new ServerAddress("127.0.0.1", 27017)
      val dbb = new ServerAddress("127.0.0.1", 27018)
      passDefinitionTests(TestMongoIdentifier, MongoAddress(MongoPair(dba, dbb), "test_default"))
    }
    "Define DB set with ServerAddress" in {
      val dba = new ServerAddress("127.0.0.1", 27017)
      val dbb = new ServerAddress("127.0.0.1", 27018)
      val dbc = new ServerAddress("127.0.0.1", 27019)
      passDefinitionTests(TestMongoIdentifier, MongoAddress(MongoSet(List(dba, dbb, dbc)), "test_default"))
    }
    */
    "Define DB with Mongo instance" in {
      // define the db
      MongoDB.close
      MongoDB.defineDb(TestMongoIdentifier, new Mongo, "test_default")

      // make sure mongo is running
      try {
        MongoDB.use(TestMongoIdentifier) { db =>
          db.getLastError.ok must beEqualTo(true)
        }
      }
      catch {
        case e: Exception => skipped("MongoDB is not running")
      }

      // using an undefined identifier throws an exception
      MongoDB.use(DefaultMongoIdentifier) { db =>
        db.getLastError.ok must beEqualTo(true)
      } must throwA(new MongoException("Mongo not found: MongoIdentifier(test)"))
      // remove defined db
      MongoDB.close
      success
    }

    /* Requires a server other than localhost with auth setup.
    "Define and authenticate DB with Mongo instance" in {
      MongoDB.close

      // make sure mongo is running
      try {
        val pwd = "lift_pwd"
        val dbUri = new MongoURI("mongodb://")
        // define the db
        MongoDB.defineDbAuth(TestMongoIdentifier, new Mongo(dbUri), "lift_auth_test", "lift_user", pwd)
        // try to use it
        MongoDB.use(TestMongoIdentifier) { db =>
          db.getLastError.ok must beEqualTo(true)
        }
      }
      catch {
        case e: Exception => skip("MongoDB is not running")
      }
      // remove defined db
      MongoDB.close
    }
    */
  }
}
