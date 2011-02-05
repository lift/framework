/*
 * Copyright 2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package couchdb {

import _root_.java.net.ConnectException
import _root_.dispatch.{Http, StatusCode}
import _root_.net.liftweb.common.{Box, Empty, Full}
import _root_.net.liftweb.util.ControlHelpers.tryo
import _root_.org.specs._
import _root_.org.specs.runner.JUnit4

class DatabaseTestSpecsAsTest extends JUnit4(DatabaseTestSpecs)

object DatabaseTestSpecs extends Specification {
  def setup = {
    val http = new Http
    val database = new Database("test")
    (try { http(database delete) } catch { case StatusCode(_, _) => () }) must not(throwAnException[ConnectException]).orSkipExample

    (http, database)
  }

  "A database" should {
    "give 404 when info called and nonexistant" in {
      setup
      val (http, database) = setup

      http(database info) must throwAnException[StatusCode].like { case StatusCode(404, _) => true }
    }

    "give 404 when deleted but nonexistant" in {
      val (http, database) = setup

      http(database delete) must throwAnException[StatusCode].like { case StatusCode(404, _) => true }
    }

    "succeed being created" in {
      val (http, database) = setup

      http(database create) must_== ()
    }

    "give 412 instead of allowing creation when already existant" in {
      val (http, database) = setup

      http(database create) must_== ()
      http(database create) must throwAnException[StatusCode].like { case StatusCode(412, _) => true }
    }
    
    "have info when created" in {
      val (http, database) = setup

      http(database create) must_== ()
      http(database info).db_name must_== ("test")
    }

    "succeed in being deleted" in {
      val (http, database) = setup

      http(database create) must_== () 
      http(database delete) must_== ()
    }

    "succeed being recreated" in {
      val (http, database) = setup

      http(database create) must_== () 
      http(database delete) must_== ()
      http(database create) must_== () 
    }
  }
}

}
}
