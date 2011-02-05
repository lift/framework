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
import _root_.net.liftweb.common.{Box, Full}
import _root_.net.liftweb.json.Implicits._
import _root_.net.liftweb.json.JsonAST.{JObject, JString, render}
import _root_.net.liftweb.json.JsonDSL._
import _root_.net.liftweb.json.Printer.compact
import _root_.org.specs._
import _root_.org.specs.runner.JUnit4
import DocumentHelpers.{forceStore, jobjectToJObjectExtension, stripIdAndRev, updateIdAndRev}

class DocumentTestSpecsAsTest extends JUnit4(DocumentTestSpecs)

object DocumentTestSpecs extends Specification {
  def setup = {
    val http = new Http
    val database = new Database("test")
    (try { http(database delete) } catch { case StatusCode(_, _) => () }) must not(throwAnException[ConnectException]).orSkipExample
    http(database create)

    (http, database)
  }

  private final def verifyAndOpen[A](b: Box[A]): A = {
    b must verify(_.isDefined)
    b.open_!
  }

  "A document" should {
    val testDoc1: JObject = ("name" -> "Alice") ~ ("age" -> 25)
    val testDoc2: JObject = ("name" -> "Bob") ~ ("age" -> 30)

    "give 404 on get when nonexistant" in {
      val (http, database) = setup

      http(database("testdoc") fetch) must throwAnException[StatusCode].like { case StatusCode(404, _) => true }
    }

    "be insertable" in {
      val (http, database) = setup

      val newDoc = verifyAndOpen(http(database post testDoc1))
      val Full(id) = newDoc._id
      val Full(rev) = newDoc._rev
      compact(render(stripIdAndRev(newDoc))) must_== compact(render(testDoc1))
      val dbDoc = http(database(newDoc) fetch)
      compact(render(dbDoc)) must_== compact(render(newDoc))
    }

    "have history" in {
      val (http, database) = setup

      val firstDocBox = http(database post testDoc1)
      firstDocBox must verify(_.isDefined)
      val Full(firstDoc) = firstDocBox
      val Full(id) = firstDoc._id
      val Full(rev) = firstDoc._rev
      val secondDoc = verifyAndOpen(http(database store updateIdAndRev(testDoc2, id, rev)))
      val dbFirstDoc = http(database(id) @@ firstDoc fetch)
      val dbSecondDoc = http(database(id) @@ secondDoc fetch)
      val dbCurrentDoc = http(database(id) fetch)

      compact(render(dbFirstDoc)) must_== compact(render(firstDoc))
      compact(render(dbSecondDoc)) must_== compact(render(secondDoc))
      compact(render(dbCurrentDoc)) must_== compact(render(secondDoc))
    }

    "be deletable" in {
      val (http, database) = setup

      val newDoc = verifyAndOpen(http(database store testDoc1))
      http(database(newDoc) @@ newDoc delete) must be ()
      http(database(newDoc) fetch) must throwAnException[StatusCode].like { case StatusCode(404, _) => true }
    }

    "give 404 on delete when nonexistant" in {
      val (http, database) = setup

      val newDoc = verifyAndOpen(http(database store testDoc1))
      http(database(newDoc) @@ newDoc delete) must be ()
      http(database(newDoc) @@ newDoc delete) must throwAnException[StatusCode].like { case StatusCode(404, _) => true }
    }

    "be force storable" in {
      val (http, database) = setup

      val doc = ("_id" -> "test") ~ testDoc1
      stripIdAndRev(verifyAndOpen(forceStore(http, database, doc))) must_== testDoc1
      stripIdAndRev(verifyAndOpen(forceStore(http, database, doc))) must_== testDoc1
    }
  }
}

}
}
