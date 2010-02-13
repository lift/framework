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
import _root_.net.liftweb.json.JsonAST.{JArray, JObject, JValue, render}
import _root_.net.liftweb.json.JsonDSL._
import _root_.net.liftweb.json.Printer.compact
import _root_.org.specs._
import _root_.org.specs.runner.JUnit4
import DocumentHelpers.jobjectToJObjectExtension

class QueryTestSpecsAsTest extends JUnit4(QueryTestSpecs)

object QueryTestSpecs extends Specification {
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

  "Queries" should {
    val design: JObject = 
      ("language" -> "javascript") ~
      ("views" -> (("all_students"  -> ("map" -> "function(doc) { if (doc.type == 'student') { emit(null, doc); } }")) ~
             ("students_by_age" -> ("map" -> "function(doc) { if (doc.type == 'student') { emit(doc.age, doc); } }")) ~
             ("students_by_age_and_name" -> ("map" -> "function(doc) { if (doc.type == 'student') { emit([doc.age, doc.name], doc); } }"))))

    val docs: List[JObject] =
      (("type" -> "student") ~ ("name" -> "Alice")   ~ ("age" -> 10)) :: 
      (("type" -> "student") ~ ("name" -> "Bob")   ~ ("age" -> 11)) :: 
      (("type" -> "student") ~ ("name" -> "Charlie") ~ ("age" -> 11)) :: 
      (("type" -> "student") ~ ("name" -> "Donna")   ~ ("age" -> 12)) :: 
      (("type" -> "student") ~ ("name" -> "Eric")  ~ ("age" -> 12)) :: 
      (("type" -> "student") ~ ("name" -> "Fran")  ~ ("age" -> 13)) :: 
      (("type" -> "class") ~ ("name" -> "Astronomy")) ::
      (("type" -> "class") ~ ("name" -> "Baking")) ::
      (("type" -> "class") ~ ("name" -> "Chemistry")) ::
      Nil

    def findStudents(docs: List[JObject]): List[JObject] = docs.filter(_.isA("student"))

    def compareName(a: JObject, b: JObject): Boolean = 
      (a.getString("name") openOr "design") < (b.getString("name") openOr "design")

    def prep(http: Http, database: Database): (JObject, List[JObject]) = {
      val storedDesign = verifyAndOpen(http(database.design("test") put design))
      val storedDocs = docs map { doc => verifyAndOpen(http(database store doc)) }

      (storedDesign, storedDocs)
    }

    def sortedAndPrintedRows(docs: Seq[QueryRow]): String = sortedAndPrintedValues(docs.flatMap(_.value.asA[JObject]).toList)
    def sortedAndPrintedValues(docs: List[JObject]): String = compact(render(docs.sort(compareName)))

    "work with all documents" in {
      val (http, database) = setup
      val (design, docs) = prep(http, database)
      val students = findStudents(docs)

      verifyAndOpen(http(database.all.includeDocs query)) must beLike {
        case QueryResults(Full(count), Full(offset), rows) =>
          count must_== docs.length + 1 // +1 for the design doc
          offset must_== 0
          sortedAndPrintedValues(rows.flatMap(_.doc).toList) must_== sortedAndPrintedValues(design::docs)
      }
    }

    "support multi-document fetch" in {
      val (http, database) = setup
      val (design, docs) = prep(http, database)
      val students = findStudents(docs)

      verifyAndOpen(http(database(List(students(0)._id.open_!, students(3)._id.open_!, students(5)._id.open_!)) query)) must beLike {
        case QueryResults(Full(count), Full(offset), rows) =>
          sortedAndPrintedValues(rows.flatMap(_.doc).toList) must_== sortedAndPrintedValues(students(0)::students(3)::students(5)::Nil)
      }
    }

    "work with views" in {
      val (http, database) = setup
      val (design, docs) = prep(http, database)
      val students = findStudents(docs)

      verifyAndOpen(http(database.design("test").view("all_students") query)) must beLike {
        case QueryResults(Full(count), Full(offset), rows) =>
          count must_== students.length
          offset must_== 0
          sortedAndPrintedRows(rows) must_== sortedAndPrintedValues(students)
      }
    }

    "support minimum key bounds" in {
      val (http, database) = setup
      val (design, docs) = prep(http, database)
      val students = findStudents(docs)

      verifyAndOpen(http(database.design("test").view("students_by_age").from(11) query)) must beLike {
        case QueryResults(_, _, rows) =>
          (rows.flatMap(_.value.asA[JObject]).toList.sort(compareName) must_==
           students.filter(_.getInt("age").map(_ >= 11).open_!).sort(compareName))
      }
    }

    "support maximum key bounds" in {
      val (http, database) = setup
      val (design, docs) = prep(http, database)
      val students = findStudents(docs)

      verifyAndOpen(http(database.design("test").view("students_by_age").to(12) query)) must beLike {
        case QueryResults(_, _, rows) =>
          (rows.flatMap(_.value.asA[JObject]).toList.sort(compareName) must_==
           students.filter(_.getInt("age").map(_ <= 12).open_!).sort(compareName))
      }
    }

    "support key lookup" in {
      val (http, database) = setup
      val (design, docs) = prep(http, database)
      val students = findStudents(docs)

      verifyAndOpen(http(database.design("test").view("students_by_age").key(11) query)) must beLike {
        case QueryResults(_, _, rows) =>
          rows.length must_== 2
          (rows.flatMap(_.value.asA[JObject]).toList.sort(compareName) must_==
           students.filter(_.getInt("age").map(_ == 11).open_!))
      }
    }

    "support limiting the number of results" in {
      val (http, database) = setup
      prep(http, database)

      verifyAndOpen(http(database.design("test").view("students_by_age").from(12).limit(2) query)) must beLike {
        case QueryResults(_, _, rows) => rows.length must_== 2
      }
    }

    "support descending sort" in {
      val (http, database) = setup
      val (design, docs) = prep(http, database)
      val students = findStudents(docs)

      verifyAndOpen(http(database.design("test").view("students_by_age").descending.from(11) query)) must beLike {
        case QueryResults(_, _, rows) =>
          rows.length must_== 3
          (rows.flatMap(_.value.asA[JObject]).toList.sort(compareName) must_==
           students.filter(_.getInt("age").map(_ <= 11).open_!))
      }
    }

    "preserve query ordering" in {
      val (http, database) = setup
      val (design, docs) = prep(http, database)
      val students = findStudents(docs)

      verifyAndOpen(http(database.design("test").view("students_by_age_and_name").from(JArray(11::Nil)).to(JArray(12::JObject(Nil)::Nil)) query)) must beLike {
        case QueryResults(_, _, rows) =>
          rows.length must_== 4
          (rows.flatMap(_.value.asA[JObject]).toList must_==
            students.filter(_.getInt("age").map(age => age >= 11 && age <= 12).open_!))
      }
    }
  }
}

}
}
