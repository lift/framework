/*
 * Copyright 2011-2014 WorldWide Conferencing, LLC
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

import BsonDSL._
import json.JObject

import java.util.{Calendar, Date, UUID}
import java.util.regex.Pattern

import org.bson.types.ObjectId
import org.specs2.mutable.Specification

package queryexamplesfixtures {
  case class Person(_id: ObjectId, name: String, birthDate: Date, childId: UUID, petId: Option[ObjectId]) extends MongoDocument[Person] {
    def meta = Person
  }
  object Person extends MongoDocumentMeta[Person] {
    override def formats = allFormats
    // index name
    createIndex(("name" -> 1))

    // implicit formats already exists
    def findAllBornAfter(dt: Date) = findAll(("birthDate" -> ("$gt" -> dt)))
  }
}

object QueryExamplesSpec extends Specification with MongoTestKit {
  "QueryExamples Specification".title

  import queryexamplesfixtures._

  "Query examples" in {
    checkMongoIsRunning

    val fredsBirthDate = Calendar.getInstance
    fredsBirthDate.set(1970, 1, 1, 19, 0)

    val wilmasBirthDate = Calendar.getInstance
    wilmasBirthDate.set(1971, 8, 30, 19, 0)

    val barneysBirthDate = Calendar.getInstance
    barneysBirthDate.set(1972, 8, 30, 19, 0)

    val bettysBirthDate = Calendar.getInstance
    bettysBirthDate.set(1973, 8, 30, 19, 0)

    val dinoId = ObjectId.get
    val pebblesId = UUID.randomUUID
    val bammbammId = UUID.randomUUID

    val fred = Person(ObjectId.get, "Flinstone, Fred", fredsBirthDate.getTime, pebblesId, Some(dinoId))
    val wilma = Person(ObjectId.get, "Flinstone, Wilma", wilmasBirthDate.getTime, pebblesId, Some(dinoId))
    val barney = Person(ObjectId.get, "Rubble, Barney", barneysBirthDate.getTime, bammbammId, None)
    val betty = Person(ObjectId.get, "Rubble, Betty", bettysBirthDate.getTime, bammbammId, None)

    fred.save
    wilma.save
    barney.save
    betty.save

    val flinstonesIds = List(fred._id, wilma._id)
    val rubblesIds = List(barney._id, betty._id)

    // query for Bamm-Bamm's parents (UUID) by childId
    val pebblesParents = Person.findAll(("childId" -> bammbammId))

    pebblesParents.length must_== 2
    pebblesParents.map(_._id).filterNot(rubblesIds.contains(_)) must_== List()

    // query for Bamm-Bamm's and Pebbles' parents using List[UUID]
    val pebblesAndBammBammsParents = Person.findAll(("childId" -> ("$in" -> List(pebblesId, bammbammId))))

    pebblesAndBammBammsParents.length must_== 4

    // query for Dino's owners (ObjectId)
    val dinosOwners = Person.findAll(("petId" -> dinoId))

    dinosOwners.length must_== 2
    dinosOwners.map(_._id).filterNot(flinstonesIds.contains(_)) must_== List()

    // query for the Rubbles using a Regex
    val rubbles = Person.findAll(("name" -> "^Rubble".r))

    rubbles.length must_== 2
    rubbles.map(_._id).filterNot(rubblesIds.contains(_)) must_== List()

    // query for the Flinstones using a Pattern
    val flinstones = Person.findAll(("name" -> Pattern.compile("^flinst", Pattern.CASE_INSENSITIVE)))

    flinstones.length must_== 2
    flinstones.map(_._id).filterNot(flinstonesIds.contains(_)) must_== List()

    // query for the Flinstones using a List[ObjectId]
    val flinstones2 = Person.findAll(("_id" -> ("$in" -> flinstonesIds)))

    flinstones2.length must_== 2
    flinstones2.map(_._id).filterNot(flinstonesIds.contains(_)) must_== List()

    // query using Dates
    implicit val formats = Person.formats // this is needed for Dates
    val qryDate = Calendar.getInstance
    qryDate.set(1971, 1, 1, 19, 0)
    val people = Person.findAll(("birthDate" -> ("$gt" -> qryDate.getTime)))

    people.length must_== 3
    people.map(_._id).filterNot(List(wilma._id, barney._id, betty._id).contains(_)) must_== List()

    // you do not need to define the implicit formats val if you write your query in the DocumentMeta object.
    val people2 = Person.findAllBornAfter(qryDate.getTime)

    people2.length must_== 3
    people2.map(_._id).filterNot(List(wilma._id, barney._id, betty._id).contains(_)) must_== List()

    // query all with Sort
    val people3 = Person.findAll(JObject(Nil), ("birthDate" -> -1))

    people3.length must_== 4
    people3.map(_._id) must_== List(betty._id, barney._id, wilma._id, fred._id)

    val people4 = Person.findAll(JObject(Nil), ("birthDate" -> 1))

    people4.length must_== 4
    people4.map(_._id) must_== List(fred._id, wilma._id, barney._id, betty._id)
  }
}
