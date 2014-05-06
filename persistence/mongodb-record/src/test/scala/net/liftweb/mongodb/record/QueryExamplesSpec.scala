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
package record

import BsonDSL._
import json.JObject
import field._
import net.liftweb.record.field._

import java.util.{Calendar, Date, UUID}
import java.util.regex.Pattern

import org.bson.types.ObjectId
import org.specs2.mutable.Specification

package queryexamplesfixtures {
  class Person private () extends MongoRecord[Person] with ObjectIdPk[Person] {
    def meta = Person

    object name extends StringField(this, 100)
    object birthDate extends DateField(this)
    object childId extends UUIDField(this)
    object petId extends ObjectIdField(this) {
      override def optional_? = true
    }
  }
  object Person extends Person with MongoMetaRecord[Person] {
    // index name
    ensureIndex(("name" -> 1))

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

    val fred = Person.createRecord
      .name("Flinstone, Fred")
      .birthDate(fredsBirthDate.getTime)
      .childId(pebblesId)
      .petId(dinoId)
      .save()
    val wilma = Person.createRecord
      .name("Flinstone, Wilma")
      .birthDate(wilmasBirthDate.getTime)
      .childId(pebblesId)
      .petId(dinoId)
      .save()
    val barney = Person.createRecord
      .name("Rubble, Barney")
      .birthDate(barneysBirthDate.getTime)
      .childId(bammbammId)
      .save()
    val betty = Person.createRecord
      .name("Rubble, Betty")
      .birthDate(bettysBirthDate.getTime)
      .childId(bammbammId)
      .save()

    val flinstonesIds = List(fred.id.get, wilma.id.get)
    val rubblesIds = List(barney.id.get, betty.id.get)

    // query for Bamm-Bamm's parents (UUID)
    val pebblesParents = Person.findAll(("childId" -> bammbammId))

    pebblesParents.length must_== 2
    pebblesParents.map(_.id.get).filterNot(rubblesIds.contains(_)) must_== List()

    // query for Bamm-Bamm's and Pebbles' parents using List[UUID]
    val pebblesAndBammBammsParents = Person.findAll(("childId" -> ("$in" -> List(pebblesId, bammbammId))))

    pebblesAndBammBammsParents.length must_== 4

    // query for Dino's owners (ObjectId)
    val dinosOwners = Person.findAll(("petId" -> dinoId))

    dinosOwners.length must_== 2
    dinosOwners.map(_.id.get).filterNot(flinstonesIds.contains(_)) must_== List()

    // query for the Rubbles using a Regex
    val rubbles = Person.findAll(("name" -> "^Rubble".r))

    rubbles.length must_== 2
    rubbles.map(_.id.get).filterNot(rubblesIds.contains(_)) must_== List()

    // query for the Flinstones using a Pattern
    val flinstones = Person.findAll(("name" -> Pattern.compile("^flinst", Pattern.CASE_INSENSITIVE)))

    flinstones.length must_== 2
    flinstones.map(_.id.get).filterNot(flinstonesIds.contains(_)) must_== List()

    // query for the Flinstones using a List[ObjectId]
    val flinstones2 = Person.findAll(("_id" -> ("$in" -> flinstonesIds)))

    flinstones2.length must_== 2
    flinstones2.map(_.id.get).filterNot(flinstonesIds.contains(_)) must_== List()

    // query using Dates
    implicit val formats = Person.formats // this is needed for Dates
    val qryDate = Calendar.getInstance
    qryDate.set(1971, 1, 1, 19, 0)
    val people = Person.findAll(("birthDate" -> ("$gt" -> qryDate.getTime)))

    people.length must_== 3
    people.map(_.id.get).filterNot(List(wilma.id.get, barney.id.get, betty.id.get).contains(_)) must_== List()

    // you do not need to define the implicit formats val if you write your query in the MongoMetaRecord object.
    val people2 = Person.findAllBornAfter(qryDate.getTime)

    people2.length must_== 3
    people2.map(_.id.get).filterNot(List(wilma.id.get, barney.id.get, betty.id.get).contains(_)) must_== List()

    // query with Sort
    val people3 = Person.findAll(JObject(Nil), ("birthDate" -> -1))

    people3.length must_== 4
    people3.map(_.id.get) must_== List(betty.id.get, barney.id.get, wilma.id.get, fred.id.get)

    val people4 = Person.findAll(JObject(Nil), ("birthDate" -> 1))

    people4.length must_== 4
    people4.map(_.id.get) must_== List(fred.id.get, wilma.id.get, barney.id.get, betty.id.get)
  }
}
