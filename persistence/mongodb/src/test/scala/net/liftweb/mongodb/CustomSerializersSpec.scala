/*
 * Copyright 2010-2011 WorldWide Conferencing, LLC
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

import java.util.{Calendar, Date, TimeZone, UUID}
import java.util.regex.Pattern

import org.bson.types.ObjectId

import org.joda.time.{Instant, DateTime}

import org.specs2.mutable.Specification


package customserializersspecs {

  /*
  * ObjectId as String
  */
  case class Person(_id: String)
    extends MongoDocument[Person]
  {
    def meta = Person
  }
  object Person extends MongoDocumentMeta[Person]

  /*
  * ObjectId as ObjectId
  */
  case class PersonWithObjectId(_id: ObjectId)
    extends MongoDocument[PersonWithObjectId]
  {
    def meta = PersonWithObjectId
  }
  object PersonWithObjectId extends MongoDocumentMeta[PersonWithObjectId] {
    override def formats = allFormats
  }

  /*
   * Pattern as Pattern
   */
  case class PersonWithPattern(_id:ObjectId, pattern: Pattern) extends MongoDocument[PersonWithPattern] {
    def meta = PersonWithPattern
  }
  object PersonWithPattern extends MongoDocumentMeta[PersonWithPattern] {
    override def formats = allFormats
  }

  /*
  * Date as Date
  */
  case class PersonWithDate(_id: ObjectId, birthDate: Date) extends MongoDocument[PersonWithDate] {
    def meta = PersonWithDate
  }
  object PersonWithDate extends MongoDocumentMeta[PersonWithDate] {
    override def formats = allFormats
  }

  /*
   * DateTime as DateTime
   */
  case class PersonWithDateTime(_id: ObjectId, birthDate: DateTime) extends MongoDocument[PersonWithDateTime] {
    def meta = PersonWithDateTime
  }
  object PersonWithDateTime extends MongoDocumentMeta[PersonWithDateTime] {
    override def formats = allFormats
  }

  /*
   * UUID as UUID
   */
  case class PersonWithUUID(_id: UUID) extends MongoDocument[PersonWithUUID] {
    def meta = PersonWithUUID
  }
  object PersonWithUUID extends MongoDocumentMeta[PersonWithUUID] {
    override def formats = allFormats
  }
}


/**
 * Systems under specification for CustomSerializers.
 */
class CustomSerializersSpec extends Specification with MongoTestKit {
  "CustomSerializers Specification".title

  import customserializersspecs._

  val utc = TimeZone.getTimeZone("UTC")

  "CustomSerializers" should {
    "handle ObjectId as String value" in {
      checkMongoIsRunning

      // test data
      val jack = Person(ObjectId.get.toString)

      // save the Person document
      jack.save

      // retrieve it and compare
      Person.find(jack._id) must beLike {
        case Some(j) =>
          j._id mustEqual jack._id
      }
    }

    "handle ObjectId as ObjectId value using ObjectIdSerializer" in {
      checkMongoIsRunning

      // test data
      val jack = PersonWithObjectId(ObjectId.get)

      // save the PersonWithObjectId document
      jack.save

      // retrieve it and compare
      PersonWithObjectId.find(jack._id) must beLike {
        case Some(j) =>
          j._id mustEqual jack._id
      }
    }

    "handle Pattern as Pattern value using PatternSerializer" in {
      checkMongoIsRunning

      // test data
      val pattern = Pattern.compile("(?idmsux-idmsux)m(a)gi(?:ic)?[a-zA-Z]+boom")
      val jack = PersonWithPattern(ObjectId.get, pattern)

      // save the PersonWithPattern document
      jack.save

      // retrieve it and compare
      PersonWithPattern.find(jack._id) must beLike {
        case Some(j) =>
          j.pattern.pattern mustEqual jack.pattern.pattern
          j.pattern.flags mustEqual jack.pattern.flags
      }
    }

    "handle DateTime as DateTime value using DateTimeSerializer" in {
      checkMongoIsRunning

      // test data
      val birthday = (new Instant(1288742280000L)).toDateTime
      val jack = PersonWithDateTime(ObjectId.get, birthday)

      // save the Person document
      jack.save

      // retrieve it and compare
      PersonWithDateTime.find(jack._id) must beLike {
        case Some(j) =>
          j.birthDate mustEqual jack.birthDate
      }
    }

    "handle Date as Date value using DateSerializer" in {
      checkMongoIsRunning

      // test data
      val bdjack = Calendar.getInstance
      bdjack.setTimeZone(utc)
      bdjack.setTimeInMillis(1288742280000L)
      val jack = PersonWithDate(ObjectId.get, bdjack.getTime)

      // save the Person document
      jack.save

      // retrieve it and compare
      PersonWithDate.find(jack._id) must beLike {
        case Some(j) =>
          j.birthDate mustEqual jack.birthDate
      }
    }

    "handle UUID as UUID value using UUIDSerializer" in {
      checkMongoIsRunning

      // test data
      val uuid = UUID.randomUUID
      val jack = PersonWithUUID(uuid)

      // save the Person document
      jack.save

      // retrieve it and compare
      PersonWithUUID.find(jack._id) must beLike {
        case Some(j) =>
          j._id mustEqual jack._id
      }
    }
  }
}
