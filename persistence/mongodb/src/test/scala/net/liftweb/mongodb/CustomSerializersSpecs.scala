/*
 * Copyright 2010 WorldWide Conferencing, LLC
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

package net.liftweb {
package mongodb {

import json.JsonAST._

import java.util.{Calendar, Date, TimeZone}

import org.bson.types.ObjectId
import org.specs.Specification
import org.specs.runner.JUnit4

class CustomSerializersSpecsTest extends JUnit4(CustomSerializersSpecs)

package customserializersspecs {

  /*
  * Date as String
  */
  case class Person(_id: String, birthDate: Date)
    extends MongoDocument[Person]
  {
    def meta = Person
  }
  object Person extends MongoDocumentMeta[Person]

  /*
  * Date as Date
  */
  case class Person2(_id: ObjectId, birthDate: Date) extends MongoDocument[Person2] {
    def meta = Person2
  }
  object Person2 extends MongoDocumentMeta[Person2] {
    override def formats = allFormats
  }
}

object CustomSerializersSpecs extends Specification with MongoTestKit {

  import customserializersspecs._

  val utc = TimeZone.getTimeZone("UTC")

  "CustomSerializers" should {
    "handle Date as String value" in {
      checkMongoIsRunning

      // test data
      val bdjack = Calendar.getInstance
      bdjack.setTimeZone(utc)
      bdjack.setTimeInMillis(1288742280000L)
      val jack = Person(ObjectId.get.toString, bdjack.getTime)

      // save the Person document
      jack.save

      // retrieve it and compare
      val jack2 = Person.find(jack._id)
      jack2 must notBeEmpty
      jack2 foreach { j =>
        j._id mustEqual jack._id
        j.birthDate mustEqual jack.birthDate
      }
    }

    "handle Date as Date value using DateSerializer" in {
      checkMongoIsRunning

      // test data
      val bdjack = Calendar.getInstance
      bdjack.setTimeZone(utc)
      bdjack.setTimeInMillis(1288742280000L)
      val jack = Person2(ObjectId.get, bdjack.getTime)

      // save the Person document
      jack.save

      // retrieve it and compare
      val findJack = Person2.find(jack._id)
      findJack must notBeEmpty
      findJack foreach { j =>
        j._id mustEqual jack._id
        j.birthDate mustEqual jack.birthDate
      }
    }
  }
}


}
}
