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
package couchdb {

import net.liftweb.common.{Failure, Full}
import net.liftweb.json.Implicits.{int2jvalue, string2jvalue}
import net.liftweb.json.JsonAST.{JField, JInt, JObject, JString, render}
import net.liftweb.json.JsonDSL.{jobject2assoc, pair2Assoc, pair2jvalue}
import net.liftweb.json.Printer.compact
import net.liftweb.record.field.{IntField, OptionalStringField, StringField}
import org.specs._
import org.specs.runner.JUnit4
import DocumentHelpers.jobjectToJObjectExtension

class JSONRecordTestSpecsAsTest extends JUnit4(JSONRecordTestSpecs)

package jsontestrecords {
  class Person extends JSONRecord[Person] {
    def meta = Person

    object name extends StringField(this, 200)
    object age extends IntField(this) {
      override def defaultValue = 0
    }
    object favoriteColor extends OptionalStringField(this, 200)
  }

  object Person extends Person with JSONMetaRecord[Person] {
    def createRecord = new Person
  }
}

object JSONRecordTestSpecs extends Specification {
  import jsontestrecords._

  def assertEqualPerson(a: Person, b: Person) = {
    a.name.valueBox must_== b.name.valueBox
    a.age.valueBox must_== b.age.valueBox
    a.age.valueBox must_== b.age.valueBox
  }
    
  "A JSON record" should {
    def testRec1: Person = Person.createRecord.name("Alice").age(25)
    val testDoc1: JObject = ("age" -> 25) ~ ("name" -> "Alice")
    def testRec2: Person = Person.createRecord.name("Bob").age(30)
    val testDoc2: JObject = ("age" -> 30) ~ ("extra1" -> "value1") ~ ("extra2" -> "value2") ~ ("favoriteColor" -> "blue") ~ ("name" -> "Bob")
    def testRec3: Person = Person.createRecord.name("Charlie").age(0)
    val testDoc3: JObject = ("name" -> "Bob")

    "encode basic records correctly" in {
      compact(render(testRec1.asJValue)) must_== compact(render(testDoc1))
    }

    "decode basic records correctly" in {
      val recBox = Person.fromJValue(testDoc1)
      recBox must verify (_.isDefined)
      val Full(rec) = recBox
      assertEqualPerson(rec, testRec1)
    }

    "preserve extra fields from JSON" in {
      val recBox = Person.fromJValue(testDoc2)
      recBox must verify (_.isDefined)
      val Full(rec) = recBox
      rec.additionalJFields must_== List(JField("extra1", JString("value1")), 
                                         JField("extra2", JString("value2")))
      rec.age.set(1)

      compact(render(rec.asJValue)) must_== compact(render(("age" -> 1) ~ testDoc2.remove("age")))
    }

    "support unset optional fields" in {
      val recBox = Person.fromJValue(testDoc1)
      recBox must verify (_.isDefined)
      val Full(rec) = recBox

      rec.favoriteColor.value must not (verify (_.isDefined))
    }

    "support set optional fields" in {
      val recBox = Person.fromJValue(testDoc2)
      recBox must verify (_.isDefined)
      val Full(rec) = recBox

      rec.favoriteColor.value must_== Some("blue")
    }

    "not set missing fields" in {
      val rec = Person.createRecord
      rec.age.set(123)
      JSONMetaRecord.overrideNeedAllJSONFields.doWith(false) { rec.setFieldsFromJValue(testDoc3) } must_== Full(())
      rec.age.value must_== 123
    }

    "honor overrideIgnoreExtraJSONFields == true" in {
      val recBox = JSONMetaRecord.overrideIgnoreExtraJSONFields.doWith(true) { Person.fromJValue(testDoc2) }
      recBox must verify (_.isDefined)
    }

    "honor overrideIgnoreExtraJSONFields == false" in {
      val recBox = JSONMetaRecord.overrideIgnoreExtraJSONFields.doWith(false) { Person.fromJValue(testDoc2) }
      recBox must not (verify (_.isDefined))
    }

    "honor overrideNeedAllJSONFields == true" in {
      val recBox = JSONMetaRecord.overrideNeedAllJSONFields.doWith(true) { Person.fromJValue(testDoc3) }
      recBox must not (verify (_.isDefined))
    }

    "honor overrideNeedAllJSONFields == false" in {
      val recBox = JSONMetaRecord.overrideNeedAllJSONFields.doWith(false) { Person.fromJValue(testDoc3) }
      recBox must verify (_.isDefined)
    }
  }
}

}
}
