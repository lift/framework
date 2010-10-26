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

import net.liftweb.common.{Failure, Full, Empty}
import net.liftweb.json.Implicits.{int2jvalue, string2jvalue}
import net.liftweb.json.JsonAST.{JField, JInt, JObject, JString, JNull, render}
import net.liftweb.json.JsonDSL.{jobject2assoc, pair2Assoc, pair2jvalue}
import net.liftweb.json.Printer.compact
import net.liftweb.record.field.{IntField, OptionalStringField, StringField}
import org.specs._
import org.specs.runner.JUnit4
import DocumentHelpers.jobjectToJObjectExtension

class JSONRecordTestSpecsAsTest extends JUnit4(JSONRecordTestSpecs)

package jsontestrecords {
  class Person private () extends JSONRecord[Person] {
    def meta = Person

    object name extends StringField(this, 200)
    object age extends IntField(this) {
      override def defaultValue = 0
    }
    object favoriteColor extends OptionalStringField(this, 200)
    object address extends JSONSubRecordField(this, Address, Empty) {
      override def optional_? = true
      
    }
  }

  object Person extends Person with JSONMetaRecord[Person]
  
  class Address extends JSONRecord[Address] {
	  def meta = Address
	
	//  object country extends CountryField(this)
	//  object postalCode extends PostalCodeField(this, country)
	  object country extends StringField(this, 60)
	  object postalCode extends StringField(this, 10)
	  object city extends StringField(this, 60)
	  object street extends StringField(this, 200)
	}
	
	object Address extends Address with JSONMetaRecord[Address]
}

object JSONRecordTestSpecs extends Specification {
  import jsontestrecords._

  def assertEqualPerson(a: Person, b: Person) = {
    a.name.valueBox must_== b.name.valueBox
    a.age.valueBox must_== b.age.valueBox
    a.favoriteColor.valueBox must_== b.favoriteColor.valueBox
    a.address.valueBox.isEmpty must_== b.address.valueBox.isEmpty
    for (aa <- a.address.valueBox ; ba <- b.address.valueBox) {
	  aa.country.valueBox must_== ba.country.valueBox
	  aa.postalCode.valueBox must_== aa.postalCode.valueBox
	  aa.city.valueBox must_== aa.city.valueBox
	  aa.street.valueBox must_== aa.street.valueBox
    }
  }
    
  "A JSON record" should {
    def testRec1: Person = Person.createRecord.name("Alice").age(25)
    val testDoc1: JObject = ("age" -> 25) ~ ("name" -> "Alice")
    def testRec2: Person = Person.createRecord.name("Bob").age(30)
    val testDoc2: JObject = ("age" -> 30) ~ ("extra1" -> "value1") ~ ("extra2" -> "value2") ~ ("favoriteColor" -> "blue") ~ ("name" -> "Bob")
    def testRec3: Person = Person.createRecord.name("Charlie").age(0)
    val testDoc3: JObject = ("name" -> "Bob")
    def testRec4: Person = Person.createRecord.name("Max").age(25).address(Address.createRecord.street("my street").country("France").city("Paris").postalCode("75001"))
    val testDoc4: JObject = ("address" ->  ("city" -> "Paris") ~ ("country" -> "France") ~ ("postalCode" -> "75001") ~ ("street" -> "my street")) ~ ("age" -> 25) ~ ("name" -> "Max")

    "encode basic records correctly" in {
      compact(render(testRec1.asJValue)) must_== compact(render(testDoc1))
    }

    "encode record with subrecord correctly" in {
      compact(render(testRec4.asJValue)) must_== compact(render(testDoc4))
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

    "support set subRecord field" in {
      val recBox = Person.fromJValue(testDoc4)
      recBox must verify (_.isDefined)
      val Full(rec) = recBox

      rec.address.valueBox.flatMap(_.street.valueBox) must_== Full("my street")
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
