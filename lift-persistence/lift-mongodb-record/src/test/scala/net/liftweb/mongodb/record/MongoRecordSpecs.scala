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
package record {

import field.{JsonObjectField, MongoJsonObjectListField}

import java.util.Date

import org.bson.types.ObjectId
import org.specs.Specification
import org.specs.runner.JUnit4

import net.liftweb.common._
import net.liftweb.json.DefaultFormats
import net.liftweb.json.JsonAST._
import net.liftweb.record.field.StringField

import com.mongodb._

class MongoRecordSpecsTest extends JUnit4(MongoRecordSpecs)

package mongorecordspecs {
  case class JsonObj(id: String, name: String) extends JsonObject[JsonObj] {
    def meta = JsonObj
  }
  object JsonObj extends JsonObjectMeta[JsonObj]

  class NullTestRec extends MongoRecord[NullTestRec] with MongoId[NullTestRec] {

    def meta = NullTestRec

    object nullstring extends StringField(this, 32) {
      override def optional_? = true
    }

    object jsonobj extends JsonObjectField[NullTestRec, JsonObj](this, JsonObj) {
      def defaultValue = JsonObj("1", null)
    }
    object jsonobjlist extends MongoJsonObjectListField[NullTestRec, JsonObj](this, JsonObj)
  }

  object NullTestRec extends NullTestRec with MongoMetaRecord[NullTestRec] {
    def createRecord = new NullTestRec
  }

  case class BoxTestJsonObj(id: String, boxEmpty: Box[String], boxFull: Box[String], boxFail: Box[String])
  extends JsonObject[BoxTestJsonObj] {
    def meta = BoxTestJsonObj
  }
  object BoxTestJsonObj extends JsonObjectMeta[BoxTestJsonObj]

  class BoxTestRec extends MongoRecord[BoxTestRec] with MongoId[BoxTestRec] {
    def meta = BoxTestRec

    object jsonobj extends JsonObjectField[BoxTestRec, BoxTestJsonObj](this, BoxTestJsonObj) {
      def defaultValue = BoxTestJsonObj("0", Empty, Full("Full String"), Failure("Failure"))
    }
    object jsonobjlist extends MongoJsonObjectListField[BoxTestRec, BoxTestJsonObj](this, BoxTestJsonObj)
  }
  object BoxTestRec extends BoxTestRec with MongoMetaRecord[BoxTestRec] {
    def createRecord = new BoxTestRec
    override def formats = super.formats + new BoxSerializer
  }
}

object MongoRecordSpecs extends Specification {

  doBeforeSpec {
    // create a Mongo instance
    val mongoHost = MongoHost("localhost", 27017)
    // define the db
    MongoDB.defineDb(DefaultMongoIdentifier, MongoAddress(mongoHost, "record_specs"))
  }

  def isMongoRunning: Boolean = {
    try {
      MongoDB.use(DefaultMongoIdentifier) ( db => { db.getLastError } )
      true
    }
    catch {
      case e => false
    }
  }

  val debug = false

  def checkMongoIsRunning = isMongoRunning must beEqualTo(true).orSkipExample

  "MongoRecord" should {

    "handle null" in {
      checkMongoIsRunning
      import mongorecordspecs._

      val ntr = NullTestRec.createRecord
      ntr.nullstring.set(null)
      ntr.jsonobjlist.set(List(JsonObj("1", null), JsonObj("2", "jsonobj2")))

      ntr.save must_== ntr

      val ntrFromDb = NullTestRec.find(ntr.id)

      ntrFromDb must notBeEmpty

      ntrFromDb foreach { n =>
        // goes in as
        ntr.nullstring.valueBox.map(_ must beNull)
        ntr.nullstring.value must beNull
        // comes out as
        n.nullstring.valueBox must_== Empty
        n.nullstring.value must_== ""
        // JsonObjects
        n.jsonobjlist.value.size must_== 2
        ntr.jsonobjlist.value.size must_== 2
        n.jsonobjlist.value(0).id must_== ntr.jsonobjlist.value(0).id
        n.jsonobjlist.value(0).name must beNull
        ntr.jsonobjlist.value(0).name must beNull
        n.jsonobjlist.value(1).id must_== ntr.jsonobjlist.value(1).id
        n.jsonobjlist.value(1).name must_== ntr.jsonobjlist.value(1).name
      }
    }

    "handle Box using BoxSerializer" in {
      checkMongoIsRunning
      import mongorecordspecs._

      val btr = BoxTestRec.createRecord
      btr.jsonobjlist.set(
        BoxTestJsonObj("1", Empty, Full("Full String1"), Failure("Failure1")) ::
        BoxTestJsonObj("2", Empty, Full("Full String2"), Failure("Failure2")) ::
        Nil
      )

      btr.save

      val btrFromDb = BoxTestRec.find(btr.id)

      btrFromDb must notBeEmpty

      btrFromDb foreach { b =>
        b.jsonobjlist.value.size must_== 2
        btr.jsonobjlist.value.size must_== 2
        val sortedList = b.jsonobjlist.value.sort(_.id < _.id)
        sortedList(0).boxEmpty must_== Empty
        sortedList(0).boxFull must_== Full("Full String1")
        sortedList(0).boxFail must_== Failure("Failure1")
      }
    }

  }

  doAfterSpec {
    if (!debug && isMongoRunning) {
      // drop the database
      MongoDB.use {
        db => db.dropDatabase
      }
    }

    // clear the mongo instances
    MongoDB.close
  }
}

}
}
}
