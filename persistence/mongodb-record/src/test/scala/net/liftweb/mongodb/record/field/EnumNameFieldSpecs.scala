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
package field {

import org.bson.types.ObjectId
import org.specs.Specification
import org.specs.runner.JUnit4

import net.liftweb.common._
import net.liftweb.json.ext.EnumNameSerializer
import net.liftweb.record.field.{EnumNameField, OptionalEnumNameField}
import net.liftweb.util.Helpers._

import com.mongodb._

package enumnamefieldspecs {
  object WeekDay extends Enumeration {
    type WeekDay = Value
    val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
  }

  case class JsonObj(dow: WeekDay.WeekDay) extends JsonObject[JsonObj] {
    def meta = JsonObj
  }
  object JsonObj extends JsonObjectMeta[JsonObj]

  class EnumNameRec extends MongoRecord[EnumNameRec] with MongoId[EnumNameRec] {
    def meta = EnumNameRec

    object dow extends EnumNameField(this, WeekDay)
    object dowOptional extends OptionalEnumNameField(this, WeekDay)
    object jsonobj extends JsonObjectField[EnumNameRec, JsonObj](this, JsonObj) {
      def defaultValue = JsonObj(WeekDay.Mon)
    }

    override def equals(other: Any): Boolean = other match {
      case that: EnumNameRec =>
        this.id == that.id &&
        this.dow.value == that.dow.value &&
        this.dowOptional.valueBox == that.dowOptional.valueBox &&
        this.jsonobj.value == that.jsonobj.value
      case _ => false
    }
  }
  object EnumNameRec extends EnumNameRec with MongoMetaRecord[EnumNameRec] {
    override def collectionName = "enumnamerecs"
    override def formats = super.formats + new EnumNameSerializer(WeekDay)
  }
}

class EnumNameFieldSpecsTest extends JUnit4(EnumNameFieldSpecs)

object EnumNameFieldSpecs extends Specification with MongoTestKit {

  import enumnamefieldspecs._

  "EnumNameField" should {

    "work with default values" in {
      checkMongoIsRunning

      val er = EnumNameRec.createRecord.save

      val erFromDb = EnumNameRec.find(er.id)
      erFromDb must notBeEmpty
      erFromDb foreach { er2 =>
        er2 mustEqual er
        er2.dow.value mustEqual WeekDay.Mon
        er2.dowOptional.valueBox mustEqual Empty
        er2.jsonobj.value mustEqual JsonObj(WeekDay.Mon)
      }
    }

    "work with set values" in {
      checkMongoIsRunning

      val er = EnumNameRec.createRecord
        .dow(WeekDay.Tue)
        .jsonobj(JsonObj(WeekDay.Sun))
        .save

      val erFromDb = EnumNameRec.find(er.id)
      erFromDb must notBeEmpty
      erFromDb foreach { er2 =>
        er2 mustEqual er
        er2.dow.value mustEqual WeekDay.Tue
        er2.jsonobj.value mustEqual JsonObj(WeekDay.Sun)
      }
    }

    "work with Empty optional values" in {
      checkMongoIsRunning

      val er = EnumNameRec.createRecord
      er.dowOptional.setBox(Empty)
      er.save

      val erFromDb = EnumNameRec.find(er.id)
      erFromDb must notBeEmpty
      erFromDb foreach { er2 =>
        er2 mustEqual er
        er2.dowOptional.valueBox mustEqual Empty
      }
    }

    "work with Full optional values" in {
      checkMongoIsRunning

      val er = EnumNameRec.createRecord
      er.dowOptional.setBox(Full(WeekDay.Sat))
      er.save

      val erFromDb = EnumNameRec.find(er.id)
      erFromDb must notBeEmpty
      erFromDb foreach { er2 =>
        er2 mustEqual er
        er2.dowOptional.valueBox mustEqual Full(WeekDay.Sat)
      }
    }
  }
}

}
}
}
}
