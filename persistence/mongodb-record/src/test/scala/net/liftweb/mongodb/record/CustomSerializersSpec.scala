/*
 * Copyright 2010-2014 WorldWide Conferencing, LLC
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

import common._
import field._
import http.js.JE._
import http.{LiftSession, S}
import json.JsonAST._
import util.Helpers._

import java.util.{Calendar, Date}

import org.bson.types.ObjectId
import org.specs2.mutable.Specification

import net.liftweb.record.field._
import xml.{Elem, NodeSeq}
import util.Helpers

package customserializersspecs {

case class Child(name: String, birthdate: Date) extends JsonObject[Child] {
  def meta = Child
}

object Child extends JsonObjectMeta[Child]

/*
* Date as String
*/
class Person extends MongoRecord[Person] with ObjectIdPk[Person] {
  def meta = Person

  object children extends MongoJsonObjectListField(this, Child)

  object firstBorn extends JsonObjectField(this, Child) {
    def defaultValue = Child("", now)
  }

}

object Person extends Person with MongoMetaRecord[Person]

/*
* Date as Date
*/
class Person2 extends MongoRecord[Person2] with ObjectIdPk[Person2] {
  def meta = Person2

  object children extends MongoJsonObjectListField(this, Child)

  object firstBorn extends JsonObjectField(this, Child) {
    def defaultValue = Child("", now)
  }

}

object Person2 extends Person2 with MongoMetaRecord[Person2] {
  override def formats = allFormats
}

class Player extends MongoRecord[Player] with ObjectIdPk[Player] {
  def meta = Player

  object name extends StringField(this, 256)

}

object Player extends Player with MongoMetaRecord[Player]

/*
* ObjectId as String
*/
case class Team(id: String, name: String, qb: String) extends JsonObject[Team] {
  def meta = Team
}

object Team extends JsonObjectMeta[Team]

class League extends MongoRecord[League] with ObjectIdPk[League] {
  def meta = League

  object teams extends MongoJsonObjectListField(this, Team)

  object champion extends JsonObjectField(this, Team) {
    def defaultValue = Team("", "", "")
  }

}

object League extends League with MongoMetaRecord[League]

/*
* ObjectId as ObjectId
*/
case class Team2(id: ObjectId, name: String, qb: ObjectId) extends JsonObject[Team2] {
  def meta = Team2
}

object Team2 extends JsonObjectMeta[Team2]

class League2 extends MongoRecord[League2] with ObjectIdPk[League2] {
  def meta = League2

  object teams extends MongoJsonObjectListField(this, Team2)

  object champion extends JsonObjectField(this, Team2) {
    def defaultValue = Team2(ObjectId.get, "", ObjectId.get)
  }

}

object League2 extends League2 with MongoMetaRecord[League2] {
  override def formats = super.formats + new ObjectIdSerializer
}

object WeekDay extends Enumeration {
  type WeekDay = Value
  val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
}

class EnumRec extends MongoRecord[EnumRec] with ObjectIdPk[EnumRec] {
  def meta = EnumRec

  object dow extends EnumField(this, WeekDay)

}

object EnumRec extends EnumRec with MongoMetaRecord[EnumRec] {
  override def collectionName = "enumrecs"
}

}


/**
 * Systems under specification for CustomSerializers.
 */
object CustomSerializersSpec extends Specification  with MongoTestKit {
  "CustomSerializers Specification".title

  import customserializersspecs._

  "CustomSerializers" should {
    "handle Date as String value in JsonObjects" in {
      checkMongoIsRunning

      // test data
      val bdjack = Calendar.getInstance.setTimezone(utc)
      bdjack.setTimeInMillis(1288742280000L)
      val bdjill = Calendar.getInstance.setTimezone(utc)
      bdjill.setTimeInMillis(1288742880000L)
      val jack = Child("Jack", bdjack.getTime)
      val jill = Child("Jill", bdjill.getTime)

      // create and save a Person record
      val mother = Person.createRecord
      mother.children(List(jack, jill))
      mother.firstBorn(jack)
      mother.save()

      // retrieve it and compare
      val mother2 = Person.find(mother.id.get)
      mother2.isDefined must_== true
      mother2 foreach {
        m =>
          m.children.value mustEqual mother.children.value
          m.firstBorn.value mustEqual mother.firstBorn.value
      }

      // check the conversion functions
      /*
      mother.children.asJs mustEqual JsArray(
        JsObj(("name", Str("Jack")), ("birthdate", Str("2010-11-02T23:58:00.000Z"))),
        JsObj(("name", Str("Jill")), ("birthdate", Str("2010-11-03T00:08:00.000Z")))
      )*/

      mother.children.asJValue mustEqual JArray(List(
        JObject(List(
          JField("name", JString("Jack")),
          JField("birthdate", JString("2010-11-02T23:58:00.000Z"))
        )),
        JObject(List(
          JField("name", JString("Jill")),
          JField("birthdate", JString("2010-11-03T00:08:00.000Z"))))
      ))
      mother.children.toForm must beEmpty
      /*
      mother.firstBorn.asJs mustEqual
        JsObj(("name", Str("Jack")), ("birthdate", Str("2010-11-02T23:58:00.000Z")))
        */
      mother.firstBorn.asJValue mustEqual
        JObject(List(
          JField("name", JString("Jack")),
          JField("birthdate", JString("2010-11-02T23:58:00.000Z"))
        ))
      mother.firstBorn.toForm must beEmpty
    }

    "handle Date as Date value in JsonObjects using DateSerializer" in {
      checkMongoIsRunning

      // test data
      val bdjack = Calendar.getInstance.setTimezone(utc)
      bdjack.setTimeInMillis(1288742280000L)
      val bdjill = Calendar.getInstance.setTimezone(utc)
      bdjill.setTimeInMillis(1288742880000L)
      val jack = Child("Jack", bdjack.getTime)
      val jill = Child("Jill", bdjill.getTime)

      // create and save a Person record
      val mother = Person2.createRecord
      mother.children(List(jack, jill))
      mother.firstBorn(jack)
      mother.save()

      // retrieve it and compare
      val mother2 = Person2.find(mother.id.get)
      mother2.isDefined must_== true
      mother2 foreach {
        m =>
          m.children.value mustEqual mother.children.value
          m.firstBorn.value mustEqual mother.firstBorn.value
      }

      // check the conversion functions
      /*
      mother.children.asJs mustEqual JsArray(
        JsObj(("name", Str("Jack")), ("birthdate", JsObj(("$dt", Str("2010-11-02T23:58:00.000Z"))))),
        JsObj(("name", Str("Jill")), ("birthdate", JsObj(("$dt", Str("2010-11-03T00:08:00.000Z")))))
      )*/

      mother.children.asJValue mustEqual JArray(List(
        JObject(List(
          JField("name", JString("Jack")),
          JField("birthdate", JObject(List(JField("$dt", JString("2010-11-02T23:58:00.000Z")))))
        )),
        JObject(List(
          JField("name", JString("Jill")),
          JField("birthdate", JObject(List(JField("$dt", JString("2010-11-03T00:08:00.000Z")))))
        ))
      ))
      mother.children.toForm must beEmpty

      /*
      mother.firstBorn.asJs mustEqual
        JsObj(("name", Str("Jack")), ("birthdate", JsObj(("$dt", Str("2010-11-02T23:58:00.000Z")))))
        */

      mother.firstBorn.asJValue mustEqual
        JObject(List(
          JField("name", JString("Jack")),
          JField("birthdate", JObject(List(JField("$dt", JString("2010-11-02T23:58:00.000Z")))))
        ))
      mother.firstBorn.toForm must beEmpty
    }

    "handle ObjectId as String value in JsonObjects" in {
      checkMongoIsRunning

      // test data
      val rmoss = Player.createRecord.name("Randy Moss").save()
      val bfavre = Player.createRecord.name("Brett Favre").save()
      val vikes = Team(ObjectId.get.toString, "Vikings", bfavre.id.toString)
      val jets = Team(ObjectId.get.toString, "Jets", "")
      val saints = Team(ObjectId.get.toString, "Saints", "")

      // create and save a League record
      val nfl = League.createRecord
      nfl.teams(List(vikes, jets, saints))
      nfl.champion(saints)
      nfl.save()

      // retrieve it and compare
      val nfl2 = League.find(nfl.id.get)
      nfl2.isDefined must_== true
      nfl2 foreach {
        l =>
          l.teams.value mustEqual nfl.teams.value
          l.champion.value mustEqual nfl.champion.value
      }

      // find a player
      val vqb = Player.find(vikes.qb)
      vqb.isDefined must_== true
      vqb foreach {
        p =>
          p.name.value mustEqual "Brett Favre"
      }

      // check the conversion functions
      nfl.id.asJs mustEqual Str(nfl.id.value.toString)
      nfl.id.asJValue mustEqual JString(nfl.id.value.toString)
      val session = new LiftSession("", randomString(20), Empty)
      val formPattern = <input name=".*" type="text" tabindex="1" value={nfl.id.value.toString} id="_id_id"></input>
      S.initIfUninitted(session) {
        val form = nfl.id.toForm
        form.isDefined must_== true
        form foreach {
          fprime =>
            val f = ("* [name]" #> ".*" & "select *" #> (((ns: NodeSeq) => ns.filter {
              case e: Elem => e.attribute("selected").map(_.text) == Some("selected")
              case _ => false
            }) andThen "* [value]" #> ".*"))(fprime)
            val ret: Boolean = Helpers.compareXml(f, formPattern)
            ret must_== true
        }
      }

      // check the setFrom* functions
      val nflid = ObjectId.get
      nfl.id.setFromString(nflid.toString)
      nfl.id.value mustEqual nflid

      nfl.id.setFromString("garbage")
      nfl.id.valueBox mustEqual Failure("Invalid ObjectId string: garbage")

      nfl.id.setFromJValue(JString(nflid.toString))
      nfl.id.value mustEqual nflid

      nfl.id.setFromAny(nflid)
      nfl.id.value mustEqual nflid

      nfl.id.setFromAny(nflid.toString)
      nfl.id.value mustEqual nflid

    }

    "handle ObjectId as ObjectId values in JsonObjects using ObjectIdSerializer" in {
      checkMongoIsRunning

      // test data
      val rmoss = Player.createRecord.name("Randy Moss").save()
      val bfavre = Player.createRecord.name("Brett Favre").save()
      val vikes = Team2(ObjectId.get, "Vikings", bfavre.id.get)
      val jets = Team2(ObjectId.get, "Jets", bfavre.id.get)
      val saints = Team2(ObjectId.get, "Saints", bfavre.id.get)

      // create and save a League record
      val nfl = League2.createRecord
      nfl.teams(List(vikes, jets, saints))
      nfl.champion(saints)
      nfl.save()

      // retrieve it and compare
      val nfl2 = League2.find(nfl.id.toString)
      nfl2.isDefined must_== true
      nfl2 foreach {
        l =>
          l.teams.value mustEqual nfl.teams.value
          l.champion.value mustEqual nfl.champion.value
      }

      // find a player
      val vqb = Player.find(vikes.qb)
      vqb.isDefined must_== true
      vqb foreach {
        p =>
          p.name.value mustEqual "Brett Favre"
      }

      // check the conversion functions
      nfl.id.asJs.toJsCmd mustEqual """{"$oid":"%s"}""".format(nfl.id.value.toString)
      nfl.id.asJValue mustEqual JObject(List(JField("$oid", JString(nfl.id.value.toString))))
      val session = new LiftSession("", randomString(20), Empty)
      val formPattern = <input name=".*" type="text" tabindex="1" value={nfl.id.value.toString} id="_id_id"></input>
      S.initIfUninitted(session) {
        val form = nfl.id.toForm
        form.isDefined must_== true
        form foreach {
          fprime =>
            val f = ("* [name]" #> ".*" & "select *" #> (((ns: NodeSeq) => ns.filter {
              case e: Elem => e.attribute("selected").map(_.text) == Some("selected")
              case _ => false
            }) andThen "* [value]" #> ".*"))(fprime)
            val ret: Boolean = Helpers.compareXml(f, formPattern)
            ret must_== true
        }
      }

      // check the setFrom* functions
      val nflid = ObjectId.get
      nfl.id.setFromString(nflid.toString)
      nfl.id.value mustEqual nflid

      nfl.id.setFromString("garbage")
      nfl.id.valueBox mustEqual Failure("Invalid ObjectId string: garbage")

      nfl.id.setFromJValue(JObject(List(JField("$oid", JString(nflid.toString)))))
      nfl.id.value mustEqual nflid

      nfl.id.setFromAny(nflid)
      nfl.id.value mustEqual nflid

      nfl.id.setFromAny(nflid.toString)
      nfl.id.value mustEqual nflid
    }
  }
}
