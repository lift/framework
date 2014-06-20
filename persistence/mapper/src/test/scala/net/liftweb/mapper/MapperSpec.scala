/*
 * Copyright 2007-2011 WorldWide Conferencing, LLC
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
package mapper

import java.util.Locale

import org.specs2.mutable.Specification
import org.specs2.specification.BeforeExample

import common._
import json._
import util._
import Helpers._
import http.LiftRules
import http.provider.HTTPRequest


/**
 * Systems under specification for Mapper. The model classes here are
 * defined in MapperSpecsModel.scala
 */
class MapperSpec extends Specification with BeforeExample {
  "Mapper Specification".title
  // Do everything in order.
  sequential

  // Make sure we have everything configured first
  MapperSpecsModel.setup()

  def providers: List[DbProviders.Provider] = DbProviders.H2MemoryProvider :: Nil

  /*
   private def logDBStuff(log: DBLog, len: Long) {
   println(" in log stuff "+log.getClass.getName)
   log match {
   case null =>
   case _ => println(log.allEntries)
   }
   }

   DB.addLogFunc(logDBStuff)
   */

//  if (!DB.loggingEnabled_? && doLog) DB.addLogFunc(logDBStuff)

  def before = MapperSpecsModel.cleanup()  // before each example

  providers.foreach(provider => {
   try {
    provider.setupDB

    ("Mapper for " + provider.name) should {

      "schemify" in {
        val elwood = SampleModel.find(By(SampleModel.firstName, "Elwood")).openOrThrowException("Test")
        val madeline = SampleModel.find(By(SampleModel.firstName, "Madeline")).openOrThrowException("Test")
        val archer = SampleModel.find(By(SampleModel.firstName, "Archer")).openOrThrowException("Test")
        val notNull = SampleModel.find(By(SampleModel.firstName, "NotNull")).openOrThrowException("Test")

        elwood.firstName.get must_== "Elwood"
        madeline.firstName.get must_== "Madeline"
        archer.firstName.get must_== "Archer"

        archer.moose.get must_== Empty
        notNull.moose.get must_== Full(99L)

        val disabled = SampleModel.find(By(SampleModel.status, SampleStatus.Disabled))

        val meow = SampleTag.find(By(SampleTag.tag, "Meow")).openOrThrowException("Test")

        meow.tag.get must_== "Meow"

        elwood.id.get must be_<(madeline.id.get).eventually
      }

      "non-snake connection should lower case default table & column names" in {
        SampleModel.firstName.name must_== "firstName"
        SampleModel.firstName.dbColumnName must_== "firstname"
        SampleModel.dbTableName must_== "samplemodel"
      }

      "should use displayNameCalculator for displayName" in {
        val localeCalculator = LiftRules.localeCalculator
        SampleModel.firstName.displayName must_== "DEFAULT:SampleModel.firstName"

        LiftRules.localeCalculator = (request: Box[HTTPRequest]) => request.flatMap(_.locale)
          .openOr(new Locale("xx", "YY"))
        SampleModel.firstName.displayName must_== "xx_YY:SampleModel.firstName"

        LiftRules.localeCalculator = localeCalculator
        success
      }

      "snake connection should snakify default table & column names" in {
        SampleModelSnake.firstName.name must_== "firstName"
        SampleModelSnake.firstName.dbColumnName must_== "first_name"
        SampleModelSnake.dbTableName must_== "sample_model_snake"
      }

      "user defined names are not changed" in {
        SampleTag.extraColumn.name must_== "extraColumn"
        SampleTag.extraColumn.dbColumnName must_== "AnExtraColumn"
        Mixer.dbTableName must_== "MIXME_UP"
      }

      "basic JSON encoding/decoding works" in {
        val m = SampleModel.findAll().head
        val json = m.encodeAsJson()
        val rebuilt = SampleModel.buildFromJson(json)
        m must_== rebuilt
      }

      "basic JSON encoding/decoding works with snake_case" in {
        val m = SampleModelSnake.findAll().head
        val json = m.encodeAsJson()
        val rebuilt = SampleModelSnake.buildFromJson(json)
        m must_== rebuilt
      }

      "Can JSON decode and write back" in {
        val m = SampleModel.find(2).openOrThrowException("Test")
        val json = m.encodeAsJson()
        val rebuilt = SampleModel.buildFromJson(json)
        rebuilt.firstName("yak").save
        val recalled = SampleModel.find(2).openOrThrowException("Test")
        recalled.firstName.get must_== "yak"
      }

      "You can put stuff in a Set" in {
        val m1 = SampleModel.find(1).openOrThrowException("Test")
        val m2 = SampleModel.find(1).openOrThrowException("Test")

        (m1 == m2) must_== true

        val s1 = Set(SampleModel.findAll: _*)

        s1.contains(m1) must_== true

        val s2 = s1 ++ SampleModel.findAll

        s1.size must_== s2.size
      }

      "Like works" in {
        val oo = SampleTag.findAll(Like(SampleTag.tag, "%oo%"))

        (oo.length > 0) must beTrue

        for (t <- oo)
          (t.tag.get.indexOf("oo") >= 0) must beTrue

        for (t <- oo)
          t.model.cached_? must beFalse

        val mm = SampleTag.findAll(Like(SampleTag.tag, "M%"))

        for (t <- mm)
          (t.tag.get.startsWith("M")) must beTrue

        for (t <- mm) yield {
          t.model.cached_? must beFalse
          t.model.obj
          t.model.cached_? must beTrue
        }

        (mm.length > 0) must beTrue
      }

      "Nullable Long works" in {
        SampleModel.create.firstName("fruit").moose(Full(77L)).save

        SampleModel.findAll(By(SampleModel.moose, Empty)).length must_== 3L
        SampleModel.findAll(NotBy(SampleModel.moose, Empty)).length must_== 2L
        SampleModel.findAll(NotNullRef(SampleModel.moose)).length must_== 2L
        SampleModel.findAll(NullRef(SampleModel.moose)).length must_== 3L
      }

      "enforce NOT NULL" in {
        val nullString: String = null
        SampleModel.create.firstName("Not Null").notNull(nullString).save must throwA[java.sql.SQLException]
      }

      "enforce FK constraint on DefaultConnection" in {
        val supportsFK = DB.use(DefaultConnectionIdentifier) { conn => conn.driverType.supportsForeignKeys_? }
        if (!supportsFK) skipped("Driver %s does not support FK constraints".format(provider))

        SampleTag.create.model(42).save must throwA[java.sql.SQLException]
      }

      "not enforce FK constraint on SnakeConnection" in {
        SampleTagSnake.create.model(42).save must_== true
      }

      "Precache works" in {
        val oo = SampleTag.findAll(By(SampleTag.tag, "Meow"), PreCache(SampleTag.model))

        for (t <- oo) yield t.model.cached_? must beTrue

        (oo.length > 0) must beTrue
      }

      "Precache works with OrderBy" in {
        if ((provider ne DbProviders.DerbyProvider)
            && (provider ne DbProviders.MySqlProvider)) {
          // this doesn't work for Derby, but it's a derby bug
          // nor does it work in MySQL, but it's a MySQL limitation
          //  try { provider.setupDB } catch { case e => skip(e.getMessage) }
          val dogs = Dog.findAll(By(Dog.name, "fido"), OrderBy(Dog.name, Ascending), PreCache(Dog.owner))
          val oo = SampleTag.findAll(OrderBy(SampleTag.tag, Ascending), MaxRows(2), PreCache(SampleTag.model))

          (oo.length > 0) must beTrue
          for (t <- oo) t.model.cached_? must beTrue
        }
        success
      }

      "Non-deterministic Precache works" in {
        val dogs = Dog.findAll(By(Dog.name, "fido"), PreCache(Dog.owner, false))
        val oo = SampleTag.findAll(By(SampleTag.tag, "Meow"), PreCache(SampleTag.model, false))

        for (t <- oo) yield t.model.cached_? must beTrue

        (oo.length > 0) must beTrue
      }

      "Non-deterministic Precache works with OrderBy" in {
        val dogs = Dog.findAll(By(Dog.name, "fido"), OrderBy(Dog.name, Ascending), PreCache(Dog.owner, false))
        val oo = SampleTag.findAll(OrderBy(SampleTag.tag, Ascending), MaxRows(2), PreCache(SampleTag.model, false))

        for (t <- oo) yield t.model.cached_? must beTrue

        (oo.length > 0) must beTrue
      }

      "work with Mixed case" in {
        val elwood = Mixer.find(By(Mixer.name, "Elwood")).openOrThrowException("Test")
        val madeline = Mixer.find(By(Mixer.name, "Madeline")).openOrThrowException("Test")
        val archer = Mixer.find(By(Mixer.name, "Archer")).openOrThrowException("Test")

        elwood.name.get must_== "Elwood"
        madeline.name.get must_== "Madeline"
        archer.name.get must_== "Archer"

        elwood.weight.get must_== 33
        madeline.weight.get must_== 44
        archer.weight.get must_== 105
      }

      "work with Mixed case update and delete" in {
        val elwood = Mixer.find(By(Mixer.name, "Elwood")).openOrThrowException("Test")
        elwood.name.get must_== "Elwood"
        elwood.name("FruitBar").weight(966).save

        val fb = Mixer.find(By(Mixer.weight, 966)).openOrThrowException("Test")

        fb.name.get must_== "FruitBar"
        fb.weight.get must_== 966
        fb.delete_!

        Mixer.find(By(Mixer.weight, 966)).isDefined must_== false
        Mixer.find(By(Mixer.name, "FruitBar")).isDefined must_== false
        Mixer.find(By(Mixer.name, "Elwood")).isDefined must_== false

      }

      "work with Mixed case update and delete for Dog2" in {
        val elwood = Dog2.find(By(Dog2.name, "Elwood")).openOrThrowException("Test")
        elwood.name.get must_== "Elwood"
        elwood.name("FruitBar").actualAge(966).save

        val fb = Dog2.find(By(Dog2.actualAge, 966)).openOrThrowException("Test")

        fb.name.get must_== "FruitBar"
        fb.actualAge.get must_== 966
        fb.delete_!

        Dog2.find(By(Dog2.actualAge, 966)).isDefined must_== false
        Dog2.find(By(Dog2.name, "FruitBar")).isDefined must_== false
        Dog2.find(By(Dog2.name, "Elwood")).isDefined must_== false
      }

      "Non-autogenerated primary key items should be savable after a field has been changed" in {
        val item = TstItem.create.tmdbId(1L).saveMe
        item.name("test").save must_== true
      }

      "we can read and write String primary keys" in {
        val i1 = Thing.create.name("frog").saveMe
        val i2 = Thing.create.name("dog").saveMe

        Thing.find(By(Thing.thing_id, i1.thing_id.get)).openOrThrowException("Test").name.get must_== "frog"
        Thing.find(By(Thing.thing_id, i2.thing_id.get)).openOrThrowException("Test").name.get must_== "dog"
      }


      "Precache works with OrderBy with Mixed Case" in {
        if ((provider ne DbProviders.DerbyProvider)
            && (provider ne DbProviders.MySqlProvider)) {
          // this doesn't work for Derby, but it's a derby bug
          // nor does it work in MySQL, but it's a MySQL limitation
          //  try { provider.setupDB } catch { case e => skip(e.getMessage) }
          val dogs = Dog2.findAll(By(Dog2.name, "fido"), OrderBy(Dog2.name, Ascending), PreCache(Dog2.owner))
          val oo = SampleTag.findAll(OrderBy(SampleTag.tag, Ascending), MaxRows(2), PreCache(SampleTag.model))

          (oo.length > 0) must beTrue
          for (t <- oo) yield t.model.cached_? must beTrue
        }
        success
      }

      "Non-deterministic Precache works with Mixed Case" in {
        val dogs = Dog2.findAll(By(Dog2.name, "fido"), PreCache(Dog2.owner, false))
        val oo = SampleTag.findAll(By(SampleTag.tag, "Meow"), PreCache(SampleTag.model, false))

        for (t <- oo) yield t.model.cached_? must beTrue

        (oo.length > 0) must beTrue
      }


      "CreatedAt and UpdatedAt work" in {
        val now = Helpers.now
        val dog = Dog2.find().openOrThrowException("Test")

        val oldUpdate = dog.updatedAt.get

        val d1 = (now.getTime - dog.createdAt.get.getTime) / 100000L
        d1 must_== 0L

        val d2 = (now.getTime - dog.updatedAt.get.getTime) / 100000L
        d2 must_== 0L

        dog.name("ralph").save

        val dog2 = Dog2.find(dog.dog2id.get).openOrThrowException("Test")

        dog.createdAt.get.getTime must_== dog2.createdAt.get.getTime
        oldUpdate.getTime must_!= dog2.updatedAt.get.getTime
      }

      "Non-deterministic Precache works with OrderBy with Mixed Case" in {
        val dogs = Dog2.findAll(By(Dog2.name, "fido"), OrderBy(Dog2.name, Ascending), PreCache(Dog2.owner, false))

        val oo = SampleTag.findAll(OrderBy(SampleTag.tag, Ascending), MaxRows(2), PreCache(SampleTag.model, false))

        for (t <- oo) yield t.model.cached_? must beTrue

        (oo.length > 0) must beTrue
      }

      "Save flag results in update rather than insert" in {
        val elwood = SampleModel.find(By(SampleModel.firstName, "Elwood")).openOrThrowException("Test")
        elwood.firstName.get must_== "Elwood"
        elwood.firstName("Frog").save

        val frog = SampleModel.find(By(SampleModel.firstName, "Frog")).openOrThrowException("Test")
        frog.firstName.get must_== "Frog"

        SampleModel.findAll().length must_== 4
        SampleModel.find(By(SampleModel.firstName, "Elwood")).isEmpty must_== true
      }

      "accept a Seq[T] as argument to ByList query parameter" in {
        // See http://github.com/dpp/liftweb/issues#issue/77 for original request
        val seq: Seq[String] = List("Elwood", "Archer")
        val result = SampleModel.findAll(ByList(SampleModel.firstName, seq))
        result.length must_== 2
      }
    }
   } catch {
     case e if !provider.required_? => skipped("Provider %s not available: %s".format(provider, e))
     case _: Exception => skipped
   }
  })
}


