/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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
package mapper {

import _root_.org.specs._
import _root_.org.specs.runner.JUnit3
import _root_.org.specs.runner.ConsoleRunner
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http.LiftRules
import _root_.net.liftweb.http.provider.HTTPRequest

import Helpers._
import _root_.net.liftweb.json._
import _root_.java.sql.{Connection, DriverManager}
import _root_.java.util.Locale
//import _root_.net.liftweb.mapper.DBVendors.{MySqlRunner, DerbyRunner}

class MapperSpecsAsTest extends JUnit3(MapperSpecs)
object MapperSpecsRunner extends ConsoleRunner(MapperSpecs)

object MapperSpecs extends Specification {

  val doLog = false

  def providers =
    if (false || Props.getBool("lift.fasttest", false))
      (DBProviders.H2MemoryProvider :: Nil)
    else
      DBProviders.asList

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

  def dbSetup() {
    Schemifier.destroyTables_!!(ignoreLogger _, SampleModel, SampleTag,
                                Dog, User, Mixer, Dog2)
    Schemifier.schemify(true, ignoreLogger _, SampleModel, SampleTag,
                        User, Dog, Mixer, Dog2)
  }

  def snakify(connid: ConnectionIdentifier, name: String): String = {
    if (connid.jndiName == "snake")
      StringHelpers.snakify(name)
    else
      name.toLowerCase
  }

  /*
  if (!DB.loggingEnabled_? && doLog)
    DB.addLogFunc(logDBStuff)
*/

  MapperRules.columnName = snakify
  MapperRules.tableName = snakify

  // Simple name calculator
  def displayNameCalculator(bm:BaseMapper,l:java.util.Locale,name:String) = {
    val mapperName = bm.dbName
    val displayName = name match {
      case "firstName" if l == Locale.getDefault() => "DEFAULT:"+mapperName+"."+name
      case "firstName" if l == new Locale("da","DK") => "da_DK:"+mapperName+"."+name
      case _ => name
    }
    displayName
  }
  MapperRules.displayNameCalculator.default.set(displayNameCalculator _)

  // Snake connection doesn't create FK constraints
  MapperRules.createForeignKeys_? = c => c.jndiName != "snake"

  providers.foreach(provider => {
      def cleanup() {
        try { provider.setupDB } catch { case e if !provider.required_? => skip("Provider %s not available: %s".format(provider, e)) }
        Schemifier.destroyTables_!!(DefaultConnectionIdentifier, if (doLog) Schemifier.infoF _ else ignoreLogger _,  SampleTag, SampleModel, Dog, Mixer, Dog2, User, TstItem)
        Schemifier.destroyTables_!!(DBProviders.SnakeConnectionIdentifier, if (doLog) Schemifier.infoF _ else ignoreLogger _, SampleTagSnake, SampleModelSnake)
        Schemifier.schemify(true, if (doLog) Schemifier.infoF _ else ignoreLogger _, DefaultConnectionIdentifier, SampleModel, SampleTag, User, Dog, Mixer, Dog2, TstItem)
        Schemifier.schemify(true, if (doLog) Schemifier.infoF _ else ignoreLogger _, DBProviders.SnakeConnectionIdentifier, SampleModelSnake, SampleTagSnake)
      }

      ("Mapper for " + provider.name) should {
        "schemify" in {
          cleanup()

          val elwood = SampleModel.find(By(SampleModel.firstName, "Elwood")).open_!
          val madeline = SampleModel.find(By(SampleModel.firstName, "Madeline")).open_!
          val archer = SampleModel.find(By(SampleModel.firstName, "Archer")).open_!
          val notNull = SampleModel.find(By(SampleModel.firstName, "NotNull")).open_!

          elwood.firstName.is must_== "Elwood"
          madeline.firstName.is must_== "Madeline"
          archer.firstName.is must_== "Archer"

          archer.moose.is must_== Empty
          notNull.moose.is must_== Full(99L)

          val meow = SampleTag.find(By(SampleTag.tag, "Meow")).open_!

          meow.tag.is must_== "Meow"

          elwood.id.is must be_<(madeline.id.is)
        }

        "non-snake connection should lower case default table & column names" in {
          SampleModel.firstName.name must_== "firstName"
          SampleModel.firstName.dbColumnName must_== "firstname"
          SampleModel.dbTableName must_== "samplemodel"
        }

        "should use displayNameCalculator for displayName" in {
          val localeCalculator = LiftRules.localeCalculator
          SampleModel.firstName.displayName must_== "DEFAULT:SampleModel.firstName"

          LiftRules.localeCalculator = (request: Box[HTTPRequest]) => request.flatMap(_.locale).openOr(new Locale("da","DK"))
          SampleModel.firstName.displayName must_== "da_DK:SampleModel.firstName"

          LiftRules.localeCalculator = localeCalculator
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
          cleanup()
          val m = SampleModel.findAll().head
          val json = m.encodeAsJson()
          val rebuilt = SampleModel.buildFromJson(json)
          m must_== rebuilt
        }

        "basic JSON encoding/decoding works with snake_case" in {
          cleanup()
          val m = SampleModelSnake.findAll().head
          val json = m.encodeAsJson()
          val rebuilt = SampleModelSnake.buildFromJson(json)
          m must_== rebuilt
        }


        "Can JSON decode and write back" in {
          cleanup()
          val m = SampleModel.find(2).open_!
          val json = m.encodeAsJson()
          val rebuilt = SampleModel.buildFromJson(json)
          rebuilt.firstName("yak").save
          val recalled = SampleModel.find(2).open_!
          recalled.firstName.is must_== "yak"
        }

        "You can put stuff in a Set" in {
          cleanup()
          val m1 = SampleModel.find(1).open_!
          val m2 = SampleModel.find(1).open_!

          (m1 == m2) must_== true

          val s1 = Set(SampleModel.findAll :_*)

          s1.contains(m1) must_== true

          val s2 = s1 ++ SampleModel.findAll

          s1.size must_== s2.size
        }


        "Like works" in {
          cleanup()

          val oo = SampleTag.findAll(Like(SampleTag.tag, "%oo%"))

          (oo.length > 0) must beTrue

          for (t <- oo)
          (t.tag.is.indexOf("oo") >= 0) must beTrue

          for (t <- oo)
          t.model.cached_? must beFalse

          val mm = SampleTag.findAll(Like(SampleTag.tag, "M%"))

          (mm.length > 0) must beTrue

          for (t <- mm)
          (t.tag.is.startsWith("M")) must beTrue

          for (t <- mm) {
            t.model.cached_? must beFalse
            t.model.obj
            t.model.cached_? must beTrue
          }
        }

        "Nullable Long works" in {
          cleanup()

          SampleModel.create.firstName("fruit").moose(Full(77L)).save

          SampleModel.findAll(By(SampleModel.moose, Empty)).length must_== 3L
          SampleModel.findAll(NotBy(SampleModel.moose, Empty)).length must_== 2L
          SampleModel.findAll(NotNullRef(SampleModel.moose)).length must_== 2L
          SampleModel.findAll(NullRef(SampleModel.moose)).length must_== 3L
        }

        "enforce NOT NULL" in {
          cleanup()

          val nullString: String = null
          SampleModel.create.firstName("Not Null").notNull(nullString).save must throwA[java.sql.SQLException]
        }

        "enforce FK constraint on DefaultConnection" in {
          cleanup()
          val supportsFK = DB.use(DefaultConnectionIdentifier) {
            conn => conn.driverType.supportsForeignKeys_?
          }
          if (!supportsFK) skip("Driver %s does not support FK constraints".format(provider))

          SampleTag.create.model(42).save must throwA[java.sql.SQLException]
        }

        "not enforce FK constraint on SnakeConnection" in {
          cleanup()
          SampleTagSnake.create.model(42).save
        }

        "Precache works" in {
          cleanup()

          val oo = SampleTag.findAll(By(SampleTag.tag, "Meow"),
                                     PreCache(SampleTag.model))

          (oo.length > 0) must beTrue

          for (t <- oo)
          t.model.cached_? must beTrue
        }

        "Precache works with OrderBy" in {
          if ((provider ne DBProviders.DerbyProvider)
              && (provider ne DBProviders.MySqlProvider)) { // this doesn't work for Derby, but it's a derby bug
            // nor does it work in MySQL, but it's a MySQL limitation
            //  try { provider.setupDB } catch { case e => skip(e.getMessage) }

            cleanup()

            val dogs = Dog.findAll(By(Dog.name,"fido"),OrderBy(Dog.name,Ascending),
                                   PreCache(Dog.owner))

            val oo = SampleTag.findAll(OrderBy(SampleTag.tag, Ascending),
                                       MaxRows(2),
                                       PreCache(SampleTag.model))

            (oo.length > 0) must beTrue

            for (t <- oo)
            t.model.cached_? must beTrue
          }
        }

        "Non-deterministic Precache works" in {
          cleanup()

          val dogs = Dog.findAll(By(Dog.name,"fido"), PreCache(Dog.owner, false))
          val oo = SampleTag.findAll(By(SampleTag.tag, "Meow"),
                                     PreCache(SampleTag.model, false))

          (oo.length > 0) must beTrue

          for (t <- oo)
          t.model.cached_? must beTrue
        }

        "Non-deterministic Precache works with OrderBy" in {
          cleanup()

          val dogs = Dog.findAll(By(Dog.name,"fido"),OrderBy(Dog.name,Ascending),
                                 PreCache(Dog.owner, false))

          val oo = SampleTag.findAll(OrderBy(SampleTag.tag, Ascending),
                                     MaxRows(2),
                                     PreCache(SampleTag.model, false))

          (oo.length > 0) must beTrue

          for (t <- oo)
          t.model.cached_? must beTrue
        }


        "work with Mixed case" in {
          cleanup()

          val elwood = Mixer.find(By(Mixer.name, "Elwood")).open_!
          val madeline = Mixer.find(By(Mixer.name, "Madeline")).open_!
          val archer = Mixer.find(By(Mixer.name, "Archer")).open_!

          elwood.name.is must_== "Elwood"
          madeline.name.is must_== "Madeline"
          archer.name.is must_== "Archer"

          elwood.weight.is must_== 33
          madeline.weight.is must_== 44
          archer.weight.is must_== 105
        }

        "work with Mixed case update and delete" in {
          cleanup()


          val elwood = Mixer.find(By(Mixer.name, "Elwood")).open_!


          elwood.name.is must_== "Elwood"

          elwood.name("FruitBar").weight(966).save

          val fb = Mixer.find(By(Mixer.weight, 966)).open_!

          fb.name.is must_== "FruitBar"

          fb.weight.is must_== 966

          fb.delete_!

          Mixer.find(By(Mixer.weight, 966)).isDefined must_== false
          Mixer.find(By(Mixer.name, "FruitBar")).isDefined must_== false
          Mixer.find(By(Mixer.name, "Elwood")).isDefined must_== false

        }

        "work with Mixed case update and delete for Dog2" in {
          cleanup()

          val elwood = Dog2.find(By(Dog2.name, "Elwood")).open_!

          elwood.name.is must_== "Elwood"

          elwood.name("FruitBar").actualAge(966).save

          val fb = Dog2.find(By(Dog2.actualAge, 966)).open_!

          fb.name.is must_== "FruitBar"

          fb.actualAge.is must_== 966

          fb.delete_!

          Dog2.find(By(Dog2.actualAge, 966)).isDefined must_== false
          Dog2.find(By(Dog2.name, "FruitBar")).isDefined must_== false
          Dog2.find(By(Dog2.name, "Elwood")).isDefined must_== false
        }

	
	"Non-autogenerated primary key items should be savable after a field has been changed" in {
          cleanup()

	  val item = TstItem.create.tmdbId(1L).saveMe
	  item.name("test").save
	}


        "Precache works with OrderBy with Mixed Case" in {
          if ((provider ne DBProviders.DerbyProvider)
              && (provider ne DBProviders.MySqlProvider)) { // this doesn't work for Derby, but it's a derby bug
            // nor does it work in MySQL, but it's a MySQL limitation
            //  try { provider.setupDB } catch { case e => skip(e.getMessage) }

            cleanup()

            val dogs = Dog2.findAll(By(Dog2.name,"fido"),OrderBy(Dog2.name,Ascending),
                                    PreCache(Dog2.owner))

            val oo = SampleTag.findAll(OrderBy(SampleTag.tag, Ascending),
                                       MaxRows(2),
                                       PreCache(SampleTag.model))

            (oo.length > 0) must beTrue

            for (t <- oo)
            t.model.cached_? must beTrue
          }
        }

        "Non-deterministic Precache works with Mixed Case" in {
          cleanup()

          val dogs = Dog2.findAll(By(Dog2.name,"fido"), PreCache(Dog2.owner, false))
          val oo = SampleTag.findAll(By(SampleTag.tag, "Meow"),
                                     PreCache(SampleTag.model, false))

          (oo.length > 0) must beTrue

          for (t <- oo)
          t.model.cached_? must beTrue
        }


        "Createdat and updated at work" in {
          val now = Helpers.now

          cleanup()

          val dog = Dog2.find().open_!

          val oldUpdate = dog.updatedAt.is

          val d1 = (now.getTime - dog.createdAt.getTime) / 100000L
          d1 must_== 0L

          val d2 = (now.getTime - dog.updatedAt.getTime) / 100000L
          d2 must_== 0L

          dog.name("ralph").save

          val dog2 = Dog2.find(dog.dog2id.is).open_!

          dog.createdAt.is.getTime must_== dog2.createdAt.is.getTime

          oldUpdate.getTime must_!= dog2.updatedAt.is.getTime

        }

        "Non-deterministic Precache works with OrderBy with Mixed Case" in {
          cleanup()

          val dogs = Dog2.findAll(By(Dog2.name,"fido"),OrderBy(Dog2.name,Ascending),
                                  PreCache(Dog2.owner, false))

          val oo = SampleTag.findAll(OrderBy(SampleTag.tag, Ascending),
                                     MaxRows(2),
                                     PreCache(SampleTag.model, false))

          (oo.length > 0) must beTrue

          for (t <- oo)
          t.model.cached_? must beTrue
        }


        "Save flag works" in {
          cleanup()

          val elwood = SampleModel.find(By(SampleModel.firstName, "Elwood")).open_!

          elwood.firstName.is must_== "Elwood"

          elwood.firstName("Frog").save

          val frog = SampleModel.find(By(SampleModel.firstName, "Frog")).open_!

          frog.firstName.is must_== "Frog"

          SampleModel.findAll().length must_== 4

          SampleModel.find(By(SampleModel.firstName, "Elwood")).isEmpty must_== true
        }

        "accept a Seq[T] as argument to ByList query parameter" in {
          // See http://github.com/dpp/liftweb/issues#issue/77 for original request
          cleanup()
          val seq: Seq[String] = List("Elwood", "Archer")
          val result = SampleModel.findAll(ByList(SampleModel.firstName, seq))
          result.length must_== 2
        }
      }
    })

  private def ignoreLogger(f: => AnyRef): Unit = ()
}

object SampleTag extends SampleTag with LongKeyedMetaMapper[SampleTag] {
  override def dbAddTable = Full(populate _)
  private def populate {
    val samp = SampleModel.findAll()
    val tags = List("Hello", "Moose", "Frog", "WooHoo", "Sloth",
                    "Meow", "Moof")
    for (t <- tags;
         m <- samp) SampleTag.create.tag(t).model(m).save
  }
}

class SampleTag extends LongKeyedMapper[SampleTag] with IdPK {
  def getSingleton = SampleTag // what's the "meta" server

  object tag extends MappedString(this, 32)

  object model extends MappedLongForeignKey(this, SampleModel)

  object extraColumn extends MappedString(this, 32) {
    override def dbColumnName = "AnExtraColumn"
  }

}

object SampleModel extends SampleModel with KeyedMetaMapper[Long, SampleModel] {
  override def dbAddTable = Full(populate _)

  def encodeAsJson(in: SampleModel): JsonAST.JObject = encodeAsJSON_!(in)
  def buildFromJson(json: JsonAST.JObject): SampleModel = decodeFromJSON_!(json, false)

  private def populate {
    create.firstName("Elwood").save
    create.firstName("Madeline").save
    create.firstName("Archer").save
    create.firstName("NotNull").moose(Full(99L)).save
  }
}

class SampleModel extends KeyedMapper[Long, SampleModel] {
  def getSingleton = SampleModel // what's the "meta" server
  def primaryKeyField = id

  object id extends MappedLongIndex(this)
  object firstName extends MappedString(this, 32)
  object moose extends MappedNullableLong(this)
  object notNull extends MappedString(this, 32) {
    override def dbNotNull_? = true
  }

  def encodeAsJson(): JsonAST.JObject = SampleModel.encodeAsJson(this)
}

object SampleTagSnake extends SampleTagSnake with LongKeyedMetaMapper[SampleTagSnake] {
  override def dbAddTable = Full(populate _)
  private def populate {
    val samp = SampleModelSnake.findAll()
    val tags = List("Hello", "Moose", "Frog", "WooHoo", "Sloth",
                    "Meow", "Moof")
    for (t <- tags;
         m <- samp) SampleTagSnake.create.tag(t).model(m).save
  }

   override def dbDefaultConnectionIdentifier = DBProviders.SnakeConnectionIdentifier
}

class SampleTagSnake extends LongKeyedMapper[SampleTagSnake] with IdPK {
  def getSingleton = SampleTagSnake // what's the "meta" server

  object tag extends MappedString(this, 32)

  object model extends MappedLongForeignKey(this, SampleModelSnake)

  object extraColumn extends MappedString(this, 32) {
    override def dbColumnName = "AnExtraColumn"
  }
}

object SampleModelSnake extends SampleModelSnake with KeyedMetaMapper[Long, SampleModelSnake] {
  override def dbAddTable = Full(populate _)

  def encodeAsJson(in: SampleModelSnake): JsonAST.JObject = encodeAsJSON_!(in)
  def buildFromJson(json: JsonAST.JObject): SampleModelSnake = decodeFromJSON_!(json, false)

  private def populate {
    create.firstName("Elwood").save
    create.firstName("Madeline").save
    create.firstName("Archer").save
    create.firstName("NotNull").moose(Full(99L)).save
  }

  override def dbDefaultConnectionIdentifier = DBProviders.SnakeConnectionIdentifier
}

class SampleModelSnake extends KeyedMapper[Long, SampleModelSnake] {
  def getSingleton = SampleModelSnake // what's the "meta" server
  def primaryKeyField = id

  object id extends MappedLongIndex(this)
  object firstName extends MappedString(this, 32)
  object moose extends MappedNullableLong(this)
  object notNull extends MappedString(this, 32) {
    override def dbNotNull_? = true
  }

  def encodeAsJson(): JsonAST.JObject = SampleModelSnake.encodeAsJson(this)
}

/**
 * The singleton that has methods for accessing the database
 */
object User extends User with MetaMegaProtoUser[User] {
  override def dbAddTable = Full(populate _)
  private def populate {
    create.firstName("Elwood").save
    create.firstName("Madeline").save
    create.firstName("Archer").save
  }

  override def dbTableName = "users" // define the DB table name
  override def screenWrap = Full(<lift:surround with="default" at="content">
      <lift:bind /></lift:surround>)
  // define the order fields will appear in forms and output
  override def fieldOrder = List(id, firstName, lastName, email,
                                 locale, timezone, password, textArea)

  // comment this line out to require email validations
  override def skipEmailValidation = true
}


/**
 * An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it
 */
class User extends MegaProtoUser[User] {
  def getSingleton = User // what's the "meta" server

  // define an additional field for a personal essay
  object textArea extends MappedTextarea(this, 2048) {
    override def textareaRows  = 10
    override def textareaCols = 50
    override def displayName = "Personal Essay"
  }
}

class Dog extends LongKeyedMapper[Dog] with IdPK {
  def getSingleton = Dog

  object name extends MappedPoliteString(this, 128)
  object weight extends MappedInt(this)
  object owner extends MappedLongForeignKey(this,User)
}

object Dog extends Dog with LongKeyedMetaMapper[Dog] {
  override def dbAddTable = Full(populate _)

  private def populate {
    create.name("Elwood").save
    create.name("Madeline").save
    create.name("Archer").save
    create.name("fido").owner(User.find(By(User.firstName, "Elwood"))).save
  }

  def who(in: Dog): Box[User] = in.owner
}

class Mixer extends LongKeyedMapper[Mixer] with IdPK {
  def getSingleton = Mixer


  object name extends MappedPoliteString(this, 128) {
    override def dbColumnName = "NaM_E"
    override def defaultValue = "wrong"
  }
  object weight extends MappedInt(this) {
    override def dbColumnName = "WEIGHT"
    override def defaultValue = -99
  }

}

object Mixer extends Mixer with LongKeyedMetaMapper[Mixer] {
  override def dbAddTable = Full(populate _)

  override def dbTableName = "MIXME_UP"

  private def populate {
    create.name("Elwood").weight(33).save
    create.name("Madeline").weight(44).save
    create.name("Archer").weight(105).save
  }
}

/**
 * Test class to see if you can have a non-autogenerated primary key
 * Issue 552
 */
class TstItem extends LongKeyedMapper[TstItem] {
  def getSingleton = TstItem
  
  def primaryKeyField = tmdbId
  
  object tmdbId extends MappedLongIndex(this) {
    override def writePermission_? = true
    override def dbAutogenerated_? = false
  }
  
  object name extends MappedText(this)	
}

object TstItem extends TstItem with LongKeyedMetaMapper[TstItem]


class Dog2 extends LongKeyedMapper[Dog2]  with CreatedUpdated {
  def getSingleton = Dog2

  override def primaryKeyField = dog2id

  object dog2id extends MappedLongIndex[Dog2](this.asInstanceOf[MapperType]) {
    override def dbColumnName = "DOG2_Id"
  }

  object name extends MappedPoliteString(this, 128)
  object weight extends MappedInt(this)
  object owner extends MappedLongForeignKey(this,User)
  object actualAge extends MappedInt(this) {
    override def dbColumnName = "ACTUAL_AGE"
    override def defaultValue = 1
    override def dbIndexed_? = true
  }
  object isDog extends MappedBoolean(this) {
    override def dbColumnName = "is_a_dog"
    override def defaultValue = false
    override def dbIndexed_? = true
  }
  object createdTime extends MappedDateTime(this) {
    override def dbColumnName = "CreatedTime"
    override def defaultValue = new _root_.java.util.Date()
    override def dbIndexed_? = true
  }
}

object Dog2 extends Dog2 with LongKeyedMetaMapper[Dog2] {
  override def dbTableName = "DOG2"
  override def dbAddTable = Full(populate _)

  private def populate {
    create.name("Elwood").actualAge(66).save
    create.name("Madeline").save
    create.name("Archer").save
    create.name("fido").owner(User.find(By(User.firstName, "Elwood"))).isDog(true).save
    create.name("toto").owner(User.find(By(User.firstName, "Archer"))).actualAge(3).isDog(true).createdTime(Dog2.getRefDate).save
  }

  // Get new instance of fixed point-in-time reference date
  def getRefDate: _root_.java.util.Date = {
    new _root_.java.util.Date(1257089309453L)
  }
}
}
}

