/*
 * Copyright 2011 WorldWide Conferencing, LLC
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

import common._
import json._
import util._
import Helpers._

/*
 * This file contains a number of objects that are common to several
 * of the Mapper specs. By placing them here we reduce code duplication
 * and get rid of some timing errors found when we moved to SBT for build.
 *
 * Derek Chen-Becker, Mar 8, 2011
 */

object MapperSpecsModel {
  // These rules are common to all Mapper specs
  def snakify(connid: ConnectionIdentifier, name: String): String = {
    if (connid.jndiName == "snake") {
      StringHelpers.snakify(name)
    } else {
      name.toLowerCase
    }
  }

  MapperRules.columnName = snakify
  MapperRules.tableName = snakify
  
  // Simple name calculator
  def displayNameCalculator(bm: BaseMapper, l: Locale, name: String) = {
    val mapperName = bm.dbName
    val displayName = name match {
      case "firstName" if l == Locale.getDefault()    => "DEFAULT:" + mapperName + "." + name
      case "firstName" if l == new Locale("xx", "YY") => "xx_YY:" + mapperName + "." + name
      case _                                          => name
    }
    displayName
  }

  MapperRules.displayNameCalculator.default.set(displayNameCalculator _)

  def setup() {
    // For now, do nothing. Just force this object to load
  }

  def doLog = false

  private def ignoreLogger(f: => AnyRef): Unit = ()

  def cleanup() {
    // Snake connection doesn't create FK constraints (put this here to be absolutely sure it gets set before Schemify)
    MapperRules.createForeignKeys_? = c => {
      c.jndiName != "snake"
    }

    Schemifier.destroyTables_!!(DefaultConnectionIdentifier, if (doLog) Schemifier.infoF _ else ignoreLogger _, SampleTag, SampleModel, Dog, Mixer, Dog2, User, TstItem, Thing)
    Schemifier.destroyTables_!!(DbProviders.SnakeConnectionIdentifier, if (doLog) Schemifier.infoF _ else ignoreLogger _, SampleTagSnake, SampleModelSnake)
    Schemifier.schemify(true, if (doLog) Schemifier.infoF _ else ignoreLogger _, DefaultConnectionIdentifier, SampleModel, SampleTag, User, Dog, Mixer, Dog2, TstItem, Thing)
    Schemifier.schemify(true, if (doLog) Schemifier.infoF _ else ignoreLogger _, DbProviders.SnakeConnectionIdentifier, SampleModelSnake, SampleTagSnake)
  }
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
  def getSingleton = SampleTag

  // what's the "meta" server
  object tag extends MappedString(this, 32)

  object model extends MappedLongForeignKey(this, SampleModel)

  object extraColumn extends MappedString(this, 32) {
    override def dbColumnName = "AnExtraColumn"
  }
}

object SampleStatus extends Enumeration {
  val Active, Disabled, Hiatus = Value
}

object SampleModel extends SampleModel with KeyedMetaMapper[Long, SampleModel] {
  override def dbAddTable = Full(populate _)

  def encodeAsJson(in: SampleModel): JsonAST.JObject = encodeAsJSON_!(in)

  def buildFromJson(json: JsonAST.JObject): SampleModel = decodeFromJSON_!(json, false)

  private def populate {
    create.firstName("Elwood").save
    create.firstName("Madeline").save
    create.firstName("Archer").status(SampleStatus.Disabled).save
    create.firstName("NotNull").moose(Full(99L)).save
  }
}

class SampleModel extends KeyedMapper[Long, SampleModel] {
  def getSingleton = SampleModel

  // what's the "meta" server
  def primaryKeyField: MappedLongIndex[SampleModel] = id

  object id extends MappedLongIndex(this)

  object firstName extends MappedString(this, 32)

  object moose extends MappedNullableLong(this)

  object notNull extends MappedString(this, 32) {
    override def dbNotNull_? = true
  }

  object status extends MappedEnum(this, SampleStatus)

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

  override def dbDefaultConnectionIdentifier = DbProviders.SnakeConnectionIdentifier
}


class SampleTagSnake extends LongKeyedMapper[SampleTagSnake] with IdPK {
  def getSingleton = SampleTagSnake

  // what's the "meta" server

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

  override def dbDefaultConnectionIdentifier = DbProviders.SnakeConnectionIdentifier
}


class SampleModelSnake extends KeyedMapper[Long, SampleModelSnake] {
  def getSingleton = SampleModelSnake

  // what's the "meta" server
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

  override def dbTableName = "users"

  // define the DB table name
  override def screenWrap = Full(<lift:surround with="default" at="content"><lift:bind/></lift:surround>)

  // define the order fields will appear in forms and output
  override def fieldOrder = List(id, firstName, lastName, email, locale, timezone, password, textArea)

  // comment this line out to require email validations
  override def skipEmailValidation = true
}


/**
 * An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it
 */
class User extends MegaProtoUser[User] {
  def getSingleton = User


  // what's the "meta" server

  // define an additional field for a personal essay
  object textArea extends MappedTextarea(this, 2048) {
    override def textareaRows = 10

    override def textareaCols = 50

    override def displayName = "Personal Essay"
  }


}


class Dog extends LongKeyedMapper[Dog] with IdPK {
  def getSingleton = Dog

  object name extends MappedPoliteString(this, 128)

  object weight extends MappedInt(this)

  object owner extends MappedLongForeignKey(this, User)

  object price extends MappedDecimal(this, new java.math.MathContext(7), 2)
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

object Thing extends Thing with KeyedMetaMapper[String, Thing] {
  override def dbTableName = "things"

  import java.util.UUID
  override def beforeCreate = List((thing: Thing) => {
    thing.thing_id(UUID.randomUUID().toString())
  })
}


class Thing extends KeyedMapper[String, Thing] {
  def getSingleton = Thing

  def primaryKeyField = thing_id

  object thing_id extends MappedStringIndex(this, 36) {
    override def writePermission_? = true
    override def dbAutogenerated_? = false
    override def dbNotNull_? = true
  }

  object name extends MappedString(this, 64)
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


class Dog2 extends LongKeyedMapper[Dog2] with CreatedUpdated {
  def getSingleton = Dog2
  override def primaryKeyField = dog2id

  object dog2id extends MappedLongIndex[Dog2](this.asInstanceOf[MapperType]) {
    override def dbColumnName = "DOG2_Id"
  }

  object name extends MappedPoliteString(this, 128)

  object weight extends MappedInt(this)

  object owner extends MappedLongForeignKey(this, User)

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
    create.name("toto").owner(User.find(By(User.firstName, "Archer"))).actualAge(3).isDog(true)
      .createdTime(Dog2.getRefDate).save
  }

  // Get new instance of fixed point-in-time reference date
  def getRefDate: _root_.java.util.Date = {
    new _root_.java.util.Date(1257089309453L)
  }
}
