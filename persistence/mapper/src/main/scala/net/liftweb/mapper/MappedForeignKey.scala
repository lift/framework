/*
 * Copyright 2006-2011 WorldWide Conferencing, LLC
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

import common._


import scala.xml.{NodeSeq, Text, Elem}
import http.{js, S, SHtml}
import js._
import S.?
import json._
import util.FieldError


/**
 * A trait that defines foreign key references
 */
trait BaseForeignKey extends BaseMappedField {

  type KeyType
  type KeyedForeignType <: KeyedMapper[KeyType, KeyedForeignType]

  type OwnerType <: Mapper[OwnerType]

  /**
   * Is the key defined?
   */
  def defined_? : Boolean

  /**
   * get the object referred to by this foreign key
   */

  def dbKeyToTable: BaseMetaMapper

  def dbKeyToColumn: BaseMappedField

  def findFor(key: KeyType): List[OwnerType]

  def findFor(key: KeyedForeignType): List[OwnerType]

  /**
   * Called when Schemifier adds a foreign key.  Return a function that will be called when Schemifier
   * is done with the schemification.
   */
  def dbAddedForeignKey: Box[() => Unit]
}


object MappedForeignKey {
  implicit def getObj[KeyType,
          MyOwner <: Mapper[MyOwner],
          Other <: KeyedMapper[KeyType,
             Other]](in:
               MappedForeignKey[KeyType,
                    MyOwner,
                    Other]):
  Box[Other] = in.obj
}

/**
 * The Trait that defines a field that is mapped to a foreign key
 */
trait MappedForeignKey[KeyType, MyOwner <: Mapper[MyOwner], Other <: KeyedMapper[KeyType, Other]]
extends MappedField[KeyType, MyOwner]
with LifecycleCallbacks {
  type FieldType <: KeyType
  // type ForeignType <: KeyedMapper[KeyType, Other]

  /**
   * What's the MetaMapper for the foreign key
   */
  def foreignMeta: KeyedMetaMapper[KeyType, Other]

  /**
   * Make sure the MetaMapper for the KeyedMapper we're checking
   * is in fact the same one as we are associated with.  Issue #532.
   */
  private def checkTypes(km: KeyedMapper[KeyType, _]): Boolean =
    km.getSingleton eq foreignMeta

  override def equals(other: Any) = other match {
    case km: KeyedMapper[KeyType, Other] if checkTypes(km) => this.get == km.primaryKeyField.get
    case _ => super.equals(other)
  }

  def dbKeyToTable: KeyedMetaMapper[KeyType, Other]

  def validSelectValues: Box[List[(KeyType, String)]] = Empty


  def immutableMsg: NodeSeq = Text(?("Can't change"))

  override def _toForm: Box[Elem] = Full(validSelectValues.flatMap{
      case Nil => Empty

      case xs =>
        Full(SHtml.selectObj(xs, Full(this.get), this.set))
    }.openOr(<span>{immutableMsg}</span>))

  /**
   * Is the key defined
   */
  def defined_? : Boolean

  /**
   * Is the obj field cached
   */
  def cached_? : Boolean = synchronized{ _calcedObj}

  override protected def dirty_?(b: Boolean) = synchronized { // issue 165
    // invalidate if the primary key has changed Issue 370
    if (_obj.isEmpty || (_calcedObj && _obj.isDefined &&
       _obj.openOrThrowException("_obj was just checked as full.").primaryKeyField.get != this.i_is_!)) {
      _obj = Empty
      _calcedObj = false
    }
    super.dirty_?(b)
  }

  /**
   * Some people prefer the name foreign to materialize the
   * foreign reference.  This is a proxy to the obj method.
   */
  def foreign: Box[Other] = obj

  /**
   * Load and cache the record that this field references
   */
  def obj: Box[Other] = synchronized {
    if (!_calcedObj) {
      _calcedObj = true
      this._obj = if(defined_?) dbKeyToTable.find(i_is_!) else Empty
    }
    _obj
  }

  private[mapper] def _primeObj(obj: Box[Any]) =
    primeObj(obj.asInstanceOf[Box[Other]])

  /**
   * Prime the reference of this FK reference
   */
  def primeObj(obj: Box[Other]) = synchronized {
    _obj = obj
    _calcedObj = true
  }

  private var _obj: Box[Other] = Empty
  private var _calcedObj = false


  /**
   * Set the value from a possible instance of the foreign mapper class.
   * v will be cached in obj.
   * If v is Empty, set the value to defaultValue (-1)
   * @return the Mapper containing this field
   */
  def apply(v: Box[Other]): MyOwner = {
    apply(v.dmap(defaultValue)(_.primaryKeyField.get))
    primeObj(v)
    fieldOwner
  }

  /**
   * Set the value from an instance of the foreign mapper class.
   * obj will be set to Full(v)
   * @return the Mapper containing this field
   */
  def apply(v: Other): MyOwner = {
    apply(v.primaryKeyField.get)
    primeObj(Full(v))
    fieldOwner
  }

  /**
   * This method, which gets called when the mapper class is going to be saved,
   * sets the field's value from obj if it's set to the default (!defined_?).
   * Overrides LifecycleCallbacks.beforeSave
   */
  override def beforeSave {
    if(!defined_?)
      for(o <- obj)
        set(o.primaryKeyField.get)
    super.beforeSave
  }

  /**
   * A validation function that checks that obj is nonempty
   */
  val valHasObj = (value: Long) =>
    if (obj.isEmpty) List(FieldError(this, scala.xml.Text("Required field: " + name)))
    else Nil
}


abstract class MappedLongForeignKey[T<:Mapper[T],O<:KeyedMapper[Long, O]](theOwner: T, _foreignMeta: => KeyedMetaMapper[Long, O])
extends MappedLong[T](theOwner) with MappedForeignKey[Long,T,O] with BaseForeignKey {
  def defined_? = i_is_! > 0L

  def foreignMeta = _foreignMeta

  def box: Box[Long] = if (defined_?) Full(get) else Empty

  type KeyType = Long
  type KeyedForeignType = O
  type OwnerType = T

  override def jdbcFriendly(field : String) = if (defined_?) new java.lang.Long(i_is_!) else null
  override def jdbcFriendly = if (defined_?) new java.lang.Long(i_is_!) else null

  lazy val dbKeyToTable: KeyedMetaMapper[Long, O] = foreignMeta

  def dbKeyToColumn = dbKeyToTable.primaryKeyField

  override def dbIndexed_? = true

  override def dbForeignKey_? = true


  def asSafeJs(obs: Box[KeyObfuscator]): JsExp =
  obs.map(o => JE.Str(o.obscure(dbKeyToTable, get))).openOr(JE.Num(get))

  override def asJsExp: JsExp = if (defined_?) super.asJsExp else JE.JsNull

  override def asJsonValue: Box[JsonAST.JValue] =
    if (defined_?) super.asJsonValue else Full(JsonAST.JNull)

  override def setFromAny(in: Any): Long =
  in match {
    case JsonAST.JNull => this.set(0L)
    case JsonAST.JInt(bigint) => this.set(bigint.longValue)
    case o => super.setFromAny(o)
  }

  /**
   * Called when Schemifier adds a foreign key.  Return a function that will be called when Schemifier
   * is done with the schemification.
   */
  def dbAddedForeignKey: Box[() => Unit] = Empty

  override def toString = if (defined_?) super.toString else "NULL"

  def findFor(key: KeyType): List[OwnerType] = theOwner.getSingleton.findAll(By(this, key))

  def findFor(key: KeyedForeignType): List[OwnerType] = theOwner.getSingleton.findAll(By(this, key))

  // def +(in: Long): Long = is + in

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  override def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.longForeignKeyColumnType  + notNullAppender()

}

abstract class MappedStringForeignKey[T<:Mapper[T],O<:KeyedMapper[String, O]](override val fieldOwner: T, foreign: => KeyedMetaMapper[String, O],override val maxLen: Int)
extends MappedString[T](fieldOwner, maxLen) with MappedForeignKey[String,T,O] with BaseForeignKey {
  def defined_? = i_is_! ne null

  type KeyType = String
  type KeyedForeignType = O
  type OwnerType = T

  override def jdbcFriendly(field: String) = i_is_!
  override def jdbcFriendly = i_is_!

  def dbKeyToTable: KeyedMetaMapper[String, O] = foreign
  def dbKeyToColumn = dbKeyToTable.primaryKeyField

  override def dbIndexed_? = true

  override def dbForeignKey_? = true

  def asSafeJs(obs: Box[KeyObfuscator]): JsExp =
    obs.map(o => JE.Str(o.obscure(dbKeyToTable, get))).openOr(JE.Str(get))

  /**
   * Called when Schemifier adds a foreign key.  Return a function that will be called when Schemifier
   * is done with the schemification.
   */
  def dbAddedForeignKey: Box[() => Unit] = Empty

  override def toString = if (defined_?) super.toString else "NULL"

  def set(v: Box[O]): T = {
    val toSet: String = v match {
      case Full(i) => i.primaryKeyField.get
      case _ => null
    }

    this(toSet)
  }

  def findFor(key: KeyType): List[OwnerType] = fieldOwner.getSingleton.findAll(By(this, key))

  def findFor(key: KeyedForeignType): List[OwnerType] = fieldOwner.getSingleton.findAll(By(this, key))

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  // defect 79 override def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.longForeignKeyColumnType

}

