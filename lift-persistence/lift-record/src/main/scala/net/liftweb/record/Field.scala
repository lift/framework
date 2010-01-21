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
package record {

import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.http.{S}
import net.liftweb.http.js.{JsExp}
import scala.xml._


trait OwnedField[OwnerType <: Record[OwnerType]] extends FieldIdentifier {
  private[record] var valueCouldNotBeSet = false
  private[record] var needsDefault = true
  private[record] var dirty = false
  private[record] var fieldName: String = _

  type MyType
  type ValidationFunction = MyType => Box[Node]

  /**
   * Return the owner of this field
   */
  def owner: OwnerType

  def couldNotSetValue = valueCouldNotBeSet = true

  protected def dirty_?(b: Boolean) = dirty = b

  def resetDirty {
    if (safe_?) dirty_?(false)
  }

  def dirty_? : Boolean = dirty

  /**
   * Should the field be ignored by the OR Mapper?
   */
  def ignoreField_? = false

  /**
   * The text name of this field
   */
  def name: String = fieldName

  /**
   * The display name of the field (by default, the 'internal' name of the field)
   */
  def displayName = name

  /**
   * Can the value of this field be read without obscuring the result?
   */
  def canRead_? = owner.safe_? || checkCanRead_?

  /**
   * If the owner is not in "safe" mode, check the current environment to see if
   * the field can be read
   */
  def checkCanRead_? = true

  def canWrite_? = owner.safe_? || checkCanWrite_?

  def checkCanWrite_? = true

  /**
   * Convert the field value to an XHTML representation
   */
  def toXHtml: NodeSeq = Text(toString)

  def toForm: NodeSeq

  def asXHtml: NodeSeq

  /**
   * Returns the field's value as a valid JavaScript expression
   */
  def asJs: JsExp

  /**
   * Are we in "safe" mode (i.e., the value of the field can be read or written without any security checks.)
   */
  final def safe_? : Boolean = owner.safe_?

  /**
   * Set the name of this field
   */
  private[record] final def setName_!(newName : String) : String = {
    if (safe_?) fieldName = newName
    fieldName
  }

  /**
   * The error message used when the fiel value could not be set
   */
  def noValueErrorMessage : String = ""

  def tabIndex: Int = 1

  override def uniqueFieldId: Box[String] = Full(name+"_id")


  def label: NodeSeq = uniqueFieldId match {
    case Full(id) =>  <label for={id+"_field"}>{displayName}</label>
    case _ => NodeSeq.Empty
  }

  /**
   * Return a list of functions that will be subsequently called for validating this field.
   * Each function takes a field-type parameter and returns a Box[Node].
   *
   * The field values is valid if all validation functions return an Empty Box
   */
  def validators: List[ValidationFunction] = Nil



  private[record] var data: MyType = _

  private[record] var obscured: MyType = _


  def apply(in: MyType): OwnerType

  /**
   * The default value of the field
   */
  def defaultValue: MyType


  /**
   * Convert the field to a String... usually of the form "displayName=value"
   */
  def asString = displayName + "=" + data

  def obscure(in: MyType): MyType = obscured

  def set(in: MyType): MyType = synchronized {
    if (checkCanWrite_?) {
      data = set_!(in)
      valueCouldNotBeSet = false
      needsDefault = false
    } else {
      valueCouldNotBeSet = true
      needsDefault = false
    }
    data
  }

  protected def set_!(in: MyType) = runFilters(in, setFilter)

  /**
   * A list of functions that transform the value before it is set.  The transformations
   * are also applied before the value is used in a query.  Typical applications
   * of this are trimming and/or toLowerCase-ing strings
   */
  protected def setFilter: List[MyType => MyType] = Nil

  def runFilters(in: MyType, filter: List[MyType => MyType]): MyType = filter match {
    case Nil => in
    case x :: xs => runFilters(x(in), xs)
  }

  def setFromAny(in: Any): Box[MyType]

  def setFromString(s: String): Box[MyType]

  def value: MyType = synchronized {
    if (needsDefault) {
      data = defaultValue;
      needsDefault = false
    }

    if (canRead_?) data
    else obscure(data)
  }

  override def toString = value match {
    case null => "null"
    case s => s.toString
  }
}

/**
 * A simple field that can store and retreive a value of a given type
 */
trait Field[ThisType, OwnerType <: Record[OwnerType]] extends OwnedField[OwnerType] {
  type MyType = ThisType


  def apply(in: MyType): OwnerType = if (owner.meta.mutable_?) {
    this.set(in)
    owner
  } else {
    owner.meta.createWithMutableField(owner, this, in)
  }
}

import _root_.java.sql.{ResultSet, Types}
import net.liftweb.mapper.{DriverType}

/**
 * Desribes common aspects related with JDBC
 */
trait JDBCFieldFlavor[MyType] {

  def jdbcFriendly(field : String) : MyType

  def targetSQLType : Int

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String
}

trait KeyField[MyType, OwnerType <: Record[OwnerType] with KeyedRecord[OwnerType, MyType]] extends Field[MyType, OwnerType] {
  def ===(other: KeyField[MyType, OwnerType]): Boolean = this.value == other.value
}

}
}
