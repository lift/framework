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
import net.liftweb.http.js.{JsExp}
import scala.reflect.Manifest
import scala.xml._


trait OwnedField[OwnerType <: Record[OwnerType]] extends FieldIdentifier {
  private[record] var needsDefault = true
  private[record] var dirty = false
  private[record] var fieldName: String = _

  type MyType
  type ValidationFunction = Box[MyType] => Box[Node]

  /**
   * Return the owner of this field
   */
  def owner: OwnerType

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
   * Is the value of this field optional (e.g. NULLable)?
   */
  def optional_? = false

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
   * The error message used when the field value could not be set
   */
  def noValueErrorMessage : String = "Value cannot be changed"

  /**
   * The error message used when the field value must be set
   */
  def notOptionalErrorMessage : String = "Value required"


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



  private[record] var data: Box[MyType] = Empty


  def apply(in: MyType): OwnerType

  /**
   * The default value of the field when a field has no value set and is optional, or a method that must return a value (e.g. value) is used
   */
  def defaultValue: MyType

  /** The default value of the field when no value is set. Must return a Full Box unless optional_? is true */
  def defaultValueBox: Box[MyType] = if (optional_?) Empty else Full(defaultValue)


  /**
   * Convert the field to a String... usually of the form "displayName=value"
   */
  def asString = displayName + "=" + data

  def obscure(in: MyType): Box[MyType] = Failure("value obscured")

  /**
   * Set the value of the field to the given value.
   * Note: Because setting a field can fail (return non-Full), this method will
   * return defaultValue if the field could not be set.
   */
  def set(in: MyType): MyType = setBox(Full(in)) openOr defaultValue

  def setBox(in: Box[MyType]): Box[MyType] = synchronized {
    needsDefault = false
    data = in match {
      case _ if !checkCanWrite_? => Failure(noValueErrorMessage)
      case Full(_)               => set_!(in)
      case _ if optional_?       => set_!(in)
      case _                     => Failure(notOptionalErrorMessage)
    }
    dirty_?(true)
    data
  }

  protected def set_!(in: Box[MyType]): Box[MyType] = runFilters(in, setFilter)

  /**
   * A list of functions that transform the value before it is set.  The transformations
   * are also applied before the value is used in a query.  Typical applications
   * of this are trimming and/or toLowerCase-ing strings
   */
  protected def setFilter: List[Box[MyType] => Box[MyType]] = Nil

  def runFilters(in: Box[MyType], filter: List[Box[MyType] => Box[MyType]]): Box[MyType] = filter match {
    case Nil => in
    case x :: xs => runFilters(x(in), xs)
  }

  /**
   * Set the value of the field from anything.
   * Implementations of this method should accept at least the following (pattern => valueBox)
   *   - value: MyType       => setBox(Full(value))
   *   - Some(value: MyType) => setBox(Full(value))
   *   - Full(value: MyType) => setBox(Full(value))
   *   - (value: MyType)::_  => setBox(Full(value))
   *   - s: String           => setFromString(s)
   *   - Some(s: String)     => setFromString(s)
   *   - Full(s: String)     => setFromString(s)
   *   - null|None|Empty     => setBox(defaultValueBox)
   *   - f: Failure          => setBox(f)
   * And usually convert the input to a string and uses setFromString as a last resort.
   * 
   * Note that setFromAny should _always_ call setBox, even if the conversion fails. This is so that validation
   * properly notes the error.
   *
   * The method genericSetFromAny implements this guideline.
   */
  def setFromAny(in: Any): Box[MyType]

  /** Generic implementation of setFromAny that implements exactly what the doc for setFromAny specifies, using a Manifest to check types */
  protected final def genericSetFromAny(in: Any)(implicit m: Manifest[MyType]): Box[MyType] = in match {
    case value       if m.erasure.isInstance(value) => setBox(Full(value.asInstanceOf[MyType]))
    case Some(value) if m.erasure.isInstance(value) => setBox(Full(value.asInstanceOf[MyType]))
    case Full(value) if m.erasure.isInstance(value) => setBox(Full(value.asInstanceOf[MyType]))
    case (value)::_  if m.erasure.isInstance(value) => setBox(Full(value.asInstanceOf[MyType]))
    case     (value: String) => setFromString(value)
    case Some(value: String) => setFromString(value)
    case Full(value: String) => setFromString(value)
    case (value: String)::_  => setFromString(value)
    case null|None|Empty     => setBox(defaultValueBox)
    case (failure: Failure)  => setBox(failure)
    case other               => setFromString(String.valueOf(other))
  }

  /**
   * Set the value of the field using some kind of type-specific conversion from a String.
   * By convention, if the field is optional_?, then the empty string should be treated as no-value (Empty).
   * Note that setFromString should _always_ call setBox, even if the conversion fails. This is so that validation
   * properly notes the error.
   * 
   * @return Full(convertedValue) if the conversion succeeds (the field value will be set by side-effect)
   *         Empty or Failure if the conversion does not succeed
   */
  def setFromString(s: String): Box[MyType]

  def value: MyType = valueBox openOr defaultValue

  def valueBox: Box[MyType] = synchronized {
    if (needsDefault) {
      data = defaultValueBox
      needsDefault = false
    }

    if (canRead_?) data
    else data.flatMap(obscure)
  }

  override def toString = value match {
    case null => "null"
    case s => s.toString
  }

  /** Clear the value of this field */
  def clear: Unit = optional_? match {
    case true  => setBox(Empty)
    case false => needsDefault = true
  }
}

/**
 * A simple field that can store and retreive a value of a given type
 */
trait Field[ThisType, OwnerType <: Record[OwnerType]] extends OwnedField[OwnerType] {
  type MyType = ThisType


  def apply(in: MyType): OwnerType = apply(Full(in))

  def apply(in: Box[MyType]): OwnerType = if (owner.meta.mutable_?) {
    this.setBox(in)
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
  def ===(other: KeyField[MyType, OwnerType]): Boolean = this.valueBox == other.valueBox
}

}
}
