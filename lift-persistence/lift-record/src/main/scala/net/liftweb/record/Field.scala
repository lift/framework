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
import net.liftweb.http.js.{JsExp}
import net.liftweb.json.JsonAST.{JNothing, JNull, JString, JValue}
import net.liftweb.util._
import scala.reflect.Manifest
import scala.xml._

/** Base trait of record fields, with functionality common to any type of field owned by any type of record */
trait BaseField extends FieldIdentifier with util.BaseField {
  private[record] var fieldName: String = _
  private[record] var dirty = false

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
   * Can the value of this field be read without obscuring the result?
   */
  def canRead_? : Boolean = safe_? || checkCanRead_?

  /**
   * If the owner is not in "safe" mode, check the current environment to see if
   * the field can be read
   */
  def checkCanRead_? = true

  /**
   * Can the value of this field be written?
   */
  def canWrite_? : Boolean = safe_? || checkCanWrite_?

  /**
   * If the owner is not in "safe" mode, check the current environment to see if
   * the field can be written
   */
  def checkCanWrite_? = true

  /**
   * Convert the field value to an XHTML representation
   */
  def toXHtml: NodeSeq = Text(toString)

  /**
   * Generate a form control for the field
   */
  def toForm: Box[NodeSeq]

  /**
   * Returns the field's value as a valid JavaScript expression
   */
  def asJs: JsExp

  /** Encode the field value into a JValue */
  def asJValue: JValue

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

  def asString: String

  def safe_? : Boolean = true // let owned fields make it unsafe some times
}

/** Refined trait for fields owned by a particular record type */
trait OwnedField[OwnerType <: Record[OwnerType]] extends BaseField {
  /**
   * Return the owner of this field
   */
  def owner: OwnerType

  /**
   * Are we in "safe" mode (i.e., the value of the field can be read or written without any security checks.)
   */
  override final def safe_? : Boolean = owner.safe_?
}

/** Refined trait for fields holding a particular value type */
trait TypedField[ThisType] extends BaseField {
  type MyType = ThisType // For backwards compatability

  type ValidationFunction = ValueType => List[FieldError]

  private[record] var data: Box[MyType] = Empty
  private[record] var needsDefault: Boolean = true
  
  /**
   * Helper for implementing asJValue for a conversion to an encoded JString
   *
   * @param encode function to transform the field value into a String
   */
  protected def asJString(encode: MyType => String): JValue =
    valueBox.map(v => JString(encode(v))) openOr (JNothing: JValue)

  /** Decode the JValue and set the field to the decoded value. Returns Empty or Failure if the value could not be set */
  def setFromJValue(jvalue: JValue): Box[MyType]

  /**
   * Helper for implementing setFromJValue for a conversion from an encoded JString
   *
   * @param decode function to try and transform a String into a field value
   */
  protected def setFromJString(jvalue: JValue)(decode: String => Box[MyType]): Box[MyType] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JString(s)                   => setBox(decode(s))
    case other                        => setBox(FieldHelpers.expectedA("JString", other))
  }

  def validations: List[ValidationFunction] = Nil

  /** Validate this field's setting, returning any errors found */
  def validate: List[FieldError] = runValidation(valueBox)

  /** Helper function that does validation of a value by using the validators specified for the field */
  protected def runValidation(in: Box[MyType]): List[FieldError] = in match {
    case Full(_) => validations.flatMap(_(toValueType(in))).distinct
    case Empty => Nil
    case Failure(msg, _, _) => Text(msg)
  }

  protected implicit def boxNodeToFieldError(in: Box[Node]): List[FieldError] =
    in match {
      case Full(node) => List(FieldError(this, node))
      case _ => Nil
    }

  protected implicit def nodeToFieldError(node: Node): List[FieldError] =
    List(FieldError(this, node))

  protected implicit def boxNodeFuncToFieldError(in: Box[MyType] => Box[Node]):
  Box[MyType] => List[FieldError] =
    param => boxNodeToFieldError(in(param))

  /** The default value of the field when no value is set. Must return a Full Box unless optional_? is true */
  def defaultValueBox: Box[MyType]

  /**
   * Convert the field to a String... usually of the form "displayName=value"
   */
  def asString = displayName + "=" + data

  def obscure(in: MyType): Box[MyType] = Failure("value obscured")

  def setBox(in: Box[MyType]): Box[MyType] = synchronized {
    needsDefault = false
    data = in match {
      case _ if !checkCanWrite_? => Failure(noValueErrorMessage)
      case Full(_)               => set_!(in)
      case _ if optional_?       => set_!(in)
      case (f: Failure)          => set_!(f) // preserve failures set in
      case _                     => Failure(notOptionalErrorMessage)
    }
    dirty_?(true)
    data
  }

  // Helper methods for things to easily use mixins and so on that use ValueType instead of Box[MyType], regardless of the optional-ness of the field
  protected def toValueType(in: Box[MyType]): ValueType
  protected def toBoxMyType(in: ValueType): Box[MyType]

  protected def set_!(in: Box[MyType]): Box[MyType] = runFilters(in, setFilterBox)

  protected def setFilter: List[ValueType => ValueType] = Nil

  /** OptionalTypedField and MandatoryTypedField implement this to do the appropriate lifting of Box[MyType] to ValueType */
  protected def liftSetFilterToBox(in: Box[MyType]): Box[MyType]

  /**
   * A list of functions that transform the value before it is set.  The transformations
   * are also applied before the value is used in a query.  Typical applications
   * of this are trimming and/or toLowerCase-ing strings
   */
  protected def setFilterBox: List[Box[MyType] => Box[MyType]] = liftSetFilterToBox _ :: Nil

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

  def valueBox: Box[MyType] = synchronized {
    if (needsDefault) {
      needsDefault = false
      data = defaultValueBox
    }

    if (canRead_?) data
    else data.flatMap(obscure)
  }

  /** Clear the value of this field */
  def clear: Unit = optional_? match {
    case true  => setBox(Empty)
    case false => setBox(defaultValueBox)
  }
}

trait MandatoryTypedField[ThisType] extends TypedField[ThisType] with Product1[ThisType] {
  type ValueType = ThisType // For util.BaseField

  //TODO: fullfil the contract of Product1[ThisType]
  def canEqual(a:Any) = false
  
  def _1 = value

  override def optional_? = false

  /**
   * Set the value of the field to the given value.
   * Note: Because setting a field can fail (return non-Full), this method will
   * return defaultValue if the field could not be set.
   */
  def set(in: MyType): MyType = setBox(Full(in)) openOr defaultValue

  def toValueType(in: Box[MyType]) = in openOr defaultValue
  def toBoxMyType(in: ValueType) = Full(in)

  def value: MyType = valueBox openOr defaultValue

  def get: MyType = value
  def is: MyType = value

  protected def liftSetFilterToBox(in: Box[MyType]): Box[MyType] = in.map(v => setFilter.foldLeft(v)((prev, f) => f(prev)))

  /**
   * The default value of the field when a field has no value set and is optional, or a method that must return a value (e.g. value) is used
   */
  def defaultValue: MyType

  def defaultValueBox: Box[MyType] = if (optional_?) Empty else Full(defaultValue)

  override def toString = valueBox match {
    case Full(null)|null => "null"
    case Full(v) => v.toString
    case _ => defaultValueBox.map(v => if (v != null) v.toString else "null") openOr ""
  }
}
  
trait OptionalTypedField[ThisType] extends TypedField[ThisType] with Product1[Box[ThisType]] {
  type ValueType = Option[ThisType] // For util.BaseField

  //TODO: fullfil the contract of Product1[ThisType]
  def canEqual(a:Any) = false
  
  def _1 = value

  final override def optional_? = true

  /**
   * Set the value of the field to the given value.
   * Note: Because setting a field can fail (return non-Full), this method will
   * return defaultValueBox if the field could not be set.
   */
  def set(in: Option[MyType]): Option[MyType] = setBox(in) or defaultValueBox

  def toValueType(in: Box[MyType]) = in
  def toBoxMyType(in: ValueType) = in

  def value: Option[MyType] = valueBox

  def get: Option[MyType] = value
  def is: Option[MyType] = value

  protected def liftSetFilterToBox(in: Box[MyType]): Box[MyType] = setFilter.foldLeft(in)((prev, f) => f(prev))


  def defaultValueBox: Box[MyType] = Empty

  override def toString = valueBox match {
    case Full(null)|null => "null"
    case Full(v) => v.toString
    case _ => defaultValueBox.map(v => if (v != null) v.toString else "null") openOr ""
  }

}

/**
 * A simple field that can store and retreive a value of a given type
 */
trait Field[ThisType, OwnerType <: Record[OwnerType]] extends OwnedField[OwnerType] with TypedField[ThisType] {

  def apply(in: MyType): OwnerType = apply(Full(in))

  def apply(in: Box[MyType]): OwnerType = if (owner.meta.mutable_?) {
    this.setBox(in)
    owner
  } else {
    owner.meta.createWithMutableField(owner, this, in)
  }
}

/**
 * Mix in to a field to change its form display to be formatted with the label aside.
 *
 * E.g.
 *   <div id={ id + "_holder" }>
 *     <div><label for={ id + "_field" }>{ displayName }</label></div>
 *     { control }
 *   </div>
 */
trait DisplayWithLabel[OwnerType <: Record[OwnerType]] extends OwnedField[OwnerType] {
  override abstract def toForm: Box[NodeSeq] =
    for (id <- uniqueFieldId; control <- super.toForm)
    yield
      <div id={ id + "_holder" }>
        <div><label for={ id + "_field" }>{ displayName }</label></div>
        { control }
        <lift:msg id={id} errorClass="lift_error"/>
      </div>
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


object FieldHelpers {
  def expectedA(what: String, notA: AnyRef): Failure = Failure("Expected a " + what + ", not a " + (if (notA == null) "null" else notA.getClass.getName))
}

}
}
