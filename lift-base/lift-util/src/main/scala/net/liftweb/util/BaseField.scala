/*
 * Copyright 2006-2010 WorldWide Conferencing, LLC
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
package util {

import common._
import xml.NodeSeq

/**
 * Defines the association of this reference with an markup tag ID
 */
trait FieldIdentifier {
  def uniqueFieldId: Box[String] = Empty
}

/**
 * Associate a FieldIdentifier with an NodeSeq
 */
case class FieldError(field: FieldIdentifier, msg: NodeSeq) {
  override def toString = field.uniqueFieldId + " : " + msg
}

object FieldError {
  import scala.xml.Text
  def apply(field: FieldIdentifier, msg: String) = new FieldError(field, Text(msg))
}

trait FieldContainer {
  def allFields: Seq[BaseField]
}

/**
 * A field that can be displayed but not edited
 */
trait ReadableField extends FieldIdentifier with ValueHolder with Bindable {
  import scala.xml.Text

  /**
   * The human name of this field
   */
  def name: String

  def displayNameHtml: Box[NodeSeq] = Empty

  def displayHtml: NodeSeq = displayNameHtml openOr Text(displayName)

  /**
   * The display name of this field (e.g., "First Name")
   */
  def displayName: String = name
  
  /**
   * Default read-only rendering of field
   */
  def asHtml: NodeSeq = Text(is.toString)

  /**
   * Given the current context, should this field be displayed
   */
  def shouldDisplay_? = true
}

/**
 * A field that can be set
 */
trait SettableField extends ReadableField with SettableValueHolder {
  /**
   * A list of functions that transform the value before it is set.  The transformations
   * are also applied before the value is used in a query.  Typical applications
   * of this are trimming and/or toLowerCase-ing strings
   */
  protected def setFilter: List[ValueType => ValueType]

  def validations: List[ValueType => List[FieldError]]

  /**
   * A unique 'id' for the field for form generation
   */
  def fieldId: Option[NodeSeq] = None

  /**
   * Is the Field required (and will have a style designating it as such)
   */
  def required_? = false

  /**
   * Is this an upload field so that a form that includes this field must be multi-part mime
   */
  def uploadField_? = false

  /**
   * Validate this field and return a list of Validation Issues
   */
  def validate: List[FieldError]
  def helpAsHtml: Box[NodeSeq] = Empty

  /**
   * Create an input field for the item
   */
  def toForm: Box[NodeSeq]

 /**
  * Give the current state of things, should the this field be shown
  */
  def show_? = true
}

trait BaseField extends SettableField with FieldContainer {
  def allFields: Seq[BaseField] = List(this)
}

trait StringValidators {
  self: FieldIdentifier =>

  import scala.xml.Text
  import java.util.regex.Pattern
    
  type ValueType

  protected def valueTypeToBoxString(in: ValueType): Box[String]
  protected def boxStrToValType(in: Box[String]): ValueType

  def maxLen: Int

  def crop(in: ValueType): ValueType = 
    boxStrToValType(valueTypeToBoxString(in).map{
      case null => null
      case s => s.substring(0, Math.min(s.length, maxLen))
    })

  def removeRegExChars(regEx: String)(in: ValueType): ValueType= 
    boxStrToValType(valueTypeToBoxString(in).map{
      case null => null
      case s => s.replaceAll(regEx, "")
    })

  def toLower(in: ValueType): ValueType = 
    boxStrToValType(valueTypeToBoxString(in).map{
      case null => null
      case s => s.toLowerCase
    })

  def toUpper(in: ValueType): ValueType = 
    boxStrToValType(valueTypeToBoxString(in).map{
      case null => null
      case s => s.toUpperCase
    })

  def trim(in: ValueType): ValueType = 
    boxStrToValType(valueTypeToBoxString(in).map{
      case null => null
      case s => s.trim
    })

  def notNull(in: ValueType): ValueType = 
    boxStrToValType(valueTypeToBoxString(in) match {
      case Full(str) if null ne str => Full(str)
      case _ => Full("")
    })

  /**
   * A validation helper.  Make sure the string is at least a particular
   * length and generate a validation issue if not
   */
  def valMinLen(len: Int, msg: => String)(value: ValueType): List[FieldError] = 
    valueTypeToBoxString(value) match {
      case Full(str) if (null ne str) && str.length >= len => Nil
      case _ => List(FieldError(this, Text(msg)))
    }


  /**
   * A validation helper.  Make sure the string is no more than a particular
   * length and generate a validation issue if not
   */
  def valMaxLen(len: Int, msg: => String)(value: ValueType): List[FieldError] =
    valueTypeToBoxString(value) match {
      case Full(str) if (null ne str) && str.length <= len => Nil
      case _ =>  List(FieldError(this, Text(msg)))
    }

  /**
   * Make sure the field matches a regular expression
   */
  def valRegex(pat: Pattern, msg: => String)(value: ValueType): List[FieldError] =
    valueTypeToBoxString(value).flatMap{str => if (pat.matcher(str).matches) Full(true) else Empty} match {
      case Full(true) => Nil
      case _ => List(FieldError(this, Text(msg)))
    }
}

}
}
