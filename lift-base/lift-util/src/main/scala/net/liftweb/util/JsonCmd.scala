/*
 * Copyright 2006-2009 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */

package net.liftweb.util

import common._
import xml.{Elem, NodeSeq}

trait HasParams {
  def param(name: String): Box[String]
}


/**
 * Impersonates a JSON command
 */
case class JsonCmd(command: String, target: String, params: Any,
                   all: _root_.scala.collection.Map[String, Any])

/**
 * Holds information about a response
 */
class ResponseInfoHolder {
  var headers: Map[String, String] = Map.empty
  private var _docType: Box[String] = Empty
  private var _setDocType = false

  def docType = _docType

  def docType_=(in: Box[String]) {
    _docType = in
    _setDocType = true
  }

  def overrodeDocType = _setDocType
}

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

trait BaseField extends FieldIdentifier with SettableValueHolder with FieldContainer {
  import scala.xml.Text

  /**
   * Will be set to the type of the field
   */
  type ValueType


  /**
   * A list of functions that transform the value before it is set.  The transformations
   * are also applied before the value is used in a query.  Typical applications
   * of this are trimming and/or toLowerCase-ing strings
   */
  protected def setFilter: List[ValueType => ValueType]


  def validations: List[ValueType => List[FieldError]]


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

  /**
   * The human name of this field
   */
  def name: String

  def helpAsHtml: Box[NodeSeq] = Empty


  /**
   * Create an input field for the item
   */
  def toForm: Box[NodeSeq]

  /**
   * A unique 'id' for the field for form generation
   */
  def fieldId: Option[NodeSeq] = None

  def displayNameHtml: Box[NodeSeq] = Empty

  def displayHtml: NodeSeq = displayNameHtml openOr Text(displayName)


  /**
   * The display name of this field (e.g., "First Name")
   */
  def displayName: String = name

  def allFields: Seq[BaseField] = List(this)
}


