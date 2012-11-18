/*
 * Copyright 2007-2012 WorldWide Conferencing, LLC
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
package record

import common._
import http.js.{JsExp, JsObj}
import http.{Req, SHtml}
import json.JsonAST._
import util._
import field._

import scala.xml._
import java.util.prefs.BackingStoreException

trait Record[MyType <: Record[MyType]] extends FieldContainer {
  self: MyType =>

  /**
   * Get the fields defined on the meta object for this record instance
   */
  def fields() = meta.fields(this)

  def allFields = fields()

  /**
   * The meta record (the object that contains the meta result for this type)
   */
  def meta: MetaRecord[MyType]

  /**
   * Is it safe to make changes to the record (or should we check access control?)
   */
  final def safe_? : Boolean = {
    Safe.safe_?(System.identityHashCode(this))
  }

  def runSafe[T](f : => T) : T = {
    Safe.runSafe(System.identityHashCode(this))(f)
  }

  /**
   * Returns the HTML representation of this Record
   */
  def toXHtml: NodeSeq = {
    meta.toXHtml(this)
  }

  /**
   * Validates this Record by calling validators for each field
   *
   * @return a List of FieldError. If this list is empty you can assume that record was validated successfully
   */
  def validate : List[FieldError] = {
    runSafe {
      meta.validate(this)
    }
  }

  /**
   * Returns the JSON representation of this record
   *
   * @return a JsObj
   */
  def asJSON: JsExp = meta.asJSON(this)

 /**
  * Save the instance and return the instance
  */
  def saveTheRecord(): Box[MyType] = throw new BackingStoreException("Raw Records don't save themselves")

  /**
   * Returns the JSON representation of this record, converts asJValue to JsObj
   *
   * @return a JsObj
   */
  def asJsExp: JsExp = meta.asJsExp(this)

  /**
   * Sets the fields of this Record from the given JSON.
   */
  def setFieldsFromJSON(json: String): Box[Unit] = meta.setFieldsFromJSON(this, json)

  /** Encode this record instance as a JObject */
  def asJValue: JObject = meta.asJValue(this)

  /** Set the fields of this record from the given JValue */
  def setFieldsFromJValue(jvalue: JValue): Box[Unit] = meta.setFieldsFromJValue(this, jvalue)

  /**
   * Sets the fields of this Record from the given JSON.
   */
  def setFieldsFromJsonString(json: String): Box[Unit] = meta.setFieldsFromJsonString(this, json)

  /**
   * Sets the fields of this Record from the given Req.
   */
  def setFieldsFromReq(req: Req){ meta.setFieldsFromReq(this, req) }

  /**
   * Present the model as a form and execute the function on submission of the form
   *
   * @param button - If it's Full, put a submit button on the form with the value of the parameter
   * @param f - the function to execute on form submission
   *
   * @return the form
   */
  def toForm(button: Box[String])(f: MyType => Unit): NodeSeq = {
    meta.toForm(this) ++
    (SHtml.hidden(() => f(this))) ++
    ((button.map(b => (<input type="submit" value={b}/>)) openOr scala.xml.Text("")))
  }

  /**
   * Present the model as a form and execute the function on submission of the form
   *
   * @param f - the function to execute on form submission
   *
   * @return the form
   */
  def toForm(f: MyType => Unit): NodeSeq = meta.toForm(this) ++ (SHtml.hidden(() => f(this)))

  /**
   * Find the field by name
   * @param fieldName -- the name of the field to find
   *
   * @return Box[MappedField]
   */
  def fieldByName(fieldName: String): Box[Field[_, MyType]] = meta.fieldByName(fieldName, this)

  override def equals(other: Any): Boolean = {
    other match {
      case that: Record[MyType] =>
        that.fields.corresponds(this.fields) { (a,b) =>
          a.name == b.name && a.valueBox == b.valueBox
        }
      case _ => false
    }
  }

  override def toString = {
    val fieldList = this.fields.map(f => "%s=%s" format (f.name,
        f.valueBox match {
          case Full(c: java.util.Calendar) => c.getTime().toString()
          case Full(null) => "null"
          case Full(v) => v.toString
          case _ => ""
        }))

    "%s={%s}" format (this.getClass.toString, fieldList.mkString(", "))
  }

  def copy: MyType = meta.copy(this)
}

trait ExpandoRecord[MyType <: Record[MyType] with ExpandoRecord[MyType]] {
  self: MyType =>

  /**
   * If there's a field in this record that defines the locale, return it
   */
  def localeField: Box[LocaleField[MyType]] = Empty

  def timeZoneField: Box[TimeZoneField[MyType]] = Empty

  def countryField: Box[CountryField[MyType]] = Empty
}


trait KeyedRecord[MyType <: KeyedRecord[MyType, KeyType], KeyType] extends Record[MyType] {
  self: MyType =>

  def primaryKey: KeyField[KeyType, MyType]

  def comparePrimaryKeys(other: MyType) = primaryKey === other.primaryKey
}

