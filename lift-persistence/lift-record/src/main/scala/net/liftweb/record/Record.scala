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

import net.liftweb._
import util._
import common._
import scala.xml._
import net.liftweb.http.js.JsExp
import net.liftweb.http.{Req, SHtml}
import net.liftweb.mapper.Safe
import field._

trait Record[MyType <: Record[MyType]] extends FieldContainer {
  self: MyType =>

  /**
   * A unique identifier for this record... used for access control
   */
  private val secure_# = Safe.next

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
    Safe.safe_?(secure_#)
  }

  def runSafe[T](f : => T) : T = {
    Safe.runSafe(secure_#)(f)
  }

  /**
   * Returns the HTML representation ofthis Record
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
   * Retuns the JSON representation of this record
   *
   * @return a JsObjss
   */
  def asJSON: JsExp = meta.asJSON(this)

  /**
   * Sets the fields of this Record from the given JSON.
   */
  def setFieldsFromJSON(json: String): Box[Unit] = meta.setFieldsFromJSON(this, json)

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

}
}
