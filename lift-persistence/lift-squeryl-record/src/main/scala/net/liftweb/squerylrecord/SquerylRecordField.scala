/*
 * Copyright 2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb.squerylrecord

/**
 * This trait has to be extended for new fields that
 * are derived from net.liftweb.record.BaseField or TypedField and should be used
 * in squeryl records.
 * 
 * This is necessary because the class of the field's value in the database
 * has to be known for squeryl, and this information is not directly
 * available in BaseField or TypedField.
 *
 * For all standard fields in record, there is a special handling in
 * squeryl-record. That means, for example, that you can create a subclass
 * of StringTypedField without the need to extend this trait.
 */
trait SquerylRecordField {

  /**
   * Should return the class of the field's value in the database.
   */
  def classOfPersistentField: Class[_]
}
