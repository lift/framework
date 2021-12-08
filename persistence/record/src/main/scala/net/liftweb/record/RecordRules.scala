/*
 * Copyright 2013 WorldWide Conferencing, LLC
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

import http.Factory
import util.ConnectionIdentifier
import util.Helpers._

import java.util.Locale

object RecordRules extends Factory {
  /**
    * Calculate the name of a field based on the name
    * of the Field. Must be set in Boot before any code
    * that touches the MetaRecord.
    *
    * To get snake_case, use this:
    *
    *  RecordRules.fieldName.default.set((_, name) => StringHelpers.snakify(name))
    */
  val fieldName = new FactoryMaker[(ConnectionIdentifier, String) => String]((_: ConnectionIdentifier, name: String) => name) {}

  /**
    * This function is used to calculate the displayName of a field. Can be
    * used to easily localize fields based on the locale in the
    * current request
    */
// Solution proposed here: https://groups.google.com/g/scala-user/c/ICRKvVrYGkk
// But this crashes the compiler with a stack overflow.
//  import scala.language.existentials
//  type RecordImpl = Record[T] forSome {type T <: Record[T]}
//  val displayName: FactoryMaker[(RecordImpl, Locale, String) => String] =
//    new FactoryMaker[(Record[_], Locale, String) => String]((m: RecordImpl, l: Locale, name: String) => name) {}

  val displayName: FactoryMaker[(Record[_], Locale, String) => String] =
    new FactoryMaker[(Record[_], Locale, String) => String]((m: Record[_], l: Locale, name: String) => name) {}
}
