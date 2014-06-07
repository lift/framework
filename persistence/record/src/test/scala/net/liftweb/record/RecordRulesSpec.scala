/*
 * Copyright 2014 WorldWide Conferencing, LLC
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
import http.{LiftSession, S}
import util.Helpers._

import org.specs2.mutable._
import org.specs2.specification.Fragment

import fixtures._


/**
 * Systems under specification for RecordField.
 */
object RecordRulesSpec extends Specification {
  "Record Rules Specification".title
  sequential

  "RecordRules" should {
    "snakify custom field name" in {
      RecordRules.fieldName.doWith((_, name) => snakify(name)) {
        val rec = BasicTestRecord.createRecord

        rec.fieldThree.name must_== "field_three"
      }
    }
    "camelify custom field display name" in {
      RecordRules.displayName.doWith((_, _, name) => camelify(name)) {
        val rec = BasicTestRecord.createRecord

        rec.fieldThree.displayName must_== "FieldThree"
      }
    }
  }
}
