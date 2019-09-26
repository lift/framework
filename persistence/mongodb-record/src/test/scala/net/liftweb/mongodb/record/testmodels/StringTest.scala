/*
 * Copyright 2020 WorldWide Conferencing, LLC
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
package mongodb
package record
package testmodels

import net.liftweb.common._
import net.liftweb.mongodb.record.field._
import net.liftweb.record.field.{OptionalStringField, StringField}

import com.mongodb._

class StringTest private () extends MongoRecord[StringTest] with ObjectIdPk[StringTest] {

  def meta = StringTest

  object stringfield extends StringField(this, 100)
  object optstringfield extends OptionalStringField(this, 100)
  object stringfieldopt extends StringField(this, 100) {
    override def optional_? = true
  }
}

object StringTest extends StringTest with MongoMetaRecord[StringTest]
