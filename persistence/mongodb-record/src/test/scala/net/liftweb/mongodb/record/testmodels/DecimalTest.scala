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

import java.math.MathContext

import net.liftweb.common._
import net.liftweb.mongodb.record.field._
import net.liftweb.mongodb.record.codecs.RecordCodec
import net.liftweb.record.field.DecimalField

import com.mongodb._

class DecimalTest private () extends MongoRecord[DecimalTest] with ObjectIdPk[DecimalTest] {

  def meta = DecimalTest

  object decimalfield extends DecimalField(this, MathContext.UNLIMITED, 2)
}

object DecimalTest extends DecimalTest with MongoMetaRecord[DecimalTest] {
  override def codecRegistry = RecordCodec.defaultRegistry
  override def bsonTypeClassMap = RecordCodec.defaultBsonTypeClassMap
}

class LegacyDecimalTest private () extends MongoRecord[LegacyDecimalTest] with ObjectIdPk[LegacyDecimalTest] {

  def meta = LegacyDecimalTest

  object decimalfield extends DecimalField(this, MathContext.UNLIMITED, 2)
}

object LegacyDecimalTest extends LegacyDecimalTest with MongoMetaRecord[LegacyDecimalTest]
