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
import net.liftweb.mongodb.codecs.{BsonTypeClassMap, JodaDateTimeCodec}
import net.liftweb.mongodb.record.field._
import net.liftweb.record.field.joda.JodaTimeField

import com.mongodb._

import org.bson.BsonType
import org.bson.codecs.configuration.CodecRegistries

import org.joda.time.DateTime

class JodaTimeTest private () extends MongoRecord[JodaTimeTest] with ObjectIdPk[JodaTimeTest] {

  def meta = JodaTimeTest

  object jodatimefield extends JodaTimeField(this)
}

object JodaTimeTest extends JodaTimeTest with MongoMetaRecord[JodaTimeTest] {
  // override def codecRegistry = CodecRegistries.fromRegistries(
  //   CodecRegistries.fromCodecs(JodaDateTimeCodec()),
  //   super.codecRegistry
  // )

  // override def bsonTypeClassMap: BsonTypeClassMap = BsonTypeClassMap((BsonType.DATE_TIME -> classOf[DateTime]))
}