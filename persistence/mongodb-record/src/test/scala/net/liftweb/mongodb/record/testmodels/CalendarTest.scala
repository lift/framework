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

import java.util.{Date, GregorianCalendar}

import net.liftweb.common._
import net.liftweb.mongodb.codecs.{BsonTypeClassMap, CalendarCodec}
import net.liftweb.mongodb.record.field._
import net.liftweb.record.field.DateTimeField

import org.bson.BsonType
import org.bson.codecs.configuration.CodecRegistries

import com.mongodb._

class CalendarTest private () extends MongoRecord[CalendarTest] with ObjectIdPk[CalendarTest] {

  def meta = CalendarTest

  object calendarfield extends DateTimeField(this)

  // override def equals(other: Any): Boolean = other match {
  //   case that: CalendarTest =>
  //     that.id.value == this.id.value &&
  //     that.calendarfield.value.getTimeInMillis == this.calendarfield.value.getTimeInMillis
  //   case _ =>
  //     false
  // }
}

object CalendarTest extends CalendarTest with MongoMetaRecord[CalendarTest] {

  override def codecRegistry = CodecRegistries.fromRegistries(
    CodecRegistries.fromCodecs(CalendarCodec()),
    super.codecRegistry
  )

  override def bsonTypeClassMap = BsonTypeClassMap((BsonType.DATE_TIME -> classOf[GregorianCalendar]))
}