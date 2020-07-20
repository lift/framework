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
package codecs

import java.util.{Calendar, GregorianCalendar}

import org.bson.codecs._
import org.bson.{BsonReader, BsonWriter}

/**
 * A Codec for Calendar instances.
 */
case class CalendarCodec() extends Codec[GregorianCalendar] {
  override def decode(reader: BsonReader, decoderContext: DecoderContext): GregorianCalendar = {
    val cal = new GregorianCalendar()
    cal.setTimeInMillis(reader.readDateTime())
    cal
  }

  override def encode(writer: BsonWriter, value: GregorianCalendar, encoderContext: EncoderContext): Unit = {
    writer.writeDateTime(value.getTimeInMillis())
  }

  override def getEncoderClass(): Class[GregorianCalendar] = classOf[GregorianCalendar]
}