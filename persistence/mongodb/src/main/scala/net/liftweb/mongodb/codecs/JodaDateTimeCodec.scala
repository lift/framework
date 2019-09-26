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

package net.liftweb.mongodb
package codecs

import org.joda.time.DateTime

import org.bson.codecs._
import org.bson.{BsonReader, BsonWriter}

/**
 * A Codec for joda DateTime instances.
 */
case class JodaDateTimeCodec() extends Codec[DateTime] {
  override def decode(reader: BsonReader, decoderContext: DecoderContext): DateTime = {
    new DateTime(reader.readDateTime())
  }

  override def encode(writer: BsonWriter, value: DateTime, encoderContext: EncoderContext): Unit = {
    writer.writeDateTime(value.getMillis())
  }

  override def getEncoderClass(): Class[DateTime] = classOf[DateTime]
}