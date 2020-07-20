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

import scala.math.BigInt

import org.bson.{BsonReader, BsonWriter}
import org.bson.codecs._

/**
 * A Codec for BigInt instances. Values are stored as INT64.
 */
case class BigIntLongCodec() extends Codec[BigInt] {
  override def encode(writer: BsonWriter, value: BigInt, encoderContext: EncoderContext): Unit = {
    writer.writeInt64(value.longValue)
  }

  override def decode(reader: BsonReader, decoderContext: DecoderContext): BigInt = {
    BigInt(reader.readInt64())
  }

  override def getEncoderClass(): Class[BigInt] = classOf[BigInt]
}