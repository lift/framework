package net.liftweb.json

/*
 * Copyright 2009 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */

import scala.reflect.Manifest
import JsonAST._
import JsonParser.parse

/** Functions to serialize and deserialize a case class.
 *
 *  FIXME: add Map support
 * 
 *  See: SerializationExamples.scala
 */
object Serialization {
  import java.io.{StringWriter, Writer}
  import Meta.Reflection._

  def write[A <: AnyRef](a: A)(implicit formats: Formats): String = 
    (write(a, new StringWriter)(formats)).toString

  def write[A <: AnyRef, W <: Writer](a: A, out: W)(implicit formats: Formats): W = 
    Printer.compact(render(Extraction.decompose(a)(formats)), out)

  def read[A](json: String)(implicit formats: Formats, mf: Manifest[A]): A = 
    parse(json).extract(formats, mf)

  def formats(hints: TypeHints) = new Formats {
    val dateFormat = DefaultFormats.lossless.dateFormat
    override val typeHints = hints
  }
}
