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

import java.util.{Date, TimeZone}
import JsonAST.JObject

trait Formats {
  val dateFormat: DateFormat
  val typeHints: TypeHints = NoTypeHints
}

trait DateFormat {
  def parse(s: String): Option[Date]
  def format(d: Date): String
}

trait TypeHints {
  val hints: List[Class[_]]
  def hintFor(clazz: Class[_]): String
  def classFor(hint: String): Option[Class[_]]

  def containsHint_?(clazz: Class[_]) = hints exists (_ isAssignableFrom clazz)
  def deserialize: PartialFunction[(String, JObject), Any] = Map()
  def serialize: PartialFunction[Any, JObject] = Map()
}

case object NoTypeHints extends TypeHints {
  val hints = Nil
  def hintFor(clazz: Class[_]) = error("NoTypeHints does not provide any type hints.")
  def classFor(hint: String) = None
}

case class ShortTypeHints(hints: List[Class[_]]) extends TypeHints {
  def hintFor(clazz: Class[_]) = clazz.getName.substring(clazz.getName.lastIndexOf(".")+1)
  def classFor(hint: String) = hints find (hintFor(_) == hint)
}

case class FullTypeHints(hints: List[Class[_]]) extends TypeHints {
  def hintFor(clazz: Class[_]) = clazz.getName
  def classFor(hint: String) = Some(Thread.currentThread.getContextClassLoader.loadClass(hint))
}

/** Default date format is UTC time.
 */
object DefaultFormats extends DefaultFormats
trait DefaultFormats extends Formats {
  import java.text.{ParseException, SimpleDateFormat}

  val dateFormat = new DateFormat {    
    def parse(s: String) = try {
      Some(formatter.parse(s))
    } catch {
      case e: ParseException => None
    }
    
    def format(d: Date) = formatter.format(d)

    private def formatter = {
      val f = dateFormatter
      f.setTimeZone(TimeZone.getTimeZone("UTC"))
      f
    }
  }

  protected def dateFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")

  def lossless = new DefaultFormats {
    override def dateFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
  }
}
