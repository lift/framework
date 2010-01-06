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

/** Formats to use when converting JSON.
 * Formats are usually configured by using an implicit parameter:
 * <pre>
 * implicit val formats = net.liftweb.json.DefaultFormats
 * </pre>
 */
trait Formats {
  val dateFormat: DateFormat
  val typeHints: TypeHints = NoTypeHints
}

/** Conversions between String and Date.
 */
trait DateFormat {
  def parse(s: String): Option[Date]
  def format(d: Date): String
}

/** Type hints can be used to alter the default conversion rules when converting
 * Scala instances into JSON and vice versa. Type hints must be used when converting
 * class which is not supported by default (for instance when class is not a case class).
 * <p>
 * Example:<pre>
 * class DateTime(val time: Long)
 *
 * val hints = new ShortTypeHints(classOf[DateTime] :: Nil) {
 *   override def serialize: PartialFunction[Any, JObject] = {
 *     case t: DateTime => JObject(JField("t", JInt(t.time)) :: Nil)
 *   }
 *
 *   override def deserialize: PartialFunction[(String, JObject), Any] = {
 *     case ("DateTime", JObject(JField("t", JInt(t)) :: Nil)) => new DateTime(t.longValue)
 *   }
 * }
 * implicit val formats = DefaultFormats.withHints(hints)
 * </pre>
 */
trait TypeHints {  
  val hints: List[Class[_]]
  
  /** Return hint for given type.
   */
  def hintFor(clazz: Class[_]): String

  /** Return type for given hint.
   */
  def classFor(hint: String): Option[Class[_]]

  def containsHint_?(clazz: Class[_]) = hints exists (_ isAssignableFrom clazz)
  def deserialize: PartialFunction[(String, JObject), Any] = Map()
  def serialize: PartialFunction[Any, JObject] = Map()
}

/** Do not use any type hints.
 */
case object NoTypeHints extends TypeHints {
  val hints = Nil
  def hintFor(clazz: Class[_]) = error("NoTypeHints does not provide any type hints.")
  def classFor(hint: String) = None
}

/** Use short class name as a type hint.
 */
case class ShortTypeHints(hints: List[Class[_]]) extends TypeHints {
  def hintFor(clazz: Class[_]) = clazz.getName.substring(clazz.getName.lastIndexOf(".")+1)
  def classFor(hint: String) = hints find (hintFor(_) == hint)
}

/** Use full class name as a type hint.
 */
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

  /** Lossless date format includes milliseconds too.
   */
  def lossless = new DefaultFormats {
    override def dateFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
  }

  /** Default formats with given <code>TypeHint</code>s.
   */
  def withHints(hints: TypeHints) = new DefaultFormats {
    override val typeHints = hints
  }
}
