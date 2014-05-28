/*
* Copyright 2010-2011 WorldWide Conferencing, LLC
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

package net.liftweb.record

import java.util.{Date, UUID}
import java.util.regex.{Pattern, PatternSyntaxException}


import org.joda.time.DateTime
import net.liftweb.json.{MappingException, TypeInfo, Formats, Serializer}
import net.liftweb.json.JsonDSL._
import net.liftweb.json.JsonAST.{JString, JField, JObject, JValue, JInt}

/**
 * How should serialized type look like in json. Preferably this should be implemented by serializers, but other parts
 * of the framework rely directly on those functions so they are kept for consistency.
 */
object SerializationFunctions {
  def dateAsJValue(d: Date, formats: Formats): JValue = ("$dt" -> formats.dateFormat.format(d))
  def patternAsJValue(p: Pattern): JValue = ("$regex" -> p.pattern) ~ ("$flags" -> p.flags)
  def uuidAsJValue(u: UUID): JValue = ("$uuid" -> u.toString)
}



/*
* Provides a way to serialize/de-serialize Patterns.
*
* Queries for a Pattern (pattern) using the lift-json DSL look like:
* ("pattern" -> (("$regex" -> pattern.pattern) ~ ("$flags" -> pattern.flags)))
* ("pattern" -> (("$regex" -> "^Mo") ~ ("$flags" -> Pattern.CASE_INSENSITIVE)))
*/
class PatternSerializer extends Serializer[Pattern] {
  private val PatternClass = classOf[Pattern]

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Pattern] = {
    case (TypeInfo(PatternClass, _), json) => json match {
      case JObject(JField("$regex", JString(s)) :: JField("$flags", JInt(f)) :: Nil) =>
        Pattern.compile(s, f.intValue)
      case x => throw new MappingException("Can't convert " + x + " to Pattern")
    }
  }

  def serialize(implicit formats: Formats): PartialFunction[Any, JValue] = {
    case x: Pattern => SerializationFunctions.patternAsJValue(x)
  }
}

/*
* Provides a way to serialize/de-serialize Dates.
*
* Queries for a Date (dt) using the lift-json DSL look like:
* ("dt" -> ("$dt" -> formats.dateFormat.format(dt)))
*/
class DateSerializer extends Serializer[Date] {
  private val DateClass = classOf[Date]

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Date] = {
    case (TypeInfo(DateClass, _), json) => json match {
      case JObject(JField("$dt", JString(s)) :: Nil) =>
        format.dateFormat.parse(s).getOrElse(throw new MappingException("Can't parse "+ s + " to Date"))
      case x => throw new MappingException("Can't convert " + x + " to Date")
    }
  }

  def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
    case x: Date => SerializationFunctions.dateAsJValue(x, format)
  }
}

/*
* Provides a way to serialize/de-serialize joda time DateTimes.
*
* Queries for a Date (dt) using the lift-json DSL look like:
* ("dt" -> ("$dt" -> formats.dateFormat.format(dt)))
*/
class DateTimeSerializer extends Serializer[DateTime] {
  private val DateTimeClass = classOf[DateTime]

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), DateTime] = {
    case (TypeInfo(DateTimeClass, _), json) => json match {
      case JObject(JField("$dt", JString(s)) :: Nil) =>
        new DateTime(format.dateFormat.parse(s).getOrElse(throw new MappingException("Can't parse "+ s + " to DateTime")))
      case x => throw new MappingException("Can't convert " + x + " to Date")
    }
  }

  def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {  
    case x: DateTime => SerializationFunctions.dateAsJValue(x.toDate, format)
  }
}

/*
* Provides a way to serialize/de-serialize UUIDs.
*
* Queries for a UUID (u) using the lift-json DSL look like:
* ("uuid" -> ("$uuid" -> u.toString))
*/
class UUIDSerializer extends Serializer[UUID] {
  private val UUIDClass = classOf[UUID]

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), UUID] = {
    case (TypeInfo(UUIDClass, _), json) => json match {
      case JObject(JField("$uuid", JString(s)) :: Nil) => UUID.fromString(s)
      case x => throw new MappingException("Can't convert " + x + " to Date")
    }
  }

  def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
    case x: UUID => SerializationFunctions.uuidAsJValue(x)
  }
}

