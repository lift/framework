/*
 * Copyright 2006-2010 WorldWide Conferencing, LLC
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
package json
package ext

import org.joda.time._
import JsonDSL._

object JodaTimeSerializers {
  def all = List(DurationSerializer(), InstantSerializer(), DateTimeSerializer(), 
                 DateMidnightSerializer(), IntervalSerializer(), LocalDateSerializer(),
                 LocalTimeSerializer())
}

object DurationSerializer {
  def apply() = new SimpleTypeSerializer(new JIntType[Duration]() {
    def targetClass = classOf[Duration]
    def unwrap(json: JInt)(implicit format: Formats) = new Duration(json.num.longValue)
    def wrap(a: Duration)(implicit format: Formats) = JInt(a.getMillis)
  })
}

object InstantSerializer {
  def apply() = new SimpleTypeSerializer(new JIntType[Instant]() {
    def targetClass = classOf[Instant]
    def unwrap(json: JInt)(implicit format: Formats) = new Instant(json.num.longValue)
    def wrap(a: Instant)(implicit format: Formats) = JInt(a.getMillis)
  })
}

object DateTimeSerializer {
  def apply() = new SimpleTypeSerializer(new JStringType[DateTime]() {
    def targetClass = classOf[DateTime]
    def unwrap(json: JString)(implicit format: Formats) = new DateTime(parse(json))
    def wrap(a: DateTime)(implicit format: Formats) = JString(format.dateFormat.format(a.toDate))
  })

  private[ext] def parse(json: JString)(implicit format: Formats) = 
    format.dateFormat.parse(json.s).map(_.getTime).getOrElse {
      throw new MappingException("Invalid date format " + json.s)
    }
}

object DateMidnightSerializer {
  def apply() = new SimpleTypeSerializer(new JStringType[DateMidnight]() {
    def targetClass = classOf[DateMidnight]
    def unwrap(json: JString)(implicit format: Formats) = 
      new DateMidnight(DateTimeSerializer.parse(json))
    def wrap(a: DateMidnight)(implicit format: Formats) = 
      JString(format.dateFormat.format(a.toDate))
  })
}

private[ext] case class _Interval(start: Long, end: Long)
object IntervalSerializer {
  def apply() = new ClassSerializer(new ClassType[Interval, _Interval]() {
    def unwrap(i: _Interval)(implicit format: Formats) = new Interval(i.start, i.end)
    def wrap(i: Interval)(implicit format: Formats) = _Interval(i.getStartMillis, i.getEndMillis)
  })
}

private[ext] case class _LocalDate(year: Int, month: Int, day: Int) 
object LocalDateSerializer {
  def apply() = new ClassSerializer(new ClassType[LocalDate, _LocalDate]() {
    def unwrap(d: _LocalDate)(implicit format: Formats) = new LocalDate(d.year, d.month, d.day)
    def wrap(d: LocalDate)(implicit format: Formats) = 
      _LocalDate(d.getYear(), d.getMonthOfYear, d.getDayOfMonth)
  })
}

private[ext] case class _LocalTime(hour: Int, minute: Int, second: Int, millis: Int) 
object LocalTimeSerializer {
  def apply() = new ClassSerializer(new ClassType[LocalTime, _LocalTime]() {
    def unwrap(t: _LocalTime)(implicit format: Formats) = 
      new LocalTime(t.hour, t.minute, t.second, t.millis)
    def wrap(t: LocalTime)(implicit format: Formats) = 
      _LocalTime(t.getHourOfDay, t.getMinuteOfHour, t.getSecondOfMinute, t.getMillisOfSecond)
  })
}

// FIXME consider moving these utilities to lift-json in some form
private[ext] trait ClassType[A, B] {
  def unwrap(b: B)(implicit format: Formats): A
  def wrap(a: A)(implicit format: Formats): B
}

case class ClassSerializer[A : Manifest, B : Manifest](t: ClassType[A, B]) extends Serializer[A] {
  private val Class = implicitly[Manifest[A]].erasure

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), A] = {
    case (TypeInfo(Class, _), json) => json match {
      case xs: JObject if (xs.extractOpt[B].isDefined) => t.unwrap(xs.extract[B])
      case value => throw new MappingException("Can't convert " + value + " to " + Class)
    }
  }

  def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
    case a: A if a.asInstanceOf[AnyRef].getClass == Class => Extraction.decompose(t.wrap(a))
  }
}

private[ext] trait SimpleType[A, JS <: JValue] {
  def targetClass: Class[A]
  def unwrap(json: JS)(implicit format: Formats): A
  def wrap(a: A)(implicit format: Formats): JS
}

private[ext] trait JIntType[A] extends SimpleType[A, JInt]
private[ext] trait JStringType[A] extends SimpleType[A, JString]

private[ext] class SimpleTypeSerializer[A, JS <: JValue](t: SimpleType[A, JS]) extends Serializer[A] {
  private val Class = t.targetClass

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), A] = {
    case (TypeInfo(Class, _), json) => json match {
      case json: JS => t.unwrap(json)
      case value => throw new MappingException("Can't convert " + value + " to " + Class)
    }
  }

  def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
    case d: A if d.asInstanceOf[AnyRef].getClass == t.targetClass => t.wrap(d)
  }
}
