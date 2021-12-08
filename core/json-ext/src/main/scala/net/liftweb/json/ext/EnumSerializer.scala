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

import scala.reflect.ClassTag

class EnumSerializer[E <: Enumeration: ClassTag](e: E)
  extends json.Serializer[E#Value] {
  import JsonDSL._

  val EnumerationClass = classOf[E#Value]

  def deserialize(implicit format: Formats):
    PartialFunction[(TypeInfo, JValue), E#Value] = {
      case (TypeInfo(EnumerationClass, _), json) => json match {
        case JInt(value) if (value <= e.maxId) => e(value.toInt)
        case value => throw new MappingException("Can't convert " +
          value + " to "+ EnumerationClass)
      }
    }

  def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
    case i: E#Value => i.id
  }
}

class EnumNameSerializer[E <: Enumeration: ClassTag](e: E)
  extends json.Serializer[E#Value] {
  import JsonDSL._

  val EnumerationClass = classOf[E#Value]

  def deserialize(implicit format: Formats):
    PartialFunction[(TypeInfo, JValue), E#Value] = {
      case (TypeInfo(EnumerationClass, _), json) => json match {
        case JString(value) if (e.values.exists(_.toString == value)) =>
          e.withName(value)
        case value => throw new MappingException("Can't convert " +
          value + " to "+ EnumerationClass)
      }
    }

  def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
    case i: E#Value => i.toString
  }
}
