/*
 * Copyright 2010-2013 WorldWide Conferencing, LLC
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
package field

import scala.xml.NodeSeq

import net.liftweb.common.{Box, Empty, Failure, Full}
import net.liftweb.http.js.JE.{JsNull, JsRaw}
import net.liftweb.json.JsonAST._
import net.liftweb.json.{JsonParser, Printer}
import net.liftweb.record._
import net.liftweb.util.Helpers.tryo

import com.mongodb._

/**
  * Note: setting optional_? = false will result in incorrect equals behavior when using setFromJValue
  */
class MongoMapField[OwnerType <: BsonRecord[OwnerType], MapValueType](rec: OwnerType)
  extends Field[Map[String, MapValueType], OwnerType] with MandatoryTypedField[Map[String, MapValueType]]
  with MongoFieldFlavor[Map[String, MapValueType]] {

  import Meta.Reflection._

  def owner = rec

  def defaultValue = Map[String, MapValueType]()

  def setFromAny(in: Any): Box[Map[String, MapValueType]] = {
    in match {
      case dbo: DBObject => setFromDBObject(dbo)
      case map: Map[_, _] => setBox(Full(map.asInstanceOf[Map[String, MapValueType]]))
      case Some(map: Map[_, _]) => setBox(Full(map.asInstanceOf[Map[String, MapValueType]]))
      case Full(map: Map[_, _]) => setBox(Full(map.asInstanceOf[Map[String, MapValueType]]))
      case (map: Map[_, _]) :: _ => setBox(Full(map.asInstanceOf[Map[String, MapValueType]]))
      case s: String => setFromString(s)
      case Some(s: String) => setFromString(s)
      case Full(s: String) => setFromString(s)
      case null|None|Empty => setBox(defaultValueBox)
      case f: Failure => setBox(f)
      case o => setFromString(o.toString)
    }
  }

  def setFromJValue(jvalue: JValue) = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JObject(obj) => setBox(Full(
      Map() ++ obj.map(jf => (jf.name, jf.value.values.asInstanceOf[MapValueType]))
    ))
    case other => setBox(FieldHelpers.expectedA("JObject", other))
  }

  def setFromString(in: String): Box[Map[String, MapValueType]] = tryo(JsonParser.parse(in)) match {
    case Full(jv: JValue) => setFromJValue(jv)
    case f: Failure => setBox(f)
    case other => setBox(Failure("Error parsing String into a JValue: "+in))
  }

  def toForm: Box[NodeSeq] = Empty

  def asJValue = JObject(value.keys.map {
    k =>
      JField(k, value(k).asInstanceOf[AnyRef] match {
        case x if primitive_?(x.getClass) => primitive2jvalue(x)
        case x if mongotype_?(x.getClass) => mongotype2jvalue(x)(owner.meta.formats)
        case x if datetype_?(x.getClass) => datetype2jvalue(x)(owner.meta.formats)
        case _ => JNothing
      })
  }.toList)

  /*
  * Convert this field's value into a DBObject so it can be stored in Mongo.
  */
  def asDBObject: DBObject = {
    val dbo = new BasicDBObject
    value.keys.foreach { k =>
      dbo.put(k.toString, value.getOrElse(k, ""))
    }
    dbo
  }

  // set this field's value using a DBObject returned from Mongo.
  def setFromDBObject(dbo: DBObject): Box[Map[String, MapValueType]] = {
    import scala.collection.JavaConversions._

    setBox(Full(
      Map() ++ dbo.keySet.map {
        k => (k.toString, dbo.get(k).asInstanceOf[MapValueType])
      }
    ))
  }
}

