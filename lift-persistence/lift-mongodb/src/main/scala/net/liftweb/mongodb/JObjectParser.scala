/*
 * Copyright 2010 WorldWide Conferencing, LLC
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

package net.liftweb {
package mongodb {

import scala.collection.JavaConversions._

import _root_.java.util.{Date, UUID}
import _root_.java.util.regex.Pattern

import _root_.net.liftweb.json.Formats
import _root_.net.liftweb.json.JsonAST._
import _root_.net.liftweb.common.Box

import com.mongodb.{BasicDBObject, BasicDBList, DBObject}
import org.bson.types.ObjectId

object JObjectParser {

  /*
  * Parse a JObject into a DBObject
  */
  def parse(jo: JObject)(implicit formats: Formats): DBObject =
    Parser.parse(jo, formats)

  /*
  * Serialize a DBObject into a JObject
  */
  def serialize(a: Any)(implicit formats: Formats): JValue = serialize(a, formats)

  private def serialize(a: Any, formats: Formats): JValue = {
    import Meta.Reflection._
    a.asInstanceOf[AnyRef] match {
      case null => JNull
      case x if primitive_?(x.getClass) => primitive2jvalue(x)
      case x if datetype_?(x.getClass) => datetype2jvalue(x)(formats)
      case x if mongotype_?(x.getClass) => mongotype2jvalue(x)(formats)
      case x: BasicDBList => JArray(x.toList.map( x => serialize(x, formats)))
      case x: BasicDBObject =>
        x.keySet.toArray.toList.map { f =>
          JField(f.toString, serialize(x.get(f.toString), formats))
        }
        match {
          case Nil => JNothing
          case fields => JObject(fields)
        }
      case x => {
        println("match error (serialize): "+x.getClass+" "+x.toString)
        JNothing
      }
    }
  }

  object Parser {

    def parse(jo: JObject, formats: Formats): DBObject = {
      parseObject(jo.obj, formats)
    }

    private def parseArray(arr: List[JValue], formats: Formats): BasicDBList = {
      val dbl = new BasicDBList
      trimArr(arr).foreach { a =>
        a match {
          case JArray(arr) => dbl.add(parseArray(arr, formats))
          case JObject(jo) => dbl.add(parseObject(jo, formats))
          case jv: JValue => dbl.add(renderValue(jv, formats))
        }
      }
      dbl
    }

    private def parseObject(obj: List[JField], formats: Formats): BasicDBObject = {
      val dbo = new BasicDBObject
      trimObj(obj).foreach { jf =>
        jf.value match {
          case JArray(arr) => dbo.put(jf.name, parseArray(arr, formats))
          case JObject(JField("$oid", JString(s)) :: Nil) if (ObjectId.isValid(s)) =>
            dbo.put(jf.name, new ObjectId(s))
          case JObject(JField("$regex", JString(s)) :: JField("$flags", JInt(f)) :: Nil) =>
            dbo.put(jf.name, Pattern.compile(s, f.intValue))
          case JObject(JField("$dt", JString(s)) :: Nil) =>
            formats.dateFormat.parse(s) foreach {
              d => dbo.put(jf.name, d)
            }
          case JObject(JField("$uuid", JString(s)) :: Nil) =>
            dbo.put(jf.name, UUID.fromString(s))
          case JObject(jo) => dbo.put(jf.name, parseObject(jo, formats))
          case jv: JValue => dbo.put(jf.name, renderValue(jv, formats))
        }
      }
      dbo
    }

    private def renderValue(jv: JValue, formats: Formats): Object = jv match {
      case JBool(b) => java.lang.Boolean.valueOf(b)
      case JInt(n) => renderInteger(n)
      case JDouble(n) => new java.lang.Double(n)
      case JNull => null
      case JNothing => error("can't render 'nothing'")
      case JString(null) => "null"
      case JString(s) if (ObjectId.isValid(s)) => new ObjectId(s)
      case JString(s) => s
      case _ => println("match error (renderValue): "+jv.getClass); ""
    }

    // FIXME: This is not ideal.
    private def renderInteger(i: BigInt): Object = {
      if (i <= java.lang.Integer.MAX_VALUE && i >= java.lang.Integer.MIN_VALUE) {
        new java.lang.Integer(i.intValue)
      }
      else if (i <= java.lang.Long.MAX_VALUE && i >= java.lang.Long.MIN_VALUE) {
        new java.lang.Long(i.longValue)
      }
      else {
        i.toString
      }
    }

    private def trimArr(xs: List[JValue]) = xs.filter(_ != JNothing)
    private def trimObj(xs: List[JField]) = xs.filter(_.value != JNothing)
  }
}

}
}
