/*
 * Copyright 2010-2014 WorldWide Conferencing, LLC
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

import scala.collection.JavaConversions._
import scala.xml.NodeSeq

import common.{Box, Empty, Failure, Full}
import http.SHtml
import http.js.JE.{JsNull, JsRaw}
import json._
import net.liftweb.record.{Field, FieldHelpers, MandatoryTypedField, Record}
import util.Helpers._

import com.mongodb._
import org.bson.types.ObjectId
import org.joda.time.DateTime

/**
  * List field.
  *
  * Supported types:
  * primitives - String, Int, Long, Double, Float, Byte, BigInt,
  * Boolean (and their Java equivalents)
  * date types - java.util.Date, org.joda.time.DateTime
  * mongo types - ObjectId, Pattern, UUID
  *
  * If you need to support other types, you will need to override the
  * asDBObject and setFromDBObject functions accordingly. And the
  * asJValue and setFromJValue functions if you will be using them.
  *
  * Note: setting optional_? = false will result in incorrect equals behavior when using setFromJValue
  */
class MongoListField[OwnerType <: BsonRecord[OwnerType], ListType: Manifest](rec: OwnerType)
  extends Field[List[ListType], OwnerType]
  with MandatoryTypedField[List[ListType]]
  with MongoFieldFlavor[List[ListType]]
{
  import mongodb.Meta.Reflection._

  lazy val mf = manifest[ListType]

  override type MyType = List[ListType]

  def owner = rec

  def defaultValue = List[ListType]()

  implicit def formats = owner.meta.formats

  def setFromAny(in: Any): Box[MyType] = {
    in match {
      case dbo: DBObject => setFromDBObject(dbo)
      case list@c::xs if mf.runtimeClass.isInstance(c) => setBox(Full(list.asInstanceOf[MyType]))
      case Some(list@c::xs) if mf.runtimeClass.isInstance(c) => setBox(Full(list.asInstanceOf[MyType]))
      case Full(list@c::xs) if mf.runtimeClass.isInstance(c) => setBox(Full(list.asInstanceOf[MyType]))
      case s: String => setFromString(s)
      case Some(s: String) => setFromString(s)
      case Full(s: String) => setFromString(s)
      case null|None|Empty => setBox(defaultValueBox)
      case f: Failure => setBox(f)
      case o => setFromString(o.toString)
    }
  }

  def setFromJValue(jvalue: JValue): Box[MyType] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JArray(array) => setBox(Full((array.map {
      case JsonObjectId(objectId) => objectId
      case JsonRegex(regex) => regex
      case JsonUUID(uuid) => uuid
      case JsonDateTime(dt) if (mf.toString == "org.joda.time.DateTime") => dt
      case JsonDate(date) => date
      case other => other.values
    }).asInstanceOf[MyType]))
    case other => setBox(FieldHelpers.expectedA("JArray", other))
  }

  // parse String into a JObject
  def setFromString(in: String): Box[List[ListType]] = tryo(JsonParser.parse(in)) match {
    case Full(jv: JValue) => setFromJValue(jv)
    case f: Failure => setBox(f)
    case other => setBox(Failure("Error parsing String into a JValue: "+in))
  }

  /** Options for select list **/
  def options: List[(ListType, String)] = Nil

  private def elem = {
    def elem0 = SHtml.multiSelectObj[ListType](
      options,
      value,
      set(_)
    ) % ("tabindex" -> tabIndex.toString)

    SHtml.hidden(() => set(Nil)) ++ (uniqueFieldId match {
      case Full(id) => (elem0 % ("id" -> id))
      case _ => elem0
    })
  }

  def toForm: Box[NodeSeq] =
    if (options.length > 0) Full(elem)
    else Empty

  def asJValue = JArray(value.map(li => li.asInstanceOf[AnyRef] match {
    case x if primitive_?(x.getClass) => primitive2jvalue(x)
    case x if mongotype_?(x.getClass) => mongotype2jvalue(x)(owner.meta.formats)
    case x if datetype_?(x.getClass) => datetype2jvalue(x)(owner.meta.formats)
    case _ => JNothing
  }))

  /*
  * Convert this field's value into a DBObject so it can be stored in Mongo.
  */
  def asDBObject: DBObject = {
    val dbl = new BasicDBList

    value.foreach {
      case f =>	f.asInstanceOf[AnyRef] match {
        case x if primitive_?(x.getClass) => dbl.add(x)
        case x if mongotype_?(x.getClass) => dbl.add(x)
        case x if datetype_?(x.getClass) => dbl.add(datetype2dbovalue(x))
        case o => dbl.add(o.toString)
      }
    }
    dbl
  }

  // set this field's value using a DBObject returned from Mongo.
  def setFromDBObject(dbo: DBObject): Box[MyType] =
    setBox(Full(dbo.asInstanceOf[BasicDBList].toList.asInstanceOf[MyType]))
}

/*
* List of JsonObject case classes
*/
class MongoJsonObjectListField[OwnerType <: BsonRecord[OwnerType], JObjectType <: JsonObject[JObjectType]]
  (rec: OwnerType, valueMeta: JsonObjectMeta[JObjectType])(implicit mf: Manifest[JObjectType])
  extends MongoListField[OwnerType, JObjectType](rec: OwnerType) {

  override def asDBObject: DBObject = {
    val dbl = new BasicDBList
    value.foreach { v => dbl.add(JObjectParser.parse(v.asJObject()(owner.meta.formats))(owner.meta.formats)) }
    dbl
  }

  override def setFromDBObject(dbo: DBObject): Box[List[JObjectType]] =
    setBox(Full(dbo.keySet.toList.map(k => {
      valueMeta.create(JObjectParser.serialize(dbo.get(k.toString))(owner.meta.formats).asInstanceOf[JObject])(owner.meta.formats)
    })))

  override def asJValue = JArray(value.map(_.asJObject()(owner.meta.formats)))

  override def setFromJValue(jvalue: JValue) = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JArray(arr) => setBox(Full(arr.map( jv => {
      valueMeta.create(jv.asInstanceOf[JObject])(owner.meta.formats)
    })))
    case other => setBox(FieldHelpers.expectedA("JArray", other))
  }
}
