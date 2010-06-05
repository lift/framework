/*
* Copyright 2010 WorldWide Conferencing, LLC
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

package net.liftweb {
package mongodb {
package record {
package field {

import java.util.Date

import scala.collection.JavaConversions._
import scala.xml.NodeSeq

import _root_.net.liftweb.common.{Box, Empty, Failure, Full}
import _root_.net.liftweb.json.JsonAST._
import _root_.net.liftweb.json.JsonParser
import _root_.net.liftweb.http.js.JE.Str
import _root_.net.liftweb.record.{Field, FieldHelpers, Record}

import com.mongodb._
import org.bson.types.ObjectId

/**
* List field. Compatible with most object types. Including Pattern, ObjectId, DBRef.
*/
class MongoListField[OwnerType <: MongoRecord[OwnerType], ListType](rec: OwnerType)
  extends Field[List[ListType], OwnerType]
  with MongoFieldFlavor[List[ListType]] {

  import Meta.Reflection._
  
  def owner = rec

  def toForm = NodeSeq.Empty // FIXME
  def asXHtml = NodeSeq.Empty // FIXME
  
  def asJs = Str(toString) // FIXME
  
  def asJValue = JArray(value.map(li => li match {
    case s: String => JString(s)
    case d: Double => JDouble(d)
    case i: Int => JInt(i)
    case b: Boolean => JBool(b)
    case _ => JNothing
  }))
  
  def setFromJValue(jvalue: JValue) = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JArray(arr) => setBox(Full(arr.map(_.values.asInstanceOf[ListType])))
    case other => setBox(FieldHelpers.expectedA("JArray", other))
  }

  def defaultValue = List[ListType]()

  def setFromAny(in: Any): Box[List[ListType]] = {
    in match {
      case dbo: DBObject => setFromDBObject(dbo)
      case list: List[ListType] => setBox(Full(list))
      case Some(list: List[ListType]) => setBox(Full(list))
      case Full(list: List[ListType]) => setBox(Full(list))
      case s: String => setFromString(s)
      case Some(s: String) => setFromString(s)
      case Full(s: String) => setFromString(s)
      case null|None|Empty => setBox(defaultValueBox)
      case f: Failure => setBox(f)
      case o => setFromString(o.toString)
    }
  }

  // parse String into a JObject
  def setFromString(in: String): Box[List[ListType]] = setFromJValue(JsonParser.parse(in))

  /*
  * Convert this field's value into a DBObject so it can be stored in Mongo.
  * Override this method for custom logic.
  */
  def asDBObject: DBObject = {
    val dbl = new BasicDBList

    implicit val formats = owner.meta.formats

    value.foreach {
      case jo: JsonObject[Any] => dbl.add(JObjectParser.parse(jo.asJObject)) // A case class that extends JsonObject @Deprecated
      case f =>	f.asInstanceOf[AnyRef] match {
        case x if primitive_?(x.getClass) => dbl.add(x)
        case x if datetype_?(x.getClass) => dbl.add(datetype2dbovalue(x))
        case x if mongotype_?(x.getClass) => dbl.add(mongotype2dbovalue(x, formats))
        case o => dbl.add(o.toString)
      }
    }
    dbl
  }

  // set this field's value using a DBObject returned from Mongo.
  def setFromDBObject(dbo: DBObject): Box[List[ListType]] =
    setBox(Full(dbo.keySet.map(k =>
      dbo.get(k.toString).asInstanceOf[ListType]).toList))
}

/*
* List of Dates. You can also just use MongListField(OwnerType, Date).
*/
class MongoDateListField[OwnerType <: MongoRecord[OwnerType]](rec: OwnerType)
  extends MongoListField[OwnerType, Date](rec: OwnerType) {

  override def setFromDBObject(dbo: DBObject): Box[List[Date]] = {
    val ret = dbo.keySet.flatMap( k => {
      (dbo.get(k.toString) match {
        case d: Date => Some(d)
        case _ => None
      }).toList
    })
    Full(set(ret.toList))
  }
}

/*
* List of JObjects
*/
@Deprecated
class MongoJObjectListField[OwnerType <: MongoRecord[OwnerType]](rec: OwnerType)
  extends MongoListField[OwnerType, JObject](rec: OwnerType) {
  
  override def setFromDBObject(dbo: DBObject): Box[List[JObject]] = {
    implicit val formats = owner.meta.formats
    val ret = dbo.keySet.map( k => {
      JObjectParser.serialize(dbo.get(k.toString)).asInstanceOf[JObject]
    })
    Full(set(ret.toList))
  }
}

/*
* List of JsonObject case classes
*/
class MongoJsonObjectListField[OwnerType <: MongoRecord[OwnerType], JObjectType <: JsonObject[JObjectType]]
  (rec: OwnerType, valueMeta: JsonObjectMeta[JObjectType])
  extends MongoListField[OwnerType, JObjectType](rec: OwnerType) {

  override def asDBObject: DBObject = {
    val dbl = new BasicDBList

    implicit val formats = owner.meta.formats
    
    value.foreach { v => dbl.add(JObjectParser.parse(v.asJObject)) }

    dbl
  }
  
  override def setFromDBObject(dbo: DBObject): Box[List[JObjectType]] = {
    implicit val formats = owner.meta.formats
    val ret = dbo.keySet.map(k => {
      valueMeta.create(JObjectParser.serialize(dbo.get(k.toString)).asInstanceOf[JObject])
    })
    Full(set(ret.toList))
  }
}



}
}
}
}
