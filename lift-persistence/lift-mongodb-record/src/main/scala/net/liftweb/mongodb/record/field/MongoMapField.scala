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
package mongodb.record.field {

import _root_.net.liftweb.common.{Box, Empty, Failure, Full}
import _root_.net.liftweb.http.js.JE.Str
import _root_.net.liftweb.json.JsonAST.{JNothing, JValue}
import _root_.net.liftweb.record.{Field, Record}
import _root_.net.liftweb.util.Log

import com.mongodb._

class MongoMapField[OwnerType <: MongoRecord[OwnerType], MapValueType](rec: OwnerType)
  extends Field[Map[String, MapValueType], OwnerType]
  with MongoFieldFlavor[Map[String, MapValueType]] {

  def asJs = Str(toString) // not implemented
  
  def asJValue = (JNothing: JValue) // not implemented
  
  def setFromJValue(jvalue: JValue) = Empty // not implemented

  def asXHtml = <div></div> // not implemented

  def defaultValue = Map[String, MapValueType]()

  def setFromAny(in: Any): Box[Map[String, MapValueType]] = {
    in match {
      case map: Map[String, MapValueType] => Full(set(map))
      case Some(map: Map[String, MapValueType]) => Full(set(map))
      case Full(map: Map[String, MapValueType]) => Full(set(map))
      case dbo: DBObject => setFromDBObject(dbo)
      case seq: Seq[Map[String, MapValueType]] if !seq.isEmpty => setFromAny(seq(0))
      case null => Full(set(null))
      case s: String => setFromString(s)
      case None | Empty | Failure(_, _, _) => Full(set(null))
      case o => {
        println("setFromString: "+o.toString)
        setFromString(o.toString)
      }
    }
  }

  def toForm = <div></div> // not implemented

  def owner = rec

  /*
  * Convert this field's value into a DBObject so it can be stored in Mongo.
  * Compatible with most object types. Including Pattern, ObjectId, JObject,
  * and JsonObject case classes
  * Override this method for custom logic.
  */
  def asDBObject: DBObject = {
    val dbo = new BasicDBObject

    for (k <- value.keys)
      dbo.put(k.toString, value.getOrElse(k, ""))

    dbo
  }

  // set this field's value using a DBObject returned from Mongo.
  def setFromDBObject(dbo: DBObject): Box[Map[String, MapValueType]] = {
    import scala.collection.jcl.Conversions._
    //import scala.collection.mutable.{Map => MMap}

    var ret = Map[String, MapValueType]()

    for (k <- dbo.keySet)
      ret += (k.toString -> dbo.get(k).asInstanceOf[MapValueType])

    Full(set(ret))
  }
}

}
}
