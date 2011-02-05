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
package record {
package field {

import _root_.net.liftweb.common.{Box, Empty, Failure, Full}
import _root_.net.liftweb.http.js.JE.Str
import _root_.net.liftweb.json.JsonAST.{JNothing, JObject, JValue}
import _root_.net.liftweb.record.{Field, MandatoryTypedField, Record}
import _root_.scala.xml.NodeSeq

import com.mongodb.{BasicDBObject, BasicDBObjectBuilder, DBObject, DBRef}
import com.mongodb.util.JSON
import org.bson.types.ObjectId

/*
* Field for storing a DBRef
*/
class DBRefField[OwnerType <: MongoRecord[OwnerType], RefType <: MongoRecord[RefType]](rec: OwnerType, ref: RefType)
  extends Field[DBRef, OwnerType] with MandatoryTypedField[DBRef] {

  /*
  * get the referenced object
  */
  def obj = synchronized {
    if (!_calcedObj) {
      _calcedObj = true
      this._obj = ref.meta.findAny(value.getId)
    }
    _obj
  }

  def cached_? : Boolean = synchronized { _calcedObj }

  def primeObj(obj: Box[RefType]) = synchronized {
    _obj = obj
    _calcedObj = true
  }

  private var _obj: Box[RefType] = Empty
  private var _calcedObj = false

  def asJs = Str(toString)

  def asJValue = (JNothing: JValue) // not implemented

  def setFromJValue(jvalue: JValue) = Empty // not implemented

  def asXHtml = <div></div>

  def defaultValue = new DBRef(null, null, null)

  def setFromAny(in: Any): Box[DBRef] = in match {
    case ref: DBRef => Full(set(ref))
    case Some(ref: DBRef) => Full(set(ref))
    case Full(ref: DBRef) => Full(set(ref))
    case seq: Seq[_] if !seq.isEmpty => seq.map(setFromAny).apply(0)
    case (s: String) :: _ => setFromString(s)
    case null => Full(set(null))
    case s: String => setFromString(s)
    case None | Empty | Failure(_, _, _) => Full(set(null))
    case o => setFromString(o.toString)
  }

  // assume string is json
  def setFromString(in: String): Box[DBRef] = {
    val dbo = JSON.parse(in).asInstanceOf[BasicDBObject]
    MongoDB.use(ref.meta.mongoIdentifier) ( db => {
      val id = dbo.get("$id").toString
      ObjectId.isValid(id) match {
        case true => Full(set(new DBRef(db, dbo.get("$ref").toString, new ObjectId(id))))
        case false => Full(set(new DBRef(db, dbo.get("$ref").toString, id)))
      }
    })
  }

  def toForm: Box[NodeSeq] = Empty

  def owner = rec
}

}
}
}
}
