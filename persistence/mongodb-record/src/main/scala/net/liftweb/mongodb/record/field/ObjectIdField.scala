/*
 * Copyright 2010-2017 WorldWide Conferencing, LLC
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

import java.util.Date

import net.liftweb.common.{Box, Empty, Failure, Full}
import net.liftweb.http.S
import net.liftweb.http.js.JE.{JsNull, JsRaw}
import net.liftweb.http.js.JsExp
import net.liftweb.json._
import net.liftweb.record._
import net.liftweb.util.Helpers._
import org.bson.types.ObjectId

trait ObjectIdTypedField[OwnerType <: BsonRecord[OwnerType]] extends TypedField[ObjectId] with Field[ObjectId, OwnerType] {

  def setFromAny(in: Any): Box[ObjectId] = in match {
    case oid: ObjectId => setBox(Full(oid))
    case Some(oid: ObjectId) => setBox(Full(oid))
    case Full(oid: ObjectId) => setBox(Full(oid))
    case (oid: ObjectId) :: _ => setBox(Full(oid))
    case s: String => setFromString(s)
    case Some(s: String) => setFromString(s)
    case Full(s: String) => setFromString(s)
    case null|None|Empty => setBox(defaultValueBox)
    case f: Failure => setBox(f)
    case o => setFromString(o.toString)
  }

  def setFromJValue(jvalue: JValue): Box[ObjectId] = jvalue match {
    case JNothing | JNull if optional_? => setBox(Empty)
    case JObject(JField("$oid", JString(s)) :: Nil) => setFromString(s)
    case JString(s) => setFromString(s)
    case other => setBox(FieldHelpers.expectedA("JObject", other))
  }

  def setFromString(in: String): Box[ObjectId] = {
    if (ObjectId.isValid(in)) {
      setBox (Full(new ObjectId(in)))
    } else {
      setBox(Failure(s"Invalid ObjectId string: $in"))
    }
  }

  private def elem =
    S.fmapFunc(S.SFuncHolder(this.setFromAny(_))) { funcName =>
      <input type="text"
        name={funcName}
        value={valueBox.map(s => s.toString) openOr ""}
        tabindex={tabIndex.toString}/>
    }

  override def toForm = uniqueFieldId match {
    case Full(id) => Full(elem % ("id" -> id))
    case _ => Full(elem)
  }

  override def asJs: JsExp = asJValue match {
    case JNothing => JsNull
    case jv => JsRaw(compactRender(jv))
  }

  def asJValue: JValue = valueBox.map(v => JsonObjectId.asJValue(v, owner.meta.formats)) openOr (JNothing: JValue)

}

class ObjectIdField[OwnerType <: BsonRecord[OwnerType]](@deprecatedName('rec) override val owner: OwnerType)
  extends MandatoryTypedField[ObjectId] with ObjectIdTypedField[OwnerType] {

  def this(owner: OwnerType, value: ObjectId) = {
    this(owner)
    setBox(Full(value))
  }

  override def defaultValue = new ObjectId

  def createdAt: Date = this.get.getDate

}

class OptionalObjectIdField[OwnerType <: BsonRecord[OwnerType]](override val owner: OwnerType)
  extends OptionalTypedField[ObjectId] with ObjectIdTypedField[OwnerType] {

  def this(owner: OwnerType, value: Box[ObjectId]) = {
    this(owner)
    setBox(value)
  }

}
