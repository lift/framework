/*
* Copyright 2010-2017 WorldWide Conferencing, LLC
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

package net.liftweb
package mongodb
package record
package field

import java.util.UUID

import net.liftweb.common.{Box, Empty, Failure, Full}
import net.liftweb.http.S
import net.liftweb.http.js.JE.{JsNull, JsRaw}
import net.liftweb.http.js.JsExp
import net.liftweb.json._
import net.liftweb.record._
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

sealed trait UUIDTypedField[OwnerType <: BsonRecord[OwnerType]] extends TypedField[UUID] with Field[UUID, OwnerType] {
  def setFromAny(in: Any): Box[UUID] = in match {
    case uid: UUID => setBox(Full(uid))
    case Some(uid: UUID) => setBox(Full(uid))
    case Full(uid: UUID) => setBox(Full(uid))
    case (uid: UUID) :: _ => setBox(Full(uid))
    case s: String => setFromString(s)
    case Some(s: String) => setFromString(s)
    case Full(s: String) => setFromString(s)
    case null|None|Empty => setBox(defaultValueBox)
    case f: Failure => setBox(f)
    case o => setFromString(o.toString)
  }

  override def setFromJValue(jvalue: JValue): Box[UUID] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JObject(JField("$uuid", JString(s)) :: Nil) => setFromString(s)
    case other => setBox(FieldHelpers.expectedA("JObject", other))
  }

  override def setFromString(in: String): Box[UUID] = tryo(UUID.fromString(in)) match {
    case Full(uid: UUID) => setBox(Full(uid))
    case f: Failure => setBox(f)
    case _ => setBox(Failure(s"Invalid UUID string: $in"))
  }

  private def elem = S.fmapFunc(S.SFuncHolder(this.setFromAny(_))) { funcName =>
    <input type="text"
      name={funcName}
      value={valueBox.map(v => v.toString) openOr ""}
      tabindex={tabIndex.toString}/>
  }

  override def toForm: Box[NodeSeq] = uniqueFieldId match {
    case Full(id) => Full(elem % ("id" -> id))
    case _ => Full(elem)
  }

  override def asJs: JsExp = asJValue match {
    case JNothing => JsNull
    case jv => JsRaw(compactRender(jv))
  }

  override def asJValue: JValue = valueBox.map(v => JsonUUID(v)) openOr (JNothing: JValue)
}

class UUIDField[OwnerType <: BsonRecord[OwnerType]](override val owner: OwnerType)
  extends UUIDTypedField[OwnerType] with MandatoryTypedField[UUID] {

  def this(rec: OwnerType, value: UUID) = {
    this(rec)
    setBox(Full(value))
  }

  override def defaultValue = UUID.randomUUID

}

class OptionalUUIDField[OwnerType <: BsonRecord[OwnerType]](override val owner: OwnerType)
  extends UUIDTypedField[OwnerType] with OptionalTypedField[UUID] {

  def this(rec: OwnerType, value: Box[UUID]) = {
    this(rec)
    setBox(value)
  }

}

