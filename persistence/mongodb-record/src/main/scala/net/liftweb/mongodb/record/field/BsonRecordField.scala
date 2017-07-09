/*
 * Copyright 2011-2017 WorldWide Conferencing, LLC
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

import common._
import http.js.JsExp
import http.js.JE.JsNull
import json._
import net.liftweb.record._
import com.mongodb._
import org.bson.Document

import scala.reflect.Manifest
import scala.xml._

/** Field that contains an entire record represented as an inline object value. Inspired by JSONSubRecordField */
abstract class BsonRecordTypedField[OwnerType <: BsonRecord[OwnerType], SubRecordType <: BsonRecord[SubRecordType]]
(override val owner: OwnerType, valueMeta: BsonMetaRecord[SubRecordType])(implicit subRecordType: Manifest[SubRecordType])
  extends Field[SubRecordType, OwnerType] {

  def this(rec: OwnerType, valueMeta: BsonMetaRecord[SubRecordType], value: Box[SubRecordType])
    (implicit subRecordType: Manifest[SubRecordType]) = {
    this(rec, valueMeta)
    setBox(value)
  }

  override def asJs = asJValue match {
    case JNothing => JsNull
    case jv => new JsExp {
      lazy val toJsCmd = compactRender(jv)
    }
  }

  override def toForm: Box[NodeSeq] = Empty

  override def setFromString(s: String): Box[SubRecordType] = valueMeta.fromJsonString(s)

  override def setFromAny(in: Any): Box[SubRecordType] = in match {
    case dbo: DBObject => setBox(Full(valueMeta.fromDBObject(dbo)))
    case dbo: Document => setBox(Full(valueMeta.fromDocument(dbo)))
    case _ => genericSetFromAny(in)
  }

  override def asJValue: JValue = valueBox.map(_.asJValue) openOr (JNothing: JValue)

  override def setFromJValue(jvalue: JValue): Box[SubRecordType] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case _ => setBox(valueMeta.fromJValue(jvalue))
  }
}

class BsonRecordField[OwnerType <: BsonRecord[OwnerType], SubRecordType <: BsonRecord[SubRecordType]]
(@deprecatedName('rec) owner: OwnerType, valueMeta: BsonMetaRecord[SubRecordType])(implicit subRecordType: Manifest[SubRecordType])
  extends BsonRecordTypedField(owner, valueMeta) with MandatoryTypedField[SubRecordType] {

  def this(rec: OwnerType, valueMeta: BsonMetaRecord[SubRecordType], value: SubRecordType)(implicit subRecordType: Manifest[SubRecordType]) = {
    this(rec, value.meta)
    set(value)
  }

  override def defaultValue = valueMeta.createRecord
}

class OptionalBsonRecordField[OwnerType <: BsonRecord[OwnerType], SubRecordType <: BsonRecord[SubRecordType]]
(owner: OwnerType, valueMeta: BsonMetaRecord[SubRecordType])(implicit subRecordType: Manifest[SubRecordType])
  extends BsonRecordTypedField(owner, valueMeta) with OptionalTypedField[SubRecordType]


/*
 * List of BsonRecords
 */
class BsonRecordListField[OwnerType <: BsonRecord[OwnerType], SubRecordType <: BsonRecord[SubRecordType]]
  (@deprecatedName('rec) owner: OwnerType, valueMeta: BsonMetaRecord[SubRecordType])(implicit mf: Manifest[SubRecordType])
  extends MongoListField[OwnerType, SubRecordType](owner: OwnerType) {

  import scala.collection.JavaConverters._

  override def validations = ((elems: ValueType) => elems.flatMap(_.validate)) :: super.validations

  override def asDBObject: DBObject = {
    val dbl = new BasicDBList
    value.foreach { v => dbl.add(v.asDBObject) }
    dbl
  }

  override def setFromDBObject(dbo: DBObject): Box[List[SubRecordType]] = {
    setBox(Full(dbo.keySet.asScala.toList.map { k =>
      valueMeta.fromDBObject(dbo.get(k).asInstanceOf[DBObject])
    }))
  }

  override def asJValue: JValue = JArray(value.map(_.asJValue))

  override def setFromJValue(jvalue: JValue) = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JArray(arr) => setBox(Full(arr.map { jv =>
      valueMeta.fromJValue(jv) openOr valueMeta.createRecord
    }))
    case other => setBox(FieldHelpers.expectedA("JArray", other))
  }

  override def setFromDocumentList(list: java.util.List[Document]): Box[List[SubRecordType]] = {
    setBox(Full(
      list.asScala.toList.map { valueMeta.fromDocument }
    ))
  }
}
