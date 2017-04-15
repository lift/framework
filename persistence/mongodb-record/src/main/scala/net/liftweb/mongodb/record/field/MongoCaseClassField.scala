/*
* Copyright 2010-2015 WorldWide Conferencing, LLC
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

import net.liftweb.record._
import net.liftweb.record.RecordHelpers.jvalueToJsExp
import net.liftweb.record.field._
import net.liftweb.http.js.JE.{JsObj, Num, Str, JsNull}
import xml.{Text, NodeSeq}
import net.liftweb.mongodb.JObjectParser
import com.mongodb.{BasicDBList, DBObject}
import net.liftweb.common.{Failure, Empty, Full, Box}
import net.liftweb.util.Helpers
import net.liftweb.json._
import reflect.Manifest
import net.liftweb.http.js.JsExp
import org.bson.Document
import scala.collection.JavaConverters._
class MongoCaseClassField[OwnerType <: Record[OwnerType],CaseType](rec: OwnerType)( implicit mf: Manifest[CaseType]) extends Field[CaseType, OwnerType] with MandatoryTypedField[CaseType] with MongoFieldFlavor[CaseType] {

  // override this for custom formats
  def formats: Formats = DefaultFormats
  implicit lazy val _formats = formats

  override type MyType = CaseType

  def owner = rec

  def asXHtml = Text(value.toString)

  def toForm: Box[NodeSeq] = Empty

  override def defaultValue = null.asInstanceOf[MyType]
  override def optional_? = true

  def asJValue: JValue = valueBox.map(v => Extraction.decompose(v)) openOr (JNothing: JValue)

  def setFromJValue(jvalue: JValue): Box[CaseType] = jvalue match {
    case JNothing|JNull => setBox(Empty)
    case s => setBox(Helpers.tryo[CaseType]{ s.extract[CaseType] })
  }

  def asDBObject: DBObject = {
    JObjectParser.parse(asJValue.asInstanceOf[JObject])
  }

  def setFromDocument(doc: Document): Box[CaseType] = {
    val jv = JObjectParser.serialize(doc)
    setFromJValue(jv)
  }

  def setFromDBObject(dbo: DBObject): Box[CaseType] = {
    val jvalue = JObjectParser.serialize(dbo)
    setFromJValue(jvalue)
  }

  override def setFromString(in: String): Box[CaseType] = {
    Helpers.tryo{ JsonParser.parse(in).extract[CaseType] }
  }

  def setFromAny(in: Any): Box[CaseType] = in match {
    case dbo: DBObject => setFromDBObject(dbo)
    case doc: org.bson.Document => setFromDocument(doc)
    case c if mf.runtimeClass.isInstance(c) => setBox(Full(c.asInstanceOf[CaseType]))
    case Full(c) if mf.runtimeClass.isInstance(c) => setBox(Full(c.asInstanceOf[CaseType]))
    case null|None|Empty     => setBox(defaultValueBox)
    case (failure: Failure)  => setBox(failure)
    case _ => setBox(defaultValueBox)
  }
}

class MongoCaseClassListField[OwnerType <: Record[OwnerType],CaseType](rec: OwnerType)( implicit mf: Manifest[CaseType]) extends Field[List[CaseType], OwnerType] with MandatoryTypedField[List[CaseType]] with MongoFieldFlavor[List[CaseType]] {

  // override this for custom formats
  def formats: Formats = DefaultFormats
  implicit lazy val _formats = formats

  override type MyType = List[CaseType]

  def owner = rec

  def asXHtml = Text(value.toString)

  def toForm: Box[NodeSeq] = Empty

  override def defaultValue: MyType = Nil
  override def optional_? = true

  def asJValue: JValue = JArray(value.map(v => Extraction.decompose(v)))

  def setFromJValue(jvalue: JValue): Box[MyType] = jvalue match {
    case JArray(contents) => setBox(Full(contents.flatMap(s => Helpers.tryo[CaseType]{ s.extract[CaseType] })))
    case _ => setBox(Empty)
  }

  def setFromDocumentList(list: java.util.List[Document]): Box[MyType] = {
    val objs = list.asScala.map{ d => JObjectParser.serialize(d) }
    setFromJValue(JArray(objs.toList))
  }

  def asDBObject: DBObject = {
    val dbl = new BasicDBList

    asJValue match {
      case JArray(list) =>
        list.foreach(v => dbl.add(JObjectParser.parse(v.asInstanceOf[JObject])))
      case _ =>
    }

    dbl
  }

  def setFromDBObject(dbo: DBObject): Box[MyType] = {
    val jvalue = JObjectParser.serialize(dbo)
    setFromJValue(jvalue)
  }

  def setFromAny(in: Any): Box[MyType] = in match {
    case dbo: DBObject => setFromDBObject(dbo)
    case list@c::xs if mf.runtimeClass.isInstance(c) =>  setBox(Full(list.asInstanceOf[MyType]))
    case jlist: java.util.List[_] => {
      if (!jlist.isEmpty) {
        val elem = jlist.get(0)
        if (elem.isInstanceOf[org.bson.Document]) {
          setFromDocumentList(jlist.asInstanceOf[java.util.List[org.bson.Document]])
        } else {
          setBox(Full(jlist.asScala.toList.asInstanceOf[MyType]))
        }
      } else {
        setBox(Full(Nil))
      }
    }
    case _ => setBox(Empty)
  }

  override def setFromString(in: String): Box[MyType] = {
    setFromJValue(JsonParser.parse(in))
  }
}

