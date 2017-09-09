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

abstract class CaseClassTypedField[OwnerType <: Record[OwnerType], CaseType](val owner: OwnerType)(implicit mf: Manifest[CaseType])
  extends Field[CaseType, OwnerType] with MongoFieldFlavor[CaseType] {

  // override this for custom formats
  def formats: Formats = DefaultFormats

  implicit lazy val _formats = formats

  override type MyType = CaseType


  def toForm: Box[NodeSeq] = Empty

  def asJValue: JValue = valueBox.map(Extraction.decompose) openOr (JNothing: JValue)

  def setFromJValue(jvalue: JValue): Box[CaseType] = jvalue match {
    case JNothing | JNull => setBox(Empty)
    case s => setBox(Helpers.tryo[CaseType] { s.extract[CaseType] })
  }

  def asDBObject: DBObject = asJValue match {
    case JNothing | JNull => null
    case other => JObjectParser.parse(other.asInstanceOf[JObject])
  }

  def setFromDocument(doc: Document): Box[CaseType] = {
    val jv = JObjectParser.serialize(doc)
    setFromJValue(jv)
  }

  def setFromDBObject(dbo: DBObject): Box[CaseType] = {
    val jvalue = JObjectParser.serialize(dbo)
    setFromJValue(jvalue)
  }

  def setFromString(in: String): Box[CaseType] = Helpers.tryo {
    JsonParser.parse(in).extract[CaseType]
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

class CaseClassField[OwnerType <: Record[OwnerType], CaseType](owner: OwnerType)(implicit mf: Manifest[CaseType])
  extends CaseClassTypedField[OwnerType, CaseType](owner) with MandatoryTypedField[CaseType] {


  def this(owner: OwnerType, value: CaseType)(implicit mf: Manifest[CaseType]) = {
    this(owner)
    setBox(Full(value))
  }

  def defaultValue = null.asInstanceOf[MyType]
}

@deprecated("Use the more consistently named 'CaseClassField' instead. This class will be removed in Lift 4.", "3.2")
class MongoCaseClassField[OwnerType <: Record[OwnerType], CaseType](@deprecatedName('rec) owner: OwnerType)(implicit mf: Manifest[CaseType])
  extends CaseClassField[OwnerType, CaseType](owner)

class OptionalCaseClassField[OwnerType <: Record[OwnerType], CaseType](owner: OwnerType)(implicit mf: Manifest[CaseType])
  extends CaseClassTypedField[OwnerType, CaseType](owner) with OptionalTypedField[CaseType] {

  def this(owner: OwnerType, value: Box[CaseType])(implicit mf: Manifest[CaseType]) = {
    this(owner)
    setBox(value)
  }
}

class CaseClassListField[OwnerType <: Record[OwnerType], CaseType](val owner: OwnerType)(implicit mf: Manifest[CaseType])
  extends Field[List[CaseType], OwnerType] with MandatoryTypedField[List[CaseType]] with MongoFieldFlavor[List[CaseType]] {

  // override this for custom formats
  def formats: Formats = DefaultFormats
  implicit lazy val _formats = formats

  override type MyType = List[CaseType]

  def asXHtml = Text(value.toString)

  def toForm: Box[NodeSeq] = Empty

  def defaultValue: MyType = Nil

  def asJValue: JValue = JArray(value.map(v => Extraction.decompose(v)))

  def setFromJValue(jvalue: JValue): Box[MyType] = jvalue match {
    case JArray(contents) => setBox(Full(contents.flatMap(s => Helpers.tryo[CaseType]{ s.extract[CaseType] })))
    case _ => setBox(Empty)
  }

  def setFromDocumentList(list: java.util.List[Document]): Box[MyType] = {
    val objs = list.asScala.map { JObjectParser.serialize }
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

  def setFromString(in: String): Box[MyType] = {
    setFromJValue(JsonParser.parse(in))
  }
}

@deprecated("Please use the more consistently named 'CaseClassListField' instead. This class will be removed in Lift 4.", "3.2")
class MongoCaseClassListField[OwnerType <: Record[OwnerType], CaseType](@deprecatedName('rec) owner: OwnerType)(implicit mf: Manifest[CaseType])
  extends CaseClassListField[OwnerType, CaseType](owner)
