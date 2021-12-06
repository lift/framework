/*
* Copyright 2010-2020 WorldWide Conferencing, LLC
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

import net.liftweb.common.{Failure, Empty, Full, Box}
import net.liftweb.http.js.JsExp
import net.liftweb.http.js.JE.{JsObj, JsRaw, Num, Str, JsNull}
import net.liftweb.json._
import net.liftweb.record._
import net.liftweb.record.RecordHelpers.jvalueToJsExp
import net.liftweb.record.field._
import net.liftweb.util.Helpers

import scala.jdk.CollectionConverters._
import scala.reflect.Manifest
import scala.xml.{Text, NodeSeq}

import org.bson._
import org.bson.codecs.{BsonDocumentCodec, BsonTypeCodecMap, Codec, DecoderContext, EncoderContext}
import org.bson.codecs.configuration.CodecRegistry
import com.mongodb.{BasicDBList, DBObject}

abstract class CaseClassTypedField[OwnerType <: Record[OwnerType], CaseType](val owner: OwnerType)(implicit mf: Manifest[CaseType])
  extends Field[CaseType, OwnerType] with MongoFieldFlavor[CaseType] with BsonableField[CaseType] {

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

  def setFromBsonReader(reader: BsonReader, context: DecoderContext, registry: CodecRegistry, bsonTypeCodecMap: BsonTypeCodecMap): Box[MyType] = {
    reader.getCurrentBsonType match {
      case BsonType.DOCUMENT =>
        val doc = readValueToBsonDocument(reader, context, registry)
        setFromJValue(BsonParser.serialize(doc))
      case BsonType.NULL =>
        reader.readNull()
        Empty
      case bsonType =>
        Failure(s"Invalid BsonType for field ${name}: ${bsonType}")
    }
  }

  def writeToBsonWriter(writer: BsonWriter, context: EncoderContext, registry: CodecRegistry, bsonTypeCodecMap: BsonTypeCodecMap): Unit = {
    asJValue match {
      case jo: JObject =>
        writer.writeName(name)
        val codec = (new BsonDocumentCodec(registry)).asInstanceOf[Codec[Any]]
        context.encodeWithChildContext(codec, writer, BsonParser.parse(jo))
      case JNull if optional_? =>
      case JNull =>
        writer.writeName(name)
        writer.writeNull()
      case _ =>
    }
  }

  /**
   * Returns the field's value as a valid JavaScript expression
   */
  override def asJs = asJValue match {
    case JNothing => JsNull
    case jv => JsRaw(compactRender(jv))
  }

  @deprecated("This was replaced with the functions from 'BsonableField'.", "3.4.2")
  def asDBObject: DBObject = asJValue match {
    case JNothing | JNull => null
    case other => JObjectParser.parse(other.asInstanceOf[JObject])
  }

  def setFromDocument(doc: Document): Box[CaseType] = {
    val jv = JObjectParser.serialize(doc)
    setFromJValue(jv)
  }

  @deprecated("This was replaced with the functions from 'BsonableField'.", "3.4.2")
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
  extends Field[List[CaseType], OwnerType]
  with MandatoryTypedField[List[CaseType]]
  with MongoFieldFlavor[List[CaseType]]
  with BsonableField[List[CaseType]]
{
  // override this for custom formats
  def formats: Formats = DefaultFormats
  implicit lazy val _formats = formats

  override type MyType = List[CaseType]

  def asXHtml = Text(value.toString)

  def toForm: Box[NodeSeq] = Empty

  def defaultValue: MyType = Nil

  def asJValue: JValue = JArray(value.map(v => Extraction.decompose(v)))

  /**
   * Returns the field's value as a valid JavaScript expression
   */
  override def asJs = asJValue match {
    case JNothing => JsNull
    case jv => JsRaw(compactRender(jv))
  }

  def setFromJValue(jvalue: JValue): Box[MyType] = jvalue match {
    case JArray(contents) => setBox(Full(contents.flatMap(s => Helpers.tryo[CaseType]{ s.extract[CaseType] })))
    case _ => setBox(Empty)
  }

  @deprecated("This was replaced with the functions from 'BsonableField'.", "3.4.2")
  def setFromDocumentList(list: java.util.List[Document]): Box[MyType] = {
    val objs = list.asScala.map { JObjectParser.serialize }
    setFromJValue(JArray(objs.toList))
  }

  def setFromBsonReader(reader: BsonReader, context: DecoderContext, registry: CodecRegistry, bsonTypeCodecMap: BsonTypeCodecMap): Box[MyType] = {
    reader.getCurrentBsonType match {
      case BsonType.ARRAY =>
        setFromJValue(JArray(readArrayToBsonDocument(reader, context, registry).map { BsonParser.serialize _ }))
      case BsonType.NULL =>
        reader.readNull()
        Empty
      case bsonType =>
        Failure(s"Invalid BsonType for field ${name}: ${bsonType}")
    }
  }

  def writeToBsonWriter(writer: BsonWriter, context: EncoderContext, registry: CodecRegistry, bsonTypeCodecMap: BsonTypeCodecMap): Unit = {
    writer.writeName(name)
    writer.writeStartArray()

    asJValue match {
      case JArray(list) =>
        list.foreach { v =>
          val codec = (new BsonDocumentCodec(registry)).asInstanceOf[Codec[Any]]
          context.encodeWithChildContext(codec, writer, BsonParser.parse(v.asInstanceOf[JObject]))
        }
      case _ =>
    }

    writer.writeEndArray()
  }

  @deprecated("This was replaced with the functions from 'BsonableField'.", "3.4.2")
  def asDBObject: DBObject = {
    val dbl = new BasicDBList

    asJValue match {
      case JArray(list) =>
        list.foreach(v => dbl.add(JObjectParser.parse(v.asInstanceOf[JObject])))
      case _ =>
    }

    dbl
  }

  @deprecated("This was replaced with the functions from 'BsonableField'.", "3.4.2")
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
