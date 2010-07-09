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

private[mongodb] object Meta {

  /*
  * For converting scala objects into DBObject values
  */
  object Reflection {
    import java.lang.reflect._
    import java.util.{Calendar, Date, GregorianCalendar}
    import java.util.regex.Pattern

    import net.liftweb.json.Formats
    import net.liftweb.json.JsonAST._

    import com.mongodb.{BasicDBObject, DBRef}
    import org.bson.types.ObjectId

    /*
    * These don't require a conversion and can be put directly into a DBObject
    */
    val primitives = Set[Class[_]](classOf[String], classOf[Int], classOf[Long], classOf[Double],
                                  classOf[Float], classOf[Byte], classOf[BigDecimal], classOf[BigInt], classOf[Boolean],
                                  classOf[Short], classOf[java.lang.Integer], classOf[java.lang.Long],
                                  classOf[java.lang.Double], classOf[java.lang.Float],
                                  classOf[java.lang.Byte], classOf[java.lang.Boolean],
                                  classOf[java.lang.Short])

    def primitive_?(clazz: Class[_]) = primitives contains clazz

    /*
    * This is used to convert DBObjects into JObjects
    */
    def primitive2jvalue(a: Any) = a match {
      case x: Int => JInt(x)
      case x: Long => JInt(x)
      case x: Double => JDouble(x)
      case x: Float => JDouble(x)
      case x: Byte => JInt(BigInt(x))
      case x: BigInt => JInt(x)
      //case x: BigDecimal => JString(x.toString) // this keeps the scale intact
      case x: Boolean => JBool(x)
      case x: Short => JInt(BigInt(x))
      case x: String => JString(x)
      case x: java.lang.Integer => JInt(BigInt(x.asInstanceOf[Int]))
      case x: java.lang.Long => JInt(BigInt(x.asInstanceOf[Long]))
      case x: java.lang.Double => JDouble(x.asInstanceOf[Double])
      case x: java.lang.Float => JDouble(x.asInstanceOf[Float])
      case x: java.lang.Byte => JInt(BigInt(x.asInstanceOf[Byte]))
      case x: java.lang.Boolean => JBool(x.asInstanceOf[Boolean])
      case x: java.lang.Short => JInt(BigInt(x.asInstanceOf[Short]))
      case _ => error("not a primitive " + a.asInstanceOf[AnyRef].getClass)
    }

    /*
    * Date types require formatting
    */
    val datetypes = Set[Class[_]](classOf[Calendar], classOf[Date], classOf[GregorianCalendar])

    def datetype_?(clazz: Class[_]) = datetypes contains clazz

    def datetype2jvalue(a: Any)(implicit formats: Formats) = a match {
      case x: Calendar => JString(formats.dateFormat.format(x.getTime))
      case x: Date => JString(formats.dateFormat.format(x))
    }

    def datetype2dbovalue(a: Any) = a match {
      case x: Calendar => x.getTime
      case x: Date => x
    }

    /*
    * Extended Mongo types.
    */
    val mongotypes = Set[Class[_]](
      classOf[DBRef], classOf[MongoRef], classOf[JObject],
      classOf[ObjectId], classOf[Pattern])

    def mongotype_?(clazz: Class[_]) = mongotypes contains clazz

    def mongotype2dbovalue(a: Any, formats: Formats) = a match {
      case MongoRef(r, i) => new BasicDBObject("ref", r).append("id", i)
      case dbref: DBRef => dbref
      case jo: JObject => JObjectParser.parse(jo)(formats) // Any JObject. @Deprecated
      case oid: ObjectId => oid
      case p: Pattern => p
      case _ => error("not a mongotype " + a.asInstanceOf[AnyRef].getClass)
    }
/*
    def dbovalue2mongotype(a: Any) = a match {
      case c: Calendar => c //c.getTime
      case DBRef(r, i) => new BasicDBObject("ref", r).append("id", i)
      case jo: JObject => JObjectParser.parse(jo) // Any JObject
      case jo: JsonObject[Any] => JObjectParser.parse(jo.asJObject) // A case class that extends JsonObject
      case oid: ObjectId => oid
      case p: Pattern => p
      case _ => error("not a mongotype " + a.asInstanceOf[AnyRef].getClass)
    }
*/
  }
}

}
}
