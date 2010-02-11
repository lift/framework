/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
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
package json {

import java.lang.reflect.{Constructor => JConstructor, Type}
import java.util.Date
import scala.reflect.Manifest
import JsonAST._

/** Function to extract values from JSON AST using case classes.
 *
 *  See: ExtractionExamples.scala
 */
object Extraction {
  import Meta._
  import Meta.Reflection._

  /** Extract a case class from JSON.
   * @see net.liftweb.json.JsonAST.JValue#extract
   * @throws MappingException is thrown if extraction fails
   */
  def extract[A](json: JValue)(implicit formats: Formats, mf: Manifest[A]): A = 
    try {
      extract0(json, formats, mf)
    } catch {
      case e: MappingException => throw e
      case e: Exception => throw new MappingException("unknown error", e)
    }

  /** Extract a case class from JSON.
   * @see net.liftweb.json.JsonAST.JValue#extract
   */
  def extractOpt[A](json: JValue)(implicit formats: Formats, mf: Manifest[A]): Option[A] = 
    try { Some(extract(json)(formats, mf)) } catch { case _: MappingException => None }

  /** Decompose a case class into JSON.
   * <p>
   * Example:<pre>
   * case class Person(name: String, age: Int)
   * implicit val formats = net.liftweb.json.DefaultFormats
   * Extraction.decompose(Person("joe", 25)) == JObject(JField("age",JInt(25)) :: JField("name",JString("joe")) :: Nil)
   * </pre>
   */
  def decompose(a: Any)(implicit formats: Formats): JValue = {
    def prependTypeHint(clazz: Class[_], o: JObject) = JField("jsonClass", JString(formats.typeHints.hintFor(clazz))) ++ o

    def mkObject(clazz: Class[_], fields: List[JField]) = formats.typeHints.containsHint_?(clazz) match {
      case true  => prependTypeHint(clazz, JObject(fields))
      case false => JObject(fields)
    }
 
    val serializer = formats.typeHints.serialize
    val any = a.asInstanceOf[AnyRef]
    if (!serializer.isDefinedAt(a)) {
      any match {
        case null => JNull
        case x if primitive_?(x.getClass) => primitive2jvalue(x)(formats)
        case x: List[_] => JArray(x map decompose)
        case x: Option[_] => x.flatMap[JValue] { y => Some(decompose(y)) }.getOrElse(JNothing)
        case x: Map[_, _] => JObject((x map { case (k: String, v) => JField(k, decompose(v)) }).toList)
        case x => 
          mkObject(x.getClass, 
            x.getClass.getDeclaredFields.toList.remove(static_?).map { f => 
              f.setAccessible(true)
            
              JField(unmangleName(f), decompose(f get x))
            }
          )
      }
    } else prependTypeHint(any.getClass, serializer(any))
  }

  private def extract0[A](json: JValue, formats: Formats, mf: Manifest[A]): A = {
    if (mf.erasure == classOf[List[_]] || mf.erasure == classOf[Map[_, _]])
      fail("Root object can't yet be List or Map (needs a feature from Scala 2.8)")
    val mapping = mappingOf(mf.erasure)

    def newInstance(targetType: Class[_], args: List[Arg], json: JValue) = {
      def instantiate(constructor: JConstructor[_], args: List[Any]) = 
        try {
          constructor.newInstance(args.map(_.asInstanceOf[AnyRef]).toArray: _*)
        } catch {
          case e @ (_:IllegalArgumentException | _:InstantiationException) =>             
            fail("Parsed JSON values do not match with class constructor\nargs=" + 
                 args.mkString(",") + "\narg types=" + args.map(a => if (a != null) 
                   a.asInstanceOf[AnyRef].getClass.getName else "null").mkString(",") + 
                 "\nconstructor=" + constructor)
        }

      def mkWithTypeHint(typeHint: String, fields: List[JField]) = {
        val obj = JObject(fields)
        val deserializer = formats.typeHints.deserialize
        if (!deserializer.isDefinedAt(typeHint, obj)) {
          val concreteClass = formats.typeHints.classFor(typeHint) getOrElse fail("Do not know how to deserialize '" + typeHint + "'")
          build(obj, mappingOf(concreteClass))
        } else deserializer(typeHint, obj)
      }

      json match {
        case JObject(JField("jsonClass", JString(t)) :: xs) => mkWithTypeHint(t, xs)
        case JField(_, JObject(JField("jsonClass", JString(t)) :: xs)) => mkWithTypeHint(t, xs)
        case _ => instantiate(primaryConstructorOf(targetType), args.map(a => build(json \ a.path, a)))
      }
    }

    def newPrimitive(elementType: Class[_], elem: JValue) = convert(elem, elementType, formats)

    def build(root: JValue, mapping: Mapping): Any = mapping match {
      case Value(targetType) => convert(root, targetType, formats)
      case Constructor(targetType, args) => newInstance(targetType, args, root)
      case Cycle(targetType) => mappingOf(targetType)
      case Arg(_, m) => build(fieldValue(root), m)
      case Lst(m) => root match {
        case JArray(arr) => arr.map(build(_, m))
        case JNothing | JNull => Nil
        case x => fail("Expected array but got " + x)
      }
      case Dict(m) => root match {
        case JObject(xs) => Map(xs.map(x => (x.name, build(x.value, m))): _*)
        case x => fail("Expected object but got " + x)
      }
      case Optional(m) =>
        // FIXME Remove this try-catch.
        try { 
          build(root, m) match {
            case null => None
            case x => Some(x)
          }
        } catch {
          case e: MappingException => None
        }
    }

    def fieldValue(json: JValue): JValue = json match {
      case JField(_, value) => value
      case JNothing => JNothing
      case x => fail("Expected JField but got " + x)
    }

    build(json, mapping).asInstanceOf[A]
  }

  private def convert(json: JValue, targetType: Class[_], formats: Formats): Any = json match {
    case JInt(x) if (targetType == classOf[Int]) => x.intValue
    case JInt(x) if (targetType == classOf[Long]) => x.longValue
    case JInt(x) if (targetType == classOf[Double]) => x.doubleValue
    case JInt(x) if (targetType == classOf[Float]) => x.floatValue
    case JInt(x) if (targetType == classOf[Short]) => x.shortValue
    case JInt(x) if (targetType == classOf[Byte]) => x.byteValue
    case JInt(x) if (targetType == classOf[String]) => x.toString
    case JDouble(x) if (targetType == classOf[Float]) => x.floatValue
    case JDouble(x) if (targetType == classOf[String]) => x.toString
    case JString(s) if (targetType == classOf[Symbol]) => Symbol(s)
    case JString(s) if (targetType == classOf[Date]) => formats.dateFormat.parse(s).getOrElse(fail("Invalid date '" + s + "'"))
    case JNull => null
    case JNothing => fail("Did not find value which can be converted into " + targetType.getName)
    case _ => json.values
  }
}

}
}
