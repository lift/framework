package net.liftweb.json

/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */

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
   */
  def extract[A](json: JValue)(implicit formats: Formats, mf: Manifest[A]): A = 
    try {
      extract0(json, formats, mf)
    } catch {
      case e: MappingException => throw e
      case e: Exception => throw new MappingException("unknown error", e)
    }

  /** Decompose a case class into JSON.
   * <p>
   * Example:<pre>
   * case class Person(name: String, age: Int)
   * implicit val formats = net.liftweb.json.DefaultFormats
   * Extraction.decompose(Person("joe", 25)) == JObject(JField("age",JInt(25)) :: JField("name",JString("joe")) :: Nil)
   * </pre>
   */
  def decompose(a: Any)(implicit formats: Formats): JValue = {
    def prependTypeHint(clazz: Class[_], o: JObject) = 
      JField("jsonClass", JString(formats.typeHints.hintFor(clazz))) ++ o

    def mkObject(clazz: Class[_], fields: List[JField]) = formats.typeHints.containsHint_?(clazz) match {
      case true => prependTypeHint(clazz, JObject(fields))
      case false => JObject(fields)
    }
 
    val serializer = formats.typeHints.serialize
    val any = a.asInstanceOf[AnyRef]
    if (!serializer.isDefinedAt(a)) {
      any match {
        case null => JNull
        case x if primitive_?(x.getClass) => primitive2jvalue(x)(formats)
        case x: List[_] => JArray(x map decompose)
        case x: Option[_] => decompose(x getOrElse JNothing)
        case x => 
          x.getClass.getDeclaredFields.toList.remove(static_?).map { f => 
            f.setAccessible(true)
            JField(unmangleName(f), decompose(f get x))
          } match {
            case Nil => JNothing
            case fields => mkObject(x.getClass, fields)
          }
      }
    } else prependTypeHint(any.getClass, serializer(any))
  }

  private def extract0[A](json: JValue, formats: Formats, mf: Manifest[A]): A = {
    val mapping = mappingOf(mf.erasure)

    def newInstance(targetType: Class[_], args: => List[Any], json: JValue) = {
      def instantiate(constructor: JConstructor[_], args: List[Any]) = 
        try {
          constructor.newInstance(args.map(_.asInstanceOf[AnyRef]).toArray: _*)
        } catch {
          case e @ (_:IllegalArgumentException | _:InstantiationException) => 
            fail("Parsed JSON values do not match with class constructor\nargs=" + args.mkString(",") + 
                 "\narg types=" + args.map(arg => if (arg != null) arg.asInstanceOf[AnyRef].getClass.getName else "null").mkString(",") + 
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
        case _ => instantiate(primaryConstructorOf(targetType), args)
      }
    }

    def newPrimitive(elementType: Class[_], elem: JValue) = convert(elem, elementType, formats)

    def build(root: JValue, mapping: Mapping): Any = mapping match {
      case Value(path, targetType) => convert(fieldValue(root, path), targetType, formats)
      case Constructor(path, targetType, args) => 
        val newRoot = path match {
          case Some(p) => root \ p
          case None => root
        }
        newInstance(targetType, args.map(build(newRoot, _)), newRoot)
      case Lst(Constructor(Some(path), targetType, args)) => 
        val arr = asArray(safeFieldValue(root, path).getOrElse(JArray(Nil)), path)
        arr.arr.map(elem => newInstance(targetType, args.map(build(elem, _)), elem))
      case Lst(Value(path, elementType)) =>
        val arr = asArray(fieldValue(root, path), path)
        arr.arr.map(elem => newPrimitive(elementType, elem))
      case Lst(m) => List(build(root, m))
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

    def asArray(json: JValue, path: String) = json match {
      case a: JArray => a
      case _ => fail("Expected JArray but got " + json + "', path='" + path + "'")
    }


    def safeFieldValue(json: JValue, path: String) = (json \ path) match {
      case JField(_, value) => Some(value)
      case x => None
    }

    def fieldValue(json: JValue, path: String) = safeFieldValue(json, path).getOrElse {
      fail("Expected JField but got " + (json \ path) + ", json='" + json + "', path='" + path + "'")
    }

    build(json, mapping).asInstanceOf[A]
  }

  private def convert(value: JValue, targetType: Class[_], formats: Formats): Any = value match {
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
    case _ => value.values
  }
}

