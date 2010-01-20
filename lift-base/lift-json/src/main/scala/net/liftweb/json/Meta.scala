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

import java.lang.reflect.{Constructor => JConstructor, Field, Type}
import java.util.Date
import JsonAST._

private[json] object Meta {
  import com.thoughtworks.paranamer._
  
  /** Intermediate metadata format for case classes.
   *  This ADT is constructed (and then memoized) from given case class using reflection.
   *
   *  Example mapping.
   *
   *  package xx 
   *  case class Person(name: String, address: Address, children: List[Child])
   *  case class Address(street: String, city: String)
   *  case class Child(name: String, age: BigInt)
   *
   *  will produce following Mapping:
   *
   *  Constructor(None, "xx.Person", List(
   *    Value("name"),
   *    Constructor(Some("address"), "xx.Address", List(Value("street"), Value("city"))),
   *    ListConstructor("children", "xx.Child", List(Value("name"), Value("age")))))
   */
  sealed abstract class Mapping
  case class Value(path: String, targetType: Class[_]) extends Mapping
  case class Constructor(path: Option[String], targetType: Class[_], args: List[Mapping]) extends Mapping
  case class ListConstructor(path: String, targetType: Class[_], args: List[Mapping]) extends Mapping
  case class ListOfPrimitives(path: String, elementType: Class[_]) extends Mapping
  case class Optional(mapping: Mapping) extends Mapping

  private val mappings = new Memo[Class[_], Mapping]
  private val unmangledNames = new Memo[String, String]
  private val paranamer = new CachingParanamer(new BytecodeReadingParanamer)

  private[json] def mappingOf(clazz: Class[_]) = {
    import Reflection._

    def makeMapping(path: Option[String], clazz: Class[_], isList: Boolean): Mapping = isList match {
      case false => Constructor(path, clazz, constructorArgs(clazz))
      case true if primitive_?(clazz) => ListOfPrimitives(path.get, clazz)
      case true => ListConstructor(path.get, clazz, constructorArgs(clazz))
    }

    def constructorArgs(clazz: Class[_]) = orderedConstructorArgs(clazz).map { f =>
      fieldMapping(unmangleName(f), f.getType, f.getGenericType)
    }.toList

    def orderedConstructorArgs(clazz: Class[_]): Iterable[Field] = {
      safePrimaryConstructorOf(clazz) match {
        case Some(x) => 
          val names = paranamer.lookupParameterNames(x)
          val fields = Map() ++ clazz.getDeclaredFields.filter(!static_?(_)).map(f => (f.getName, f))
          for { n <- names } yield fields(n)
        case None => Nil
      }
    }

    def fieldMapping(name: String, fieldType: Class[_], genericType: Type): Mapping = 
      if (primitive_?(fieldType)) Value(name, fieldType)
      else if (fieldType == classOf[List[_]]) makeMapping(Some(name), typeParameter(genericType), true)
      else if (classOf[Option[_]].isAssignableFrom(fieldType))
        if (container_?(genericType)) {
          val types = containerTypes(genericType)
          Optional(fieldMapping(name, types._1, types._2))
        } else Optional(fieldMapping(name, typeParameter(genericType), null))
      else makeMapping(Some(name), fieldType, false)

    mappings.memoize(clazz, makeMapping(None, _, false))
  }

  private[json] def unmangleName(f: Field) = 
    unmangledNames.memoize(f.getName, operators.foldLeft(_)((n, o) => n.replace(o._1, o._2)))

  private[json] def fail(msg: String) = throw new MappingException(msg)

  private val operators = Map("$eq" -> "=", "$greater" -> ">", "$less" -> "<", "$plus" -> "+", "$minus" -> "-",
                      "$times" -> "*", "$div" -> "/", "$bang" -> "!", "$at" -> "@", "$hash" -> "#",
                      "$percent" -> "%", "$up" -> "^", "$amp" -> "&", "$tilde" -> "~", "$qmark" -> "?",
                      "$bar" -> "|", "$bslash" -> "\\")

  private class Memo[A, R] {
    private var cache = Map[A, R]()

    def memoize(x: A, f: A => R): R = synchronized {
      if (cache contains x) cache(x) else {
        val ret = f(x)
        cache += (x -> ret)
        ret
      }
    }
  }

  object Reflection {
    import java.lang.reflect._

    val primitives = Set[Class[_]](classOf[String], classOf[Int], classOf[Long], classOf[Double], 
                                   classOf[Float], classOf[Byte], classOf[BigInt], classOf[Boolean], 
                                   classOf[Short], classOf[java.lang.Integer], classOf[java.lang.Long], 
                                   classOf[java.lang.Double], classOf[java.lang.Float], 
                                   classOf[java.lang.Byte], classOf[java.lang.Boolean], 
                                   classOf[java.lang.Short], classOf[Date], classOf[Symbol])

    def safePrimaryConstructorOf[A](cl: Class[A]): Option[JConstructor[A]] = 
      cl.getDeclaredConstructors.toList.asInstanceOf[List[JConstructor[A]]] match {
        case Nil => None
        case x :: xs => Some[JConstructor[A]](x)
      }

    def primaryConstructorOf[A](cl: Class[A]): JConstructor[A] = 
      safePrimaryConstructorOf(cl).getOrElse(fail("Can't find primary constructor for class " + cl))

    def typeParameter(t: Type): Class[_] = {
      val ptype = t.asInstanceOf[ParameterizedType]
      ptype.getActualTypeArguments()(0) match {
        case c: Class[_] => c
        case p: ParameterizedType => p.getRawType.asInstanceOf[Class[_]]
        case x => fail("do not know how to get type parameter from " + x)
      }
    }

    def containerTypes(t: Type): (Class[_], Type) = {
      val ptype = t.asInstanceOf[ParameterizedType]
      val c = ptype.getActualTypeArguments()(0).asInstanceOf[ParameterizedType]
      val ctype = c.getRawType.asInstanceOf[Class[_]]
      (ctype, c)
    }

    def primitive_?(clazz: Class[_]) = primitives contains clazz
    def static_?(f: Field) = Modifier.isStatic(f.getModifiers)
    def container_?(t: Type) = 
      t.asInstanceOf[ParameterizedType].getActualTypeArguments()(0).isInstanceOf[ParameterizedType]

    def primitive2jvalue(a: Any)(implicit formats: Formats) = a match {
      case x: String => JString(x)
      case x: Int => JInt(x)
      case x: Long => JInt(x)
      case x: Double => JDouble(x)
      case x: Float => JDouble(x)
      case x: Byte => JInt(BigInt(x))
      case x: BigInt => JInt(x)
      case x: Boolean => JBool(x)
      case x: Short => JInt(BigInt(x))
      case x: java.lang.Integer => JInt(BigInt(x.asInstanceOf[Int]))
      case x: java.lang.Long => JInt(BigInt(x.asInstanceOf[Long]))
      case x: java.lang.Double => JDouble(x.asInstanceOf[Double])
      case x: java.lang.Float => JDouble(x.asInstanceOf[Float])
      case x: java.lang.Byte => JInt(BigInt(x.asInstanceOf[Byte]))
      case x: java.lang.Boolean => JBool(x.asInstanceOf[Boolean])
      case x: java.lang.Short => JInt(BigInt(x.asInstanceOf[Short]))
      case x: Date => JString(formats.dateFormat.format(x))
      case x: Symbol => JString(x.name)
      case _ => error("not a primitive " + a.asInstanceOf[AnyRef].getClass)
    }
  }
}

class MappingException(msg: String, cause: Exception) extends Exception(msg, cause) {
  def this(msg: String) = this(msg, null)
}

}
}
