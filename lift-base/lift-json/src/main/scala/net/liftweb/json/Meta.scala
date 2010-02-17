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
   *  Constructor("xx.Person", List(
   *    Arg("name", Value(classOf[String])),
   *    Arg("address", Constructor("xx.Address", List(Value("street"), Value("city")))),
   *    Arg("children", Lst(Constructor("xx.Child", List(Value("name"), Value("age")))))))
   */
  sealed abstract class Mapping
  case class Arg(path: String, mapping: Mapping) extends Mapping
  case class Value(targetType: Class[_]) extends Mapping
  case class Constructor(targetType: Class[_], args: List[Arg]) extends Mapping
  case class Cycle(targetType: Class[_]) extends Mapping
  case class Dict(mapping: Mapping) extends Mapping
  case class Lst(mapping: Mapping) extends Mapping
  case class Arr(mapping: Mapping) extends Mapping
  case class Optional(mapping: Mapping) extends Mapping

  private val mappings = new Memo[Class[_], Mapping]
  private val unmangledNames = new Memo[String, String]
  private val paranamer = new CachingParanamer(new BytecodeReadingParanamer)

  private[json] def mappingOf(clazz: Class[_]) = {
    import Reflection._

    def constructorArgs(clazz: Class[_], visited: Set[Class[_]]) = 
      orderedConstructorArgs(clazz).map { f =>
        toArg(unmangleName(f), f.getType, f.getGenericType, visited)
      }

    def toArg(name: String, fieldType: Class[_], genericType: Type, visited: Set[Class[_]]): Arg = {
      def mkContainer(t: Type, k: Kind, valueTypeIndex: Int, factory: Mapping => Mapping) = 
        if (typeConstructor_?(t)) {
          val types = typeConstructors(t, k)(valueTypeIndex)
          factory(fieldMapping(types._1, types._2))
        } else factory(fieldMapping(typeParameters(t, k)(valueTypeIndex), null))
        
      def fieldMapping(fType: Class[_], genType: Type): Mapping = {
        if (primitive_?(fType)) Value(fType)
        else if (classOf[List[_]].isAssignableFrom(fType)) 
          mkContainer(genType, `* -> *`, 0, Lst.apply _)
        else if (fType.isArray) 
          Arr(fieldMapping(fType.getComponentType, fType.getComponentType))
        else if (classOf[Option[_]].isAssignableFrom(fType)) 
          mkContainer(genType, `* -> *`, 0, Optional.apply _)
        else if (classOf[Map[_, _]].isAssignableFrom(fType)) 
          mkContainer(genType, `(*,*) -> *`, 1, Dict.apply _)
        else {
          if (visited.contains(fType)) Cycle(fType)
          else Constructor(fType, constructorArgs(fType, visited + fType))
        }}
     
      Arg(name, fieldMapping(fieldType, genericType))
    }

    mappings.memoize(clazz, c => Constructor(c, constructorArgs(c, Set())))
  }

  private[json] def unmangleName(f: Field) = 
    unmangledNames.memoize(f.getName, operators.foldLeft(_)((n, o) => n.replace(o._1, o._2)))

  private[json] def fail(msg: String) = throw new MappingException(msg)

  private val operators = Map(
    "$eq" -> "=", "$greater" -> ">", "$less" -> "<", "$plus" -> "+", "$minus" -> "-",
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

    private val primaryConstructorArgs = new Memo[Class[_], List[Field]]

    sealed abstract class Kind
    case object `* -> *` extends Kind
    case object `(*,*) -> *` extends Kind

    val primitives = Map[Class[_], Unit]() ++ (List[Class[_]](
      classOf[String], classOf[Int], classOf[Long], classOf[Double], 
      classOf[Float], classOf[Byte], classOf[BigInt], classOf[Boolean], 
      classOf[Short], classOf[java.lang.Integer], classOf[java.lang.Long], 
      classOf[java.lang.Double], classOf[java.lang.Float], 
      classOf[java.lang.Byte], classOf[java.lang.Boolean], classOf[Number],
      classOf[java.lang.Short], classOf[Date], classOf[Symbol]).map((_, ())))

    def orderedConstructorArgs(clazz: Class[_]): List[Field] = {
      def queryArgs(clazz: Class[_]): List[Field] = {
        val args = safePrimaryConstructorOf(clazz) match {
          case Some(x) => 
            val names = paranamer.lookupParameterNames(x)
            val fields = Map() ++ clazz.getDeclaredFields.filter(!static_?(_)).map(f => (f.getName, f))
            for { n <- names } yield fields(n)
          case None => Nil
        }
        args.toList
      }
      primaryConstructorArgs.memoize(clazz, queryArgs(_))
    }

    def safePrimaryConstructorOf[A](cl: Class[A]): Option[JConstructor[A]] = 
      cl.getDeclaredConstructors.toList.asInstanceOf[List[JConstructor[A]]] match {
        case Nil => None
        case x :: xs => Some[JConstructor[A]](x)
      }

    def primaryConstructorOf[A](cl: Class[A]): JConstructor[A] = 
      safePrimaryConstructorOf(cl).getOrElse(fail("Can't find primary constructor for class " + cl))

    def typeParameters(t: Type, k: Kind): List[Class[_]] = {
      def term(i: Int) = {
        val ptype = t.asInstanceOf[ParameterizedType]
        ptype.getActualTypeArguments()(i) match {
          case c: Class[_] => c
          case p: ParameterizedType => p.getRawType.asInstanceOf[Class[_]]
          case x => fail("do not know how to get type parameter from " + x)
        }
      }

      k match {
        case `* -> *`     => List(term(0))
        case `(*,*) -> *` => List(term(0), term(1))
      }
    }

    def typeConstructors(t: Type, k: Kind): List[(Class[_], Type)] = {
      def types(i: Int) = {
        val ptype = t.asInstanceOf[ParameterizedType]
        val c = ptype.getActualTypeArguments()(i).asInstanceOf[ParameterizedType]
        val ctype = c.getRawType.asInstanceOf[Class[_]]
        (ctype, c)
      }

      k match {
        case `* -> *`     => List(types(0))
        case `(*,*) -> *` => List(types(0), types(1))
      }
    }

    def primitive_?(clazz: Class[_]) = primitives contains clazz
    def static_?(f: Field) = Modifier.isStatic(f.getModifiers)
    def typeConstructor_?(t: Type) = t match {
      case p: ParameterizedType => 
        p.getActualTypeArguments.exists(_.isInstanceOf[ParameterizedType])
      case _ => false
    }

    def array_?(x: Any) = x != null && classOf[scala.Array[_]].isAssignableFrom(x.asInstanceOf[AnyRef].getClass)

    def mkJavaArray(x: Any, componentType: Class[_]) = {
      val arr = x.asInstanceOf[scala.Array[_]]
      val a = java.lang.reflect.Array.newInstance(componentType, arr.size)
      var i = 0
      while (i < arr.size) {
        java.lang.reflect.Array.set(a, i, arr(i))
        i += 1
      }
      a
    }

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
