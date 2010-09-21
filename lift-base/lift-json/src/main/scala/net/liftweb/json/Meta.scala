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

import java.lang.reflect.{Constructor => JConstructor, Field, Type, ParameterizedType}
import java.util.Date
import JsonAST._

case class TypeInfo(clazz: Class[_], parameterizedType: Option[ParameterizedType])

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
   *    Arg("children", Col(classOf[List[_]], Constructor("xx.Child", List(Value("name"), Value("age")))))))
   */
  sealed abstract class Mapping
  case class Arg(path: String, mapping: Mapping, optional: Boolean) extends Mapping
  case class Value(targetType: Class[_]) extends Mapping
  case class Cycle(targetType: Class[_]) extends Mapping
  case class Dict(mapping: Mapping) extends Mapping
  case class Col(targetType: Class[_], mapping: Mapping) extends Mapping
  case class Constructor(targetType: TypeInfo, choices: List[DeclaredConstructor]) extends Mapping {
    def bestMatching(argNames: List[String]): DeclaredConstructor = {
      val names = Set(argNames: _*)
      def countOptionals(args: List[Arg]) =
        args.foldLeft(0)((n, x) => if (x.optional) n+1 else n)
      def score(args: List[Arg]) =
        args.foldLeft(0)((s, arg) => if (names.contains(arg.path)) s+1 else -100)

      val best = choices.tail.foldLeft((choices.head, score(choices.head.args))) { (best, c) =>
        val newScore = score(c.args)
        if (newScore == best._2) {
          if (countOptionals(c.args) < countOptionals(best._1.args))
            (c, newScore) else best
        } else if (newScore > best._2) (c, newScore) else best
      }
      best._1
    }
  }

  case class DeclaredConstructor(constructor: JConstructor[_], args: List[Arg])

  private val mappings = new Memo[Class[_], Mapping]
  private val unmangledNames = new Memo[String, String]
  private val paranamer = new CachingParanamer(new BytecodeReadingParanamer)

  private[json] def mappingOf(clazz: Class[_]) = {
    import Reflection._

    def constructors(clazz: Class[_], visited: Set[Class[_]]) =
      Reflection.constructors(clazz).map { case (c, args) =>
        DeclaredConstructor(c, args.map { case (name, atype, genericType) =>
          toArg(unmangleName(name), atype, genericType, visited) })
      }

    def toArg(name: String, fieldType: Class[_], genericType: Type, visited: Set[Class[_]]): Arg = {
      def mkContainer(t: Type, k: Kind, valueTypeIndex: Int, factory: Mapping => Mapping) =
        if (typeConstructor_?(t)) {
          val types = typeConstructors(t, k)(valueTypeIndex)
          factory(fieldMapping(types._1, types._2)._1)
        } else factory(fieldMapping(typeParameters(t, k)(valueTypeIndex), null)._1)

      def parameterizedTypeOpt(t: Type) = t match {
        case x: ParameterizedType => Some(x)
        case _ => None
      }

      def fieldMapping(fType: Class[_], genType: Type): (Mapping, Boolean) = {
        if (primitive_?(fType)) (Value(fType), false)
        else if (classOf[List[_]].isAssignableFrom(fType))
          (mkContainer(genType, `* -> *`, 0, Col.apply(classOf[List[_]], _)), false)
        else if (classOf[Set[_]].isAssignableFrom(fType))
          (mkContainer(genType, `* -> *`, 0, Col.apply(classOf[Set[_]], _)), false)
        else if (fType.isArray)
          (mkContainer(genType, `* -> *`, 0, Col.apply(fType, _)), false)
        else if (classOf[Option[_]].isAssignableFrom(fType))
          (mkContainer(genType, `* -> *`, 0, identity _), true)
        else if (classOf[Map[_, _]].isAssignableFrom(fType))
          (mkContainer(genType, `(*,*) -> *`, 1, Dict.apply _), false)
        else {
          if (visited.contains(fType)) (Cycle(fType), false)
          else (Constructor(TypeInfo(fType, parameterizedTypeOpt(genType)),
                            constructors(fType, visited + fType)), false)
        }
      }

      val (mapping, optional) = fieldMapping(fieldType, genericType)
      Arg(name, mapping, optional)
    }

    if (primitive_?(clazz)) Value(clazz)
    else mappings.memoize(clazz, c => Constructor(TypeInfo(c, None), constructors(c, Set())))
  }

  private[json] def unmangleName(name: String) =
    unmangledNames.memoize(name, operators.foldLeft(_)((n, o) => n.replace(o._1, o._2)))

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
    import scala.collection.JavaConversions._

    private val cachedConstructorArgs = new Memo[JConstructor[_], List[(String, Class[_], Type)]]

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

    def constructors(clazz: Class[_]): List[(JConstructor[_], List[(String, Class[_], Type)])] =
      clazz.getDeclaredConstructors.map(c => (c, constructorArgs(c))).toList

    def constructorArgs(constructor: JConstructor[_]): List[(String, Class[_], Type)] = {
      def argsInfo(c: JConstructor[_]) = {
        val Name = """^((?:[^$]|[$][^0-9]+)+)([$][0-9]+)?$"""r
        def clean(name: String) = name match {
          case Name(text, junk) => text
        }
        try {
          val names = paranamer.lookupParameterNames(c).map(clean)
          val types = c.getParameterTypes
          val ptypes = c.getGenericParameterTypes
          zip3(names.toList, types.toList, ptypes.toList)
        } catch {
          case e: ParameterNamesNotFoundException => Nil
        }
      }

      cachedConstructorArgs.memoize(constructor, argsInfo(_))
    }

    def primaryConstructorArgs(c: Class[_]) = constructorArgs(c.getDeclaredConstructors()(0))

    // FIXME Replace this with Tuple3.zipped when moving to 2.8
    private def zip3[A, B, C](l1: List[A], l2: List[B], l3: List[C]): List[(A, B, C)] = {
      def zip(x1: List[A], x2: List[B], x3: List[C], acc: List[(A, B, C)]): List[(A, B, C)] =
        x1 match {
          case Nil => acc.reverse
          case x :: xs => zip(xs, x2.tail, x3.tail, (x, x2.head, x3.head) :: acc)
        }

      zip(l1, l2, l3, Nil)
    }

    def typeParameters(t: Type, k: Kind): List[Class[_]] = {
      def term(i: Int) = t match {
        case ptype: ParameterizedType => ptype.getActualTypeArguments()(i) match {
          case c: Class[_] => c
          case p: ParameterizedType => p.getRawType.asInstanceOf[Class[_]]
          case x => fail("do not know how to get type parameter from " + x)
        }
        case clazz: Class[_] if (clazz.isArray) => i match {
          case 0 => clazz.getComponentType.asInstanceOf[Class[_]]
          case _ => fail("Arrays only have one type parameter")
        }
        case clazz: GenericArrayType => i match {
          case 0 => clazz.getGenericComponentType.asInstanceOf[Class[_]]
          case _ => fail("Arrays only have one type parameter")
        }
        case _ => fail("Unsupported Type: " + t + " (" + t.getClass + ")")
      }

      k match {
        case `* -> *`     => List(term(0))
        case `(*,*) -> *` => List(term(0), term(1))
      }
    }

    def typeConstructors(t: Type, k: Kind): List[(Class[_], Type)] = {
      def types(i: Int): (Class[_], Type) = {
        val ptype = t.asInstanceOf[ParameterizedType]
        ptype.getActualTypeArguments()(i) match {
          case p: ParameterizedType => (p.getRawType.asInstanceOf[Class[_]], p)
          case c: Class[_] => (c, c)
        }
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

case class MappingException(msg: String, cause: Exception) extends Exception(msg, cause) {
  def this(msg: String) = this(msg, null)
}

}
}
