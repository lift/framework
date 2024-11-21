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

package net.liftweb
package json

// FIXME Needed to due to https://issues.scala-lang.org/browse/SI-6541,
// which causes existential types to be inferred for the generated
// unapply of a case class with a wildcard parameterized type.
// Ostensibly should be fixed in 2.12, which means we're a ways away
// from being able to remove this, though.
import scala.language.existentials

import java.lang.reflect.{Constructor => JConstructor, Field, Type, ParameterizedType, GenericArrayType}
import java.util.Date
import java.sql.Timestamp

case class TypeInfo(clazz: Class[_], parameterizedType: Option[ParameterizedType])

trait ParameterNameReader {
  def lookupParameterNames(constructor: JConstructor[_]): Iterable[String]
}

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
  case class Cycle(targetType: Type) extends Mapping
  case class Dict(mapping: Mapping) extends Mapping
  case class Col(targetType: TypeInfo, mapping: Mapping) extends Mapping
  case class HCol(targetType: TypeInfo, mappings: List[Mapping]) extends Mapping
  case class Constructor(targetType: TypeInfo, choices: List[DeclaredConstructor]) extends Mapping {
    def bestMatching(argNames: List[String]): Option[DeclaredConstructor] = {
      val names = Set(argNames: _*)
      def countOptionals(args: List[Arg]) =
        args.foldLeft(0)((n, x) => if (x.optional) n+1 else n)
      def score(args: List[Arg]) =
        args.foldLeft(0)((s, arg) => if (names.contains(arg.path)) s+1 else -100)

      if (choices.isEmpty) None
      else {
        val best = choices.tail.foldLeft((choices.head, score(choices.head.args))) { (best, c) =>
          val newScore = score(c.args)
          if (newScore == best._2) {
            if (countOptionals(c.args) < countOptionals(best._1.args))
              (c, newScore) else best
          } else if (newScore > best._2) (c, newScore) else best
        }
        Some(best._1)
      }
    }
  }

  case class DeclaredConstructor(constructor: JConstructor[_], args: List[Arg])

  // Current constructor parsing context. (containingClass + allArgs could be replaced with Constructor)
  case class Context(argName: String, containingClass: Class[_], allArgs: List[(String, Type)])

  private val mappings = new Memo[(Type, Seq[Class[_]]), Mapping]
  private val unmangledNames = new Memo[String, String]
  private val paranamer = new CachingParanamer(new BytecodeReadingParanamer)

  object ParanamerReader extends ParameterNameReader {
    def lookupParameterNames(constructor: JConstructor[_]): Iterable[String] =
      paranamer.lookupParameterNames(constructor)
  }

  private[json] def mappingOf(clazz: Type, typeArgs: Seq[Class[_]] = Seq())
                             (implicit formats: Formats): Mapping = {
    import Reflection._

    def constructors(t: Type, visited: Set[Type], context: Option[Context]): List[DeclaredConstructor] = {
      Reflection.constructors(t, formats.parameterNameReader, context).map { case (c, args) =>
        DeclaredConstructor(c, args.map { case (name, t) =>
          toArg(unmangleName(name), t, visited, Context(name, c.getDeclaringClass, args)) })
      }
    }

    def toArg(name: String, genericType: Type, visited: Set[Type], context: Context): Arg = {
      def mkContainer(t: Type, k: Kind, valueTypeIndex: Int, factory: Mapping => Mapping) = {
        if (typeConstructor_?(t)) {
          val typeArgs = typeConstructors(t, k)(valueTypeIndex)
          factory(fieldMapping(typeArgs)._1)
        } else {
          factory(fieldMapping(typeParameters(t, k, context)(valueTypeIndex))._1)
        }
      }

      def mkHeteroContainer(baseType: Type): Mapping = {
        val heteroContainerTypes = baseType match {
          case ptype: ParameterizedType =>
            ptype.getActualTypeArguments().map {
              case c: Class[_] =>
                c
              case p: ParameterizedType =>
                p.getRawType.asInstanceOf[Class[_]]
              case x =>
                fail("do not know how to get type parameter from " + x)
            }
        }

        val parameters = heteroContainerTypes.map(fieldMapping(_)._1)
        HCol(TypeInfo(rawClassOf(baseType), parameterizedTypeOpt(baseType)), parameters.toList)
      }

      def parameterizedTypeOpt(t: Type) = t match {
        case x: ParameterizedType =>
          val typeArgs = x.getActualTypeArguments.toList.zipWithIndex
            .map { case (t, idx) =>
              if (t == classOf[java.lang.Object])
                ScalaSigReader.readConstructor(context.argName, context.containingClass, idx, context.allArgs.map(_._1))
              else t
            }
          Some(mkParameterizedType(x.getRawType, typeArgs))
        case _ => None
      }

      def mkConstructor(t: Type) =
        if (visited.contains(t)) (Cycle(t), false)
        else (Constructor(TypeInfo(rawClassOf(t), parameterizedTypeOpt(t)), constructors(t, visited + t, Some(context))), false)

      def fieldMapping(t: Type): (Mapping, Boolean) = {
        t match {
          case pType: ParameterizedType =>
            val raw = rawClassOf(pType)
            val info = TypeInfo(raw, Some(pType))

            if (classOf[Set[_]].isAssignableFrom(raw))
              (mkContainer(t, `* -> *`, 0, Col.apply(info, _)), false)
            else if (raw.isArray)
              (mkContainer(t, `* -> *`, 0, Col.apply(info, _)), false)
            else if (classOf[Option[_]].isAssignableFrom(raw))
              (mkContainer(t, `* -> *`, 0, identity _), true)
            else if (classOf[Map[_, _]].isAssignableFrom(raw))
              (mkContainer(t, `(*,*) -> *`, 1, Dict.apply _), false)
            else if (classOf[Seq[_]].isAssignableFrom(raw))
              (mkContainer(t, `* -> *`, 0, Col.apply(info, _)), false)
            else if (tuples.find(_.isAssignableFrom(raw)).isDefined && formats.tuplesAsArrays)
              (mkHeteroContainer(t), false)
            else
              mkConstructor(t)
          case aType: GenericArrayType =>
            // Couldn't find better way to reconstruct proper array type:
            val raw = java.lang.reflect.Array.newInstance(rawClassOf(aType.getGenericComponentType), 0: Int).getClass
            (Col(TypeInfo(raw, None), fieldMapping(aType.getGenericComponentType)._1), false)
          case raw: Class[_] =>
            if (primitive_?(raw)) (Value(raw), false)
            else if (raw.isArray)
              (mkContainer(t, `* -> *`, 0, Col.apply(TypeInfo(raw, None), _)), false)
            else
              mkConstructor(t)
          case x => (Constructor(TypeInfo(classOf[AnyRef], None), Nil), false)
        }
      }

      val (mapping, optional) = fieldMapping(genericType)
      Arg(name, mapping, optional)
    }

    if (primitive_?(clazz)) {
      Value(rawClassOf(clazz))
    } else {
      mappings.memoize((clazz, typeArgs), { case (t, _) =>
        val c = rawClassOf(t)
        val (pt, typeInfo) =
          if (typeArgs.isEmpty) {
            (t, TypeInfo(c, None))
          } else {
            val t = mkParameterizedType(c, typeArgs)
            (t, TypeInfo(c, Some(t)))
          }

        Constructor(typeInfo, constructors(pt, Set(), None))
      })
    }
  }

  private[json] def rawClassOf(t: Type): Class[_] = t match {
    case c: Class[_] => c
    case p: ParameterizedType => rawClassOf(p.getRawType)
    case x => fail("Raw type of " + x + " not known")
  }

  private[json] def mkParameterizedType(owner: Type, typeArgs: Seq[Type]) =
    new ParameterizedType {
      def getActualTypeArguments = typeArgs.toArray
      def getOwnerType = owner
      def getRawType = owner
      override def toString = s"${getOwnerType}[${getActualTypeArguments.mkString(",")}]"
    }

  private[json] def unmangleName(name: String) =
    unmangledNames.memoize(name, scala.reflect.NameTransformer.decode)

  private[json] def fail(msg: String, cause: Exception = null) = throw new MappingException(msg, cause)

  private class Memo[A, R] {
    private val cache = new java.util.concurrent.atomic.AtomicReference(Map[A, R]())

    def memoize(x: A, f: A => R): R = {
      val c = cache.get
      def addToCache() = {
        val ret = f(x)
        cache.set(c + (x -> ret))
        ret
      }
      c.getOrElse(x, addToCache())
    }
  }

  object Reflection {
    import java.lang.reflect._

    sealed abstract class Kind
    case object `* -> *` extends Kind
    case object `(*,*) -> *` extends Kind

    val primitives = Map[Class[_], Unit]() ++ (List[Class[_]](
      classOf[String], classOf[Int], classOf[Long], classOf[Double],
      classOf[Float], classOf[Byte], classOf[BigInt], classOf[Boolean],
      classOf[Short], classOf[java.lang.Integer], classOf[java.lang.Long],
      classOf[java.lang.Double], classOf[java.lang.Float],
      classOf[java.lang.Byte], classOf[java.lang.Boolean], classOf[Number],
      classOf[java.lang.Short], classOf[Date], classOf[Timestamp], classOf[Symbol], classOf[JValue],
      classOf[JObject], classOf[JArray]).map((_, ())))

    val tuples = Seq(
      classOf[Tuple1[_]], classOf[Tuple2[_,_]], classOf[Tuple3[_,_,_]], classOf[Tuple4[_,_,_,_]],
      classOf[Tuple5[_,_,_,_,_]], classOf[Tuple6[_,_,_,_,_,_]],
      classOf[Tuple7[_,_,_,_,_,_,_]], classOf[Tuple8[_,_,_,_,_,_,_,_]],
      classOf[Tuple9[_,_,_,_,_,_,_,_,_]], classOf[Tuple10[_,_,_,_,_,_,_,_,_,_]],
      classOf[Tuple11[_,_,_,_,_,_,_,_,_,_,_]], classOf[Tuple12[_,_,_,_,_,_,_,_,_,_,_,_]],
      classOf[Tuple13[_,_,_,_,_,_,_,_,_,_,_,_,_]], classOf[Tuple14[_,_,_,_,_,_,_,_,_,_,_,_,_,_]],
      classOf[Tuple15[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]], classOf[Tuple16[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]],
      classOf[Tuple17[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]],
      classOf[Tuple18[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]],
      classOf[Tuple19[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]],
      classOf[Tuple20[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]],
      classOf[Tuple21[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]],
      classOf[Tuple22[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
    )

    val tupleConstructors: Map[Int, JConstructor[_]] = tuples.zipWithIndex.map({
      case (tupleClass, index) =>
        index -> tupleClass.getConstructors()(0)
    }).toMap

    private val primaryConstructorArgumentsMemo = new Memo[Class[_], List[(String, Type)]]
    private val declaredFieldsMemo = new Memo[Class[_], Map[String,Field]]

    def constructors(t: Type, names: ParameterNameReader, context: Option[Context]): List[(JConstructor[_], List[(String, Type)])] =
      rawClassOf(t).getDeclaredConstructors.map(c => (c, constructorArgs(t, c, names, context))).toList

    def constructorArgs(t: Type, constructor: JConstructor[_],
                        nameReader: ParameterNameReader, context: Option[Context]): List[(String, Type)] = {
      def argsInfo(c: JConstructor[_], typeArgs: Map[TypeVariable[_], Type]) = {
        val Name = """^((?:[^$]|[$][^0-9]+)+)([$][0-9]+)?$""".r
        def clean(name: String) = name match {
          case Name(text, junk) => text
        }
        try {
          val names = nameReader.lookupParameterNames(c).map(clean)
          val types = c.getGenericParameterTypes.toList.zipWithIndex map {
            case (v: TypeVariable[_], idx) =>
              val arg = typeArgs.getOrElse(v, v)
              if (arg == classOf[java.lang.Object])
                context.map(ctx => ScalaSigReader.readConstructor(ctx.argName, ctx.containingClass, idx, ctx.allArgs.map(_._1))).getOrElse(arg)
              else arg
            case (x, _) => x
          }
          names.toList.zip(types)
        } catch {
          case e: ParameterNamesNotFoundException => Nil
        }
      }

      t match {
        case c: Class[_] => argsInfo(constructor, Map())
        case p: ParameterizedType =>
          val vars =
            Map() ++ rawClassOf(p).getTypeParameters.toList.map(_.asInstanceOf[TypeVariable[_]]).zip(p.getActualTypeArguments.toList) // FIXME this cast should not be needed
          argsInfo(constructor, vars)
        case x => fail("Do not know how query constructor info for " + x)
      }
    }

    def primaryConstructorArgs(c: Class[_])(implicit formats: Formats) = {
      def findMostComprehensive(c: Class[_]): List[(String, Type)] = {
        val ord = Ordering[Int].on[JConstructor[_]](_.getParameterTypes.size)
        val primary = c.getDeclaredConstructors.max(ord)
        constructorArgs(c, primary, formats.parameterNameReader, None)
      }

      primaryConstructorArgumentsMemo.memoize(c, findMostComprehensive(_))
    }

    def typeParameters(t: Type, k: Kind, context: Context): List[Class[_]] = {
      def term(i: Int) = t match {
        case ptype: ParameterizedType => ptype.getActualTypeArguments()(i) match {
          case c: Class[_] =>
            if (c == classOf[java.lang.Object])
              ScalaSigReader.readConstructor(context.argName, context.containingClass, i, context.allArgs.map(_._1))
            else c
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

    def typeConstructors(t: Type, k: Kind): List[Type] = {
      def types(i: Int): Type = {
        val ptype = t.asInstanceOf[ParameterizedType]
        ptype.getActualTypeArguments()(i) match {
          case p: ParameterizedType => p
          case c: Class[_] => c
        }
      }

      k match {
        case `* -> *`     => List(types(0))
        case `(*,*) -> *` => List(types(0), types(1))
      }
    }

    def primitive_?(t: Type) = t match {
      case clazz: Class[_] => primitives contains clazz
      case _ => false
    }

    def tuple_?(t: Type) = t match {
      case clazz: Class[_] =>
        tuples contains clazz
      case _ =>
        false
    }

    def static_?(f: Field) = Modifier.isStatic(f.getModifiers)
    def typeConstructor_?(t: Type) = t match {
      case p: ParameterizedType =>
        p.getActualTypeArguments.exists(_.isInstanceOf[ParameterizedType])
      case _ => false
    }

    def array_?(x: Any) = x != null && classOf[scala.Array[_]].isAssignableFrom(x.asInstanceOf[AnyRef].getClass)

    def fields(clazz: Class[_]): List[(String, TypeInfo)] = {
      val fs = clazz.getDeclaredFields.toList
        .filterNot(f => Modifier.isStatic(f.getModifiers) || Modifier.isTransient(f.getModifiers))
        .map(f => (f.getName, TypeInfo(f.getType, f.getGenericType match {
          case p: ParameterizedType => Some(p)
          case _ => None
        })))
      fs ::: (if (clazz.getSuperclass == null) Nil else fields(clazz.getSuperclass))
    }

    def setField(a: AnyRef, name: String, value: Any) = {
      val f = findField(a.getClass, name)
      f.setAccessible(true)
      f.set(a, value)
    }

    def getField(a: AnyRef, name: String) = {
      val f = findField(a.getClass, name)
      f.setAccessible(true)
      f.get(a)
    }

    def findField(clazz: Class[_], name: String): Field = try {
      clazz.getDeclaredField(name)
    } catch {
      case e: NoSuchFieldException =>
        if (clazz.getSuperclass == null) throw e
        else findField(clazz.getSuperclass, name)
    }

    def getDeclaredFields(clazz: Class[_]) : Map[String,Field] = {
      def extractDeclaredFields = clazz.getDeclaredFields.map(field => (field.getName, field)).toMap
      declaredFieldsMemo.memoize(clazz, _ => extractDeclaredFields)
    }

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
      case _ => sys.error("not a primitive " + a.asInstanceOf[AnyRef].getClass)
    }
  }
}

case class MappingException(msg: String, cause: Exception) extends Exception(msg, cause) {
  def this(msg: String) = this(msg, null)
}
