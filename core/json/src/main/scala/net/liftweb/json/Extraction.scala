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

import java.lang.reflect.{Constructor => JConstructor, Type, InvocationTargetException}
import java.lang.{Integer => JavaInteger, Long => JavaLong, Short => JavaShort, Byte => JavaByte, Boolean => JavaBoolean, Double => JavaDouble, Float => JavaFloat}
import java.util.Date
import java.sql.Timestamp
import scala.reflect.Manifest

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
  def extract[A](json: JValue)(implicit formats: Formats, mf: Manifest[A]): A = {
    def allTypes(mf: Manifest[_]): List[Class[_]] = mf.runtimeClass :: (mf.typeArguments flatMap allTypes)

    try {
      val types = allTypes(mf)
      extract0(json, types.head, types.tail).asInstanceOf[A]
    } catch {
      case e: MappingException => throw e
      case e: Exception => throw new MappingException("unknown error", e)
    }
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
    def prependTypeHint(clazz: Class[_], o: JObject) =
      JObject(JField(formats.typeHintFieldName, JString(formats.typeHints.hintFor(clazz))) :: o.obj)

    def mkObject(clazz: Class[_], fields: List[JField]) = formats.typeHints.containsHint_?(clazz) match {
      case true  => prependTypeHint(clazz, JObject(fields))
      case false => JObject(fields)
    }

    val serializer = formats.typeHints.serialize
    val any = a.asInstanceOf[AnyRef]
    if (formats.customSerializer(formats).isDefinedAt(a)) {
      formats.customSerializer(formats)(a)
    } else if (!serializer.isDefinedAt(a)) {
      any match {
        case null => JNull
        case x: JValue => x
        case x if primitive_?(x.getClass) => primitive2jvalue(x)(formats)
        case x: Map[_, _] => JObject((x map { case (k: String, v) => JField(k, decompose(v)) }).toList)
        case x: Iterable[_] => JArray(x.toList map decompose)
        case x if (x.getClass.isArray) => JArray(x.asInstanceOf[Array[_]].toList map decompose)
        case x: Option[_] => x.flatMap[JValue] { y => Some(decompose(y)) }.getOrElse(JNothing)
        case x: Product if formats.tuplesAsArrays && tuple_?(x.getClass) =>
          JArray(x.productIterator.toList.map(decompose))
        case x =>
          val fields = getDeclaredFields(x.getClass)
          val constructorArgs = primaryConstructorArgs(x.getClass).map{ case (name, _) => (name,fields.get(name)) }
          constructorArgs.collect { case (name, Some(f)) =>
            f.setAccessible(true)
            JField(unmangleName(name), decompose(f get x))
          } match {
            case args =>
              val fields = formats.fieldSerializer(x.getClass).map { serializer =>
                Reflection.fields(x.getClass).map {
                  case (mangledName, _) =>
                    val n = Meta.unmangleName(mangledName)
                    val fieldVal = Reflection.getField(x, mangledName)
                    val s = serializer.serializer orElse Map((n, fieldVal) -> Some(n, fieldVal))
                    s((n, fieldVal)).map { case (name, value) => JField(name, decompose(value)) }
                      .getOrElse(JField(n, JNothing))
                }
              } getOrElse Nil
              val uniqueFields = fields filterNot (f => args.find(_.name == f.name).isDefined)
              mkObject(x.getClass, uniqueFields ++ args)
          }
      }
    } else prependTypeHint(any.getClass, serializer(any))
  }

  /** Flattens the JSON to a key/value map.
   */
  def flatten(json: JValue): Map[String, String] = {
    def escapePath(str: String) = str

    def flatten0(path: String, json: JValue): Map[String, String] = {
      json match {
        case JNothing | JNull    => Map()
        case JString(s)          => Map(path -> ("\"" + JsonAST.quote(s) + "\""))
        case JDouble(num)        => Map(path -> num.toString)
        case JInt(num)           => Map(path -> num.toString)
        case JBool(value)        => Map(path -> value.toString)
        case JObject(obj)        => obj.foldLeft(Map[String, String]()) { case (map, JField(name, value)) =>
          map ++ flatten0(path + "." + escapePath(name), value)
        }
        case JArray(arr)         => arr.length match {
          case 0 => Map(path -> "[]")
          case _ => arr.foldLeft((Map[String, String](), 0)) {
                      (tuple, value) => (tuple._1 ++ flatten0(path + "[" + tuple._2 + "]", value), tuple._2 + 1)
                    }._1
        }
      }
    }

    flatten0("", json)
  }

  /** Unflattens a key/value map to a JSON object.
   */
  def unflatten(map: Map[String, String]): JValue = {
    import scala.util.matching.Regex

    def extractValue(value: String): JValue = value.toLowerCase match {
      case ""      => JNothing
      case "null"  => JNull
      case "true"  => JBool(true)
      case "false" => JBool(false)
      case "[]"    => JArray(Nil)
      case x @ _   =>
        if (value.charAt(0).isDigit) {
          if (value.indexOf('.') == -1) JInt(BigInt(value))
          else JDouble(JsonParser.parseDouble(value))
        }
        else JString(JsonParser.unquote(value.substring(1)))
    }

    def submap(prefix: String): Map[String, String] =
      Map(
        map.filter(t => t._1 == prefix || t._1.startsWith(prefix + ".") || t._1.startsWith(prefix + "[")).map(
          t => (t._1.substring(prefix.length), t._2)
        ).toList: _*
      )

    val ArrayProp = new Regex("""^(\.([^\.\[]+))\[(\d+)\].*$""")
    val ArrayElem = new Regex("""^(\[(\d+)\]).*$""")
    val OtherProp = new Regex("""^(\.([^\.\[]+)).*$""")

    val uniquePaths = map.keys.foldLeft[Set[String]](Set()) {
      (set, key) =>
        key match {
          case ArrayProp(p, f, i) => set + p
          case OtherProp(p, f)    => set + p
          case ArrayElem(p, i)    => set + p
          case x @ _              => set + x
        }
    }.toList.sortWith(_ < _) // Sort is necessary to get array order right

    uniquePaths.foldLeft[JValue](JNothing) { (jvalue, key) =>
      jvalue.merge(key match {
        case ArrayProp(p, f, i) => JObject(List(JField(f, unflatten(submap(key)))))
        case ArrayElem(p, i)    => JArray(List(unflatten(submap(key))))
        case OtherProp(p, f)    => JObject(List(JField(f, unflatten(submap(key)))))
        case ""                 => extractValue(map(key))
      })
    }
  }

  private[this] def mkMapping(clazz: Class[_], typeArgs: Seq[Class[_]])(implicit formats: Formats): Meta.Mapping = {
    if (clazz == classOf[Option[_]] || clazz == classOf[List[_]] || clazz == classOf[Set[_]] || clazz.isArray) {
      Col(TypeInfo(clazz, None), mkMapping(typeArgs.head, typeArgs.tail))
    } else if (clazz == classOf[Map[_, _]]) {
      Dict(mkMapping(typeArgs.tail.head, typeArgs.tail.tail))
    } else if (formats.tuplesAsArrays && tuple_?(clazz)) {
      val childMappings = typeArgs.map(c => mkMapping(c, Nil)).toList
      HCol(TypeInfo(clazz, None), childMappings)
    } else {
      mappingOf(clazz, typeArgs)
    }
  }

  private def extract0(json: JValue, clazz: Class[_], typeArgs: Seq[Class[_]])
                      (implicit formats: Formats): Any = {
    val mapping = mkMapping(clazz, typeArgs)
    extract0(json, mapping)
  }

  def extract(json: JValue, target: TypeInfo)(implicit formats: Formats): Any =
    extract0(json, mappingOf(target.clazz))

  private def extract0(json: JValue, mapping: Mapping)(implicit formats: Formats): Any = {
    def newInstance(constructor: Constructor, json: JValue) = {
      def findBestConstructor = {
        if (constructor.choices.size == 1) {
          constructor.choices.head // optimized common case
        } else {
          val argNames = json match {
            case JObject(fs) => fs.map(_.name)
            case x => Nil
          }
          constructor.bestMatching(argNames)
            .getOrElse(fail("No constructor for type " + constructor.targetType.clazz + ", " + json))
        }
      }

      def setFields(a: AnyRef, json: JValue, constructor: JConstructor[_]) = json match {
        case o: JObject =>
          formats.fieldSerializer(a.getClass).map { serializer =>
            val constructorArgNames =
              Reflection.constructorArgs(a.getClass, constructor, formats.parameterNameReader, None).map(_._1).toSet
            val jsonFields = o.obj.map { f =>
              val JField(n, v) = (serializer.deserializer orElse Map(f -> f))(f)
              (n, (n, v))
            }.toMap

            val fieldsToSet =
              Reflection.fields(a.getClass).filterNot(f => constructorArgNames.contains(f._1))

            fieldsToSet.foreach { case (name, typeInfo) =>
              jsonFields.get(name).foreach { case (n, v) =>
                val typeArgs = typeInfo.parameterizedType
                  .map(_.getActualTypeArguments.map(_.asInstanceOf[Class[_]]).toList.zipWithIndex
                    .map { case (t, idx) =>
                      if (t == classOf[java.lang.Object]) ScalaSigReader.readField(name, a.getClass, idx)
                      else t
                    })
                val value = extract0(v, typeInfo.clazz, typeArgs.getOrElse(Nil))
                Reflection.setField(a, n, value)
              }
            }
          }
          a
        case _ => a
      }

      def instantiate = {
        val c = findBestConstructor
        val jconstructor = c.constructor
        val args = c.args.map(a => build(json \ a.path, a))
        try {
          if (jconstructor.getDeclaringClass == classOf[java.lang.Object])
            fail("No information known about type")

          val instance = jconstructor.newInstance(args.map(_.asInstanceOf[AnyRef]).toArray: _*)
          setFields(instance.asInstanceOf[AnyRef], json, jconstructor)
        } catch {
          case exception: Exception =>
            exception match {
              case matchedException @ (_:IllegalArgumentException | _:InstantiationException) =>
                fail("Parsed JSON values do not match with class constructor\nargs=" +
                     args.mkString(",") + "\narg types=" + args.map(a => if (a != null)
                       a.asInstanceOf[AnyRef].getClass.getName else "null").mkString(",") +
                     "\nconstructor=" + jconstructor, matchedException)

              case exceptionThrownInConstructor: InvocationTargetException =>
                fail("An exception was thrown in the class constructor during extraction", exceptionThrownInConstructor)

              case unmatchedException =>
                throw unmatchedException
            }
        }
      }

      def mkWithTypeHint(typeHint: String, fields: List[JField], typeInfo: TypeInfo) = {
        val obj = JObject(fields filterNot (_.name == formats.typeHintFieldName))
        val deserializer = formats.typeHints.deserialize
        if (!deserializer.isDefinedAt(typeHint, obj)) {
          val concreteClass = formats.typeHints.classFor(typeHint) getOrElse fail("Do not know how to deserialize '" + typeHint + "'")
          val typeArgs = typeInfo.parameterizedType
            .map(_.getActualTypeArguments.toList.map(Meta.rawClassOf)).getOrElse(Nil)
          build(obj, mappingOf(concreteClass, typeArgs))
        } else deserializer(typeHint, obj)
      }

      val custom = formats.customDeserializer(formats)
      if (custom.isDefinedAt(constructor.targetType, json)) {
        custom(constructor.targetType, json)
      } else {
        json match {
          case JNull =>
            null

          case JObject(TypeHint(t, fs)) =>
            mkWithTypeHint(t, fs, constructor.targetType)

          case _ =>
            instantiate
        }
      }
    }

    object TypeHint {
      def unapply(fs: List[JField]): Option[(String, List[JField])] =
        if (formats.typeHints == NoTypeHints) None
        else {
          val grouped = fs groupBy (_.name == formats.typeHintFieldName)
          if (grouped.isDefinedAt(true))
            Some((grouped(true).head.value.values.toString, grouped.get(false).getOrElse(Nil)))
          else None
        }
    }

    def newPrimitive(elementType: Class[_], elem: JValue) = convert(elem, elementType, formats)

    def newCollection(root: JValue, m: Mapping, constructor: Array[_] => Any) = {
      val array: Array[_] = root match {
        case JArray(arr)      => arr.map(build(_, m)).toArray
        case JNothing | JNull => Array[AnyRef]()
        case x                => fail("Expected collection but got " + x + " for root " + root + " and mapping " + m)
      }

      constructor(array)
    }

    def newOption(root: JValue, m: Mapping) = {
      root match {
        case JNothing | JNull => None
        case x => Option(build(x, m))
      }
    }

    def newTuple(root: JValue, mappings: List[Mapping]): Any = {
      root match {
        case JArray(items) if items.nonEmpty && items.length <= tuples.length =>
          val builtItems: Seq[Object] = items.zip(mappings).map({
            case (item, mapping) =>
              build(item, mapping).asInstanceOf[Object]
          })
          val tupleIndex = items.length - 1

          val typedTupleConstructor = tupleConstructors.get(tupleIndex).getOrElse {
            throw new IllegalArgumentException(s"Cannot instantiate a tuple of length ${items.length} even though that should be a valid tuple length.")
          }
          typedTupleConstructor.newInstance(builtItems: _*)

        case JArray(items) =>
          throw new IllegalArgumentException("Cannot create a tuple of length " + items.length)

        case JObject(items) if items.forall(_.name.startsWith("_")) =>
          val sortedItems = items.sortWith { (i1, i2) =>
            val numerialName1 = i1.name.drop(1).toInt
            val numerialName2 = i2.name.drop(1).toInt

            numerialName1 < numerialName2
          }
          newTuple(JArray(sortedItems.map(_.value)), mappings)

        case x =>
          throw new IllegalArgumentException("Got unexpected while attempting to create tuples: " + x)
      }
    }

    def build(root: JValue, mapping: Mapping): Any = mapping match {
      case Value(targetType) =>
        convert(root, targetType, formats)

      case c: Constructor =>
        newInstance(c, root)

      case Cycle(targetType) =>
        build(root, mappingOf(targetType))

      case Arg(path, m, optional) =>
        mkValue(root, m, path, optional)

      case HCol(targetType, mappings) if formats.tuplesAsArrays =>
        val c = targetType.clazz
        if (tuples.find(_.isAssignableFrom(c)).isDefined) {
          newTuple(root, mappings)
        } else {
          fail("Expected tuple but found " + mappings)
        }

      case Col(targetType, m) =>
        val custom = formats.customDeserializer(formats)
        val c = targetType.clazz

        if (custom.isDefinedAt(targetType, root)) custom(targetType, root)
        else if (c == classOf[List[_]]) newCollection(root, m, a => List(a.toSeq: _*))
        else if (c == classOf[Set[_]]) newCollection(root, m, a => Set(a.toSeq: _*))
        else if (c.isArray) newCollection(root, m, mkTypedArray(c))
        else if (classOf[Seq[_]].isAssignableFrom(c)) newCollection(root, m, a => List(a.toSeq: _*))
        else if (c == classOf[Option[_]]) newOption(root, m)
        else fail("Expected collection but got " + m + " for class " + c)
      case Dict(m) => root match {
        case JObject(xs) => Map(xs.map(x => (x.name, build(x.value, m))): _*)
        case x => fail("Expected object but got " + x)
      }
    }

    def mkTypedArray(c: Class[_])(a: Array[_]) = {
      import java.lang.reflect.Array.{newInstance => newArray}

      a.foldLeft((newArray(c.getComponentType, a.length), 0)) { (tuple, e) => {
        java.lang.reflect.Array.set(tuple._1, tuple._2, e); (tuple._1, tuple._2 + 1)
      }}._1
    }

    def mkList(root: JValue, m: Mapping) = root match {
      case JArray(arr) => arr.map(build(_, m))
      case JNothing | JNull => Nil
      case x => fail("Expected array but got " + x)
    }

    def mkValue(root: JValue, mapping: Mapping, path: String, optional: Boolean) = {
      if (optional && root == JNothing) {
        None
      } else {
        try {
          val x = build(root, mapping)
          if (optional) Option(x) else x
        } catch {
          case e @ MappingException(msg, _) =>
            if (optional && (root == JNothing || root == JNull)) {
              None
            } else {
              fail("No usable value for " + path + "\n" + msg, e)
            }
        }
      }
    }

    build(json, mapping)
  }

  private def convert(json: JValue, targetType: Class[_], formats: Formats): Any = json match {
    case JInt(x) if (targetType == classOf[Int]) => x.intValue
    case JInt(x) if (targetType == classOf[JavaInteger]) => JavaInteger.valueOf(x.intValue)
    case JInt(x) if (targetType == classOf[BigInt]) => x
    case JInt(x) if (targetType == classOf[Long]) => x.longValue
    case JInt(x) if (targetType == classOf[JavaLong]) => JavaLong.valueOf(x.longValue)
    case JInt(x) if (targetType == classOf[Double]) => x.doubleValue
    case JInt(x) if (targetType == classOf[JavaDouble]) => JavaDouble.valueOf(x.doubleValue)
    case JInt(x) if (targetType == classOf[Float]) => x.floatValue
    case JInt(x) if (targetType == classOf[JavaFloat]) => JavaFloat.valueOf(x.floatValue)
    case JInt(x) if (targetType == classOf[Short]) => x.shortValue
    case JInt(x) if (targetType == classOf[JavaShort]) => JavaShort.valueOf(x.shortValue)
    case JInt(x) if (targetType == classOf[Byte]) => x.byteValue
    case JInt(x) if (targetType == classOf[JavaByte]) => JavaByte.valueOf(x.byteValue)
    case JInt(x) if (targetType == classOf[String]) => x.toString
    case JInt(x) if (targetType == classOf[Number]) => x.longValue
    case JDouble(x) if (targetType == classOf[Double]) => x
    case JDouble(x) if (targetType == classOf[JavaDouble]) => JavaDouble.valueOf(x)
    case JDouble(x) if (targetType == classOf[Float]) => x.floatValue
    case JDouble(x) if (targetType == classOf[JavaFloat]) => JavaFloat.valueOf(x.floatValue)
    case JDouble(x) if (targetType == classOf[String]) => x.toString
    case JDouble(x) if (targetType == classOf[Int]) => x.intValue
    case JDouble(x) if (targetType == classOf[Long]) => x.longValue
    case JDouble(x) if (targetType == classOf[Number]) => x
    case JString(s) if (targetType == classOf[String]) => s
    case JString(s) if (targetType == classOf[Symbol]) => Symbol(s)
    case JString(s) if (targetType == classOf[Date]) => formats.dateFormat.parse(s).getOrElse(fail("Invalid date '" + s + "'"))
    case JString(s) if (targetType == classOf[Timestamp]) => new Timestamp(formats.dateFormat.parse(s).getOrElse(fail("Invalid date '" + s + "'")).getTime)
    case JBool(x) if (targetType == classOf[Boolean]) => x
    case JBool(x) if (targetType == classOf[JavaBoolean]) => JavaBoolean.valueOf(x)
    case j: JValue if (targetType == classOf[JValue]) => j
    case j: JObject if (targetType == classOf[JObject]) => j
    case j: JArray if (targetType == classOf[JArray]) => j
    case JNull => null
    case JNothing =>
      fail("Did not find value which can be converted into " + targetType.getName)
    case _ =>
      val custom = formats.customDeserializer(formats)
      val typeInfo = TypeInfo(targetType, None)

      if (custom.isDefinedAt(typeInfo, json)) {
        custom(typeInfo, json)
      } else {
        fail("Do not know how to convert " + json + " into " + targetType)
      }
  }
}
