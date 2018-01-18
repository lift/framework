/*
 * Copyright 2009-2015 WorldWide Conferencing, LLC
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

import scala.language.implicitConversions
import java.io.Writer
import java.lang.StringBuilder

/**
 * This object contains the abstract syntax tree (or AST) for working with JSON objects in
 * lift-json.
 *
 * The purpose of the JSON AST is to represent and manipulate JSON by leveraging Scala language
 * features like types, case classes, etc. The AST should allow you to represent anything you
 * could imagine from JSON land using the Scala type system.
 *
 * Everything in the AST has a single root: JValue. A JValue could, quite literally, be anything.
 * It could be an an object (represented by `[[JObject]]`), a string (`[[JString]]`), a null
 * (`[[JNull]]`), and so on. So, when constructing a JSON object with the AST directly you might
 * construct something like the following:
 *
 * {{{
 * JObject(JField("bacon", JBool(true)) :: JField("spinach", JBool(false)))
 * }}}
 *
 * Once serialized to the string representation of JSON you would end up with the following:
 *
 * {{{
 * {
 *   "bacon":true,
 *   "spinach":false
 * }
 * }}}
 */
object JsonAST {
  /**
    * Concatenate a sequence of `[[JValue]]`s together.
    *
    * This would be useful in the event that you have a handful of `JValue` instances that need to be
    * smacked together into one unit.
    *
    * For example:
    *
    * {{{
    * concat(JInt(1), JInt(2)) == JArray(List(JInt(1), JInt(2)))
    * }}}
    *
    */
  def concat(values: JValue*) = values.foldLeft(JNothing: JValue)(_ ++ _)

  object JValue extends Merge.Mergeable

  /**
   * The base type for all things that represent distinct JSON entities in the AST.
   *
   * Most members of the AST will extend this class. The one exception is `[[JField]]` which does
   * not extend this class because it really ''can't'' properly exist as a
   * first-class citizen of JSON.
   */
  sealed abstract class JValue extends Diff.Diffable {
    type Values

    /**
     * An XPath-like expression to find a child of a `[[JObject]]` or a `[[JArray]]` of `JObject`
     * by name. If you call this method on anything other than a `JObject` or `JArray` of `JObject`s
     * you'll get a `[[JNothing]]`.
     *
     * This method is most useful if you have an object that you need to dig into in order to
     * retrieve a specific value. So, let's say that you had a JSON object that looked
     * something like this:
     *
     * {{{
     * {
     *   "name": "Joe",
     *   "profession": "Software Engineer",
     *   "catchphrase": {
     *     "name": "Alabama Cheer",
     *     "value": "Roll tide"
     *   }
     * }
     * }}}
     *
     * If for some reason you're interested in taking a look at Joe's catchphrase, you can
     * query it using the `\` method to find it like so:
     *
     * Example:
     *
     * {{{
     * scala> json \ "catchphrase"
     * res0: JValue = JObject(List(JField("name", JString("Alabama Cheer")), JField("value", JString("Roll tide"))))
     * }}}
     *
     * Likewise, if you wanted to find Joe's name you could do the following:
     *
     * {{{
     * scala> json \ "name"
     * res0: JValue = JString("Joe")
     * }}}
     *
     * The result could be any subclass of `JValue`.
     * In the event that the `JValue` you're operating on is actually an array of objects, you'll
     * get back a `JArray` of the result of executing `\` on each object in the array. In the event
     * nothing is found, you'll get a `JNothing`.
     */
    def \(nameToFind: String): JValue = {
      // Use :: instead of List() to avoid the extra array allocation for the variable arguments
      findDirectByName(this :: Nil, nameToFind) match {
        case Nil => JNothing
        case x :: Nil => x
        case x => JArray(x)
      }
    }

    private def findDirectByName(xs: List[JValue], name: String): List[JValue] = xs.flatMap {
      case JObject(l) =>
        l.collect {
          case JField(n, value) if n == name => value
        }
      case JArray(l) => findDirectByName(l, name)
      case _ => Nil
    }

    private def findDirect(xs: List[JValue], p: JValue => Boolean): List[JValue] = xs.flatMap {
      case JObject(l) =>
        l.collect {
          case JField(n, x) if p(x) => x
        }
      case JArray(l) => findDirect(l, p)
      case x if p(x) => x :: Nil
      case _ => Nil
    }

    /**
     * Find all children of a `[[JObject]]` with the matching name, returning an empty `JObject` if
     * no matches are found.
     *
     * For example given this example JSON:
     *
     * {{{
     * {
     *   "name": "Joe",
     *   "profession": "Software Engineer",
     *   "catchphrase": {
     *     "name": "Alabama Cheer",
     *     "value": "Roll tide"
     *   }
     * }
     * }}}
     *
     * We might do the following:
     *
     * {{{
     * scala> json \\ "name"
     * res2: JValue = JObject(List(JField(name,JString(Joe)), JField(name,JString(Alabama Cheer))))
     * }}}
     */
    def \\(nameToFind: String): JObject = {
      def find(json: JValue): List[JField] = json match {
        case JObject(fields) =>
          fields.foldLeft(List[JField]()) {
            case (matchingFields, JField(name, value)) =>
              matchingFields :::
                List(JField(name, value)).filter(_.name == nameToFind) :::
                find(value)
          }

        case JArray(fields) =>
          fields.foldLeft(List[JField]()) { (matchingFields, children) =>
            matchingFields ::: find(children)
          }

        case _ =>
          Nil
      }

      JObject(find(this))
    }

    /**
     * Find immediate children of this `[[JValue]]` that match a specific `JValue` subclass.
     *
     * This methid will search a `[[JObject]]` or `[[JArray]]` for values of a specific type and
     * return a `List` of those values if they any are found.
     *
     * So given some JSON like so:
     *
     * {{{
     *  [
     *    {
     *      "thinga":1,
     *      "thingb":"bacon"
     *    },{
     *      "thingc":3,
     *      "thingd":"Wakka"
     *    },{
     *      "thinge":{
     *        "thingf":4
     *      },
     *      "thingg":true
     *    }
     *  ]
     * }}}
     *
     * You would use this method like so:
     *
     * {{{
     * scala> json \ classOf[JInt]
     * res0: List[net.liftweb.json.JInt#Values] = List(1, 3)
     * }}}
     *
     * This method does require that whatever type you're searching for is subtype of `JValue`.
     */
    def \[A <: JValue](clazz: Class[A]): List[A#Values] =
      findDirect(children, typePredicate(clazz) _).asInstanceOf[List[A]] map { _.values }

    /**
     * Find all descendants of this `JValue` that match a specific `JValue` subclass.
     *
     * Unlike its cousin `\`, this method will recurse down into all children looking for
     * type matches searching a `[[JObject]]` or `[[JArray]]` for values of a specific type and
     * return a `List` of those values if they are found.
     *
     * So given some JSON like so:
     *
     * {{{
     *  [
     *    {
     *      "thinga":1,
     *      "thingb":"bacon"
     *    },{
     *      "thingc":3,
     *      "thingd":"Wakka"
     *    },{
     *      "thinge":{
     *        "thingf":4
     *      },
     *      "thingg":true
     *    }
     *  ]
     * }}}
     *
     * You would use this method like so:
     *
     * {{{
     * scala> json \\ classOf[JInt]
     * res0: List[net.liftweb.json.JInt#Values] = List(1, 3, 4)
     * }}}
     */
    def \\[A <: JValue](clazz: Class[A]): List[A#Values] =
      (this filter typePredicate(clazz) _).asInstanceOf[List[A]] map { _.values }

    private def typePredicate[A <: JValue](clazz: Class[A])(json: JValue) = json match {
      case x if x.getClass == clazz => true
      case _ => false
    }

    /**
     * Return the element in the `i`-th position from a `[[JArray]]`.
     * Will return `JNothing` when invoked on any other kind of `JValue`.
     *
     * For example:
     *
     * {{{
     * scala> val array = JArray(JInt(1) :: JInt(2) :: Nil)
     * array: net.liftweb.json.JsonAST.JArray = JArray(List(JInt(1), JInt(2)))
     *
     * scala> array(1)
     * res0: net.liftweb.json.JsonAST.JValue = JInt(2)
     * }}}
     */
    def apply(i: Int): JValue = JNothing

    /**
     * Return a representation of the values in this `[[JValue]]` in a native Scala structure.
     *
     * For example, you might invoke this on a `[[JObject]]` to have its fields returned
     * as a `Map`.
     *
     * {{{
     * scala> JObject(JField("name", JString("joe")) :: Nil).values
     * res0: scala.collection.immutable.Map[String,Any] = Map(name -> joe)
     * }}}
     */
    def values: Values

    /**
     * Return direct child elements of this `JValue`, if this `JValue` is a `[[JObject]]` or `[[JArray]]`.
     *
     * This method is useful for getting all the values of a `JObject` or `JArray` and will return them as a
     * `List[JValue]`. If the `JValue` you invoke this method on is not a `JObject` or `JArray` you will instead
     * get `Nil`.
     *
     * Example:
     *
     * {{{
     * > JArray(JInt(1) :: JInt(2) :: Nil).children
     * List(JInt(1), JInt(2))
     * }}}
     *
     * @return Direct children of this `JValue` if it is a `[[JObject]]` or
     * `[[JArray]]`, or `[[JNothing]]` otherwise.
     */
    def children: List[JValue] = this match {
      case JObject(l) => l map (_.value)
      case JArray(l) => l
      case _ => Nil
    }

    /**
     * Fold over `JValue`s by applying a function to each element.
     *
     * @param f The function to apply, which takes an accumulator and the next item as paramaters.
     * @param z The initial value for the fold.
     */
    def fold[A](z: A)(f: (A, JValue) => A): A = {
      def rec(acc: A, v: JValue) = {
        val newAcc = f(acc, v)
        v match {
          case JObject(l) =>
            l.foldLeft(newAcc) {
              case (a, JField(name, value)) => value.fold(a)(f)
            }
          case JArray(l) =>
            l.foldLeft(newAcc) { (a, e) =>
              e.fold(a)(f)
            }
          case _ => newAcc
        }
      }
      rec(z, this)
    }

    /**
     * Fold over a series of `JField`s applying a function to each one.
     *
     * @param z The initial value for the fold.
     * @param f The function to apply, which takes an accumulator as its first parameter
     *          and the next field as its second.
     */
    def foldField[A](z: A)(f: (A, JField) => A): A = {
      def rec(acc: A, v: JValue) = {
        v match {
          case JObject(l) => l.foldLeft(acc) {
            case (a, field@JField(name, value)) => value.foldField(f(a, field))(f)
          }
          case JArray(l) => l.foldLeft(acc)((a, e) => e.foldField(a)(f))
          case _ => acc
        }
      }
      rec(z, this)
    }

    /**
     * Return a new `JValue` resulting from applying the given function to each value, recursively.
     *
     * If this function is invoked on a `[[JObject]]`, it will iterate over the field values of that `JObject`.
     * If this function is invoked on a `[[JArray]]`, it will iterate over the values of that `JArray`.
     * If this function is invoked on any other kind of `JValue` it will simply pass that instance into the
     * function you have provided.
     *
     * Example:
     *
     * {{{
     * JArray(JInt(1) :: JInt(2) :: Nil) map {
     *   case JInt(x) => JInt(x+1)
     *   case x => x
     * }
     * }}}
     */
    def map(f: JValue => JValue): JValue = {
      def rec(v: JValue): JValue = v match {
        case JObject(l) => f(JObject(l.map { field => field.copy(value = rec(field.value)) }))
        case JArray(l) => f(JArray(l.map(rec)))
        case x => f(x)
      }
      rec(this)
    }

    /**
     * Return a new `JValue` resulting from applying the given function to each `[[JField]]` in a `[[JObject]]` or a
     * `[[JArray]]` of `JObject`, recursively.
     *
     * Example:
     *
     * {{{
     * JObject(("age", JInt(10)) :: Nil) map {
     *   case ("age", JInt(x)) => ("age", JInt(x+1))
     *   case x => x
     * }
     * }}}
     *
     * @see transformField
     */
    def mapField(f: JField => JField): JValue = {
      def rec(v: JValue): JValue = v match {
        case JObject(l) => JObject(l.map { field => f(field.copy(value = rec(field.value))) })
        case JArray(l) => JArray(l.map(rec))
        case x => x
      }
      rec(this)
    }

    /** Return a new `JValue` resulting from applying the given partial function `f``
     * to each field in JSON.
     *
     * Example:
     * {{{
     * JObject(("age", JInt(10)) :: Nil) transformField {
     *   case ("age", JInt(x)) => ("age", JInt(x+1))
     * }
     * }}}
     */
    def transformField(f: PartialFunction[JField, JField]): JValue = mapField { x =>
      if (f.isDefinedAt(x)) f(x) else x
    }

    /**
     * Return a new `JValue` resulting from applying the given partial function
     * to each value within this `JValue`.
     *
     * If this is a `JArray`, this means we will transform each value in the
     * array and return an updated array.
     *
     * If this is a `JObject`, this means we will transform the value of each
     * field of the object and the object in turn and return an updated object.
     *
     * If this is another type of `JValue`, the value is transformed directly.
     *
     * Note that this happens recursively, so you will receive both each value
     * in an array ''and'' the array itself, or each field value in an object
     * ''and'' the object itself. If an array contains arrays, we will recurse
     * into them in turn.
     *
     * Examples:
     *
     * {{{
     * > JArray(JInt(1) :: JInt(2) :: Nil) transform {
     *   case JInt(x) =>
     *     JInt(x+1)
     * }
     * res0: net.liftweb.json.JsonAST.JValue = JArray(List(JInt(2), JInt(3)))
     * }}}
     *
     * Without type matching, notice that we get the result of the transform
     * replacing the array:
     *
     * {{{
     * > JArray(JInt(1) :: JInt(2) :: Nil) transform {
     *   case _ =>
     *     JString("hello")
     * }
     * res0: net.liftweb.json.JsonAST.JValue = JString("hello")
     * }}}
     *
     * @return This `JValue` with its child values recursively transformed by
     *         the given `PartialFunction`, when defined. If the
     *         `PartialFunction` is undefined, leaves the child values
     *         untouched.
     */
    def transform(f: PartialFunction[JValue, JValue]): JValue = map { x =>
      if (f.isDefinedAt(x)) f(x) else x
    }

    /**
     * Return a new `JValue` resulting from replacing the value at the specified field
     * path with the replacement value provided. This has no effect if the path
     * is empty or if the value is not a `[[JObject]]` instance.
     *
     * Example:
     *
     * {{{
     * > JObject(List(JField("foo", JObject(List(JField("bar", JInt(1))))))).replace("foo" :: "bar" :: Nil, JString("baz"))
     * JObject(List(JField("foo", JObject(List(JField("bar", JString("baz")))))))
     * }}}
     */
    def replace(l: List[String], replacement: JValue): JValue = {
      def rep(l: List[String], in: JValue): JValue = {
        l match {
          case x :: xs => in match {
            case JObject(fields) => JObject(
              fields.map {
                case JField(`x`, value) => JField(x, if (xs == Nil) replacement else rep(xs, value))
                case field => field
              }
            )
            case other => other
          }

          case Nil => in
        }
      }

      rep(l, this)
    }

    /**
     * Return the first field from this `JValue` which matches the given predicate.
     *
     * When invoked on a `[[JObject]]` it will first attempt to see if the `JObject` has the field defined on it.
     * Not finding the field defined, this method will recurse into the fields of that object and search for the
     * value there. When invoked on or encountering a `[[JArray]]` during recursion this method will run its search
     * on each member of the `JArray`.
     *
     * Example:
     *
     * {{{
     * > JObject(JField("age", JInt(2))) findField {
     *   case JField(n, v) =>
     *     n == "age"
     * }
     * res0: Option[net.liftweb.json.JsonAST.JField] = Some(JField(age,JInt(2))
     * }}}
     */
    def findField(p: JField => Boolean): Option[JField] = {
      def find(json: JValue): Option[JField] = json match {
        case JObject(fs) if (fs find p).isDefined => return fs find p
        case JObject(fs) => fs.flatMap { case JField(n, v) => find(v) }.headOption
        case JArray(l) => l.flatMap(find _).headOption
        case _ => None
      }
      find(this)
    }

    /**
     * Return the first element from a `JValue` which matches the given predicate.
     *
     * Example:
     *
     * {{{
     * > JArray(JInt(1) :: JInt(2) :: Nil) find { _ == JInt(2) }
     * res0: Option[net.liftweb.json.JsonAST.JValue] = Some(JInt(2))
     * }}}
     */
    def find(p: JValue => Boolean): Option[JValue] = {
      def find(json: JValue): Option[JValue] = {
        json match {
          case _ if p(json) => Some(json)
          case JObject(fs) => fs.flatMap { case JField(n, v) => find(v) }.headOption
          case JArray(l) => l.flatMap(find _).headOption
          case _ => None
        }
      }

      find(this)
    }

    /**
     * Return a `List` of all fields that match the given predicate. Does not
     * recurse into child elements, so this will only check a `JObject`'s field
     * values.
     *
     * Example:
     *
     * {{{
     * > JObject(JField("age", JInt(10))) filterField {
     *   case JField("age", JInt(x)) if x > 18 =>
     *     true
     *
     *   case _ =>
     *     false
     * }
     * res0: List[net.liftweb.json.JsonAST.JField] = List()
     * > JObject(JField("age", JInt(10))) filterField {
     *   case JField("age", JInt(x)) if x < 18 =>
     *     true
     *
     *   case _ =>
     *     false
     * }
     * res1: List[net.liftweb.json.JsonAST.JField] = List(JField(age,JInt(10)))
     * }}}
     *
     * @return A `List` of `JField`s that match the given predicate `p`, or `Nil`
     *         if this `JValue` is not a `JObject`.
     */
    def filterField(p: JField => Boolean): List[JField] =
      foldField(List[JField]())((acc, e) => if (p(e)) e :: acc else acc).reverse

    /**
     * Return a List of all values which matches the given predicate, recursively.
     *
     * Example:
     *
     * {{{
     * > JArray(JInt(1) :: JInt(2) :: Nil) filter {
     *   case JInt(x) => x > 1
     *   case _ => false
     * }
     * res0: List[net.liftweb.json.JsonAST.JValue] = List(JInt(2))
     * }}}
     *
     * This operates recursively, so nested objects work too:
     * {{{
     * > ((("boom" -> ("slam" -> "hello")) ~ ("shap" -> 3)): JObject) filter {
     *   case JString("hello") => true
     *   case _ => false
     * }
     * res0: List[net.liftweb.json.JsonAST.JValue] = List(JString(hello))
     * }}}
     */
    def filter(p: JValue => Boolean): List[JValue] =
      fold(List[JValue]())((acc, e) => if (p(e)) e :: acc else acc).reverse

    /**
     * Create a new instance of `[[WithFilter]]` for Scala to use when using
     * this `JValue` in a for comprehension.
     */
    def withFilter(p: JValue => Boolean) = new WithFilter(this, p)

    final class WithFilter(self: JValue, p: JValue => Boolean) {
      def map[A](f: JValue => A): List[A] = self filter p map f
      def flatMap[A](f: JValue => List[A]) = self filter p flatMap f
      def withFilter(q: JValue => Boolean): WithFilter = new WithFilter(self, x => p(x) && q(x))
      def foreach[U](f: JValue => U): Unit = self filter p foreach f
    }

    /**
     * Concatenate this `JValue` with another `JValue`.
     *
     * Example:
     *
     * {{{
     * > JArray(JInt(1) :: JInt(2) :: Nil) ++ JArray(JInt(3) :: Nil)
     * res0: JArray(List(JInt(1), JInt(2), JInt(3)))
     * }}}
     */
    def ++(other: JValue) = {
      def append(value1: JValue, value2: JValue): JValue = (value1, value2) match {
        case (JNothing, x) => x
        case (x, JNothing) => x
        case (JArray(xs), JArray(ys)) => JArray(xs ::: ys)
        case (JArray(xs), v: JValue) => JArray(xs ::: List(v))
        case (v: JValue, JArray(xs)) => JArray(v :: xs)
        case (x, y) => JArray(x :: y :: Nil)
      }
      append(this, other)
    }

    /**
     * Return a `JValue` where all fields matching the given predicate are removed.
     *
     * Example:
     *
     * {{{
     * > JObject(JField("age", JInt(10))) removeField {
     *   case JField("age", _) => true
     *   case _          => false
     * }
     * }}}
     */
    def removeField(p: JField => Boolean): JValue = this mapField {
      case x if p(x) => JField(x.name, JNothing)
      case x => x
    }

    /**
     * Return a JSON where all values matching the given predicate are removed.
     *
     * Example:
     *
     * {{{
     * > JArray(JInt(1) :: JInt(2) :: JNull :: Nil).remove(_ == JNull)
     * res0: net.liftweb.json.JsonAST.JValue = JArray(List(JInt(1), JInt(2), JNothing))
     * }}}
     */
    def remove(p: JValue => Boolean): JValue = this map {
      case x if p(x) => JNothing
      case x => x
    }

    /**
     * Extract a value into a concrete Scala instance from its `JValue` representation.
     *
     * Value can be:
     *  - a case class
     *  - a primitive (String, Boolean, Date, etc.)>
     *  - any type which has a configured [[TypeHints custom deserializer]]
     *  - a supported collection type of any of the above (List, Seq, Map[String, _], Set)
     *
     * Example:
     *
     * {{{
     * > case class Person(name: String)
     * > JObject(JField("name", JString("joe")) :: Nil).extract[Person]
     * res0: Person("joe")
     * }}}
     */
    def extract[A](implicit formats: Formats, mf: scala.reflect.Manifest[A]): A =
      Extraction.extract(this)(formats, mf)

    /**
     * Optionally extract a value into a concrete Scala instance from its `JValue` representation.
     *
     * This method will attempt to extract a concrete Scala instance of type `A`, but if it fails
     * it will return a `[[scala.None]]` instead of throwing an exception as `[[extract]]` would.
     *
     * Value can be:
     *  - a case class
     *  - a primitive (String, Boolean, Date, etc.)>
     *  - any type which has a configured [[TypeHints custom deserializer]]
     *  - a supported collection type of any of the above (List, Seq, Map[String, _], Set)
     *
     * Example:
     *
     * {{{
     * scala> case class Person(name: String)
     * defined class Person
     *
     * scala> implicit val formats = DefaultFormats
     * formats: net.liftweb.json.DefaultFormats.type = net.liftweb.json.DefaultFormats$@39afbb7c
     *
     * scala> JObject(JField("name", JString("joe")) :: Nil).extractOpt[Person]
     * res1: Option[Person] = Some(Person(joe))
     * }}}
     */
    def extractOpt[A](implicit formats: Formats, mf: scala.reflect.Manifest[A]): Option[A] =
      Extraction.extractOpt(this)(formats, mf)

    /**
     * Attempt to extract a concrete Scala instance of type `A` from this `JValue` and, on failing to do so, return
     * the default value instead.
     *
     * Value can be:
     *  - a case class
     *  - a primitive (String, Boolean, Date, etc.)>
     *  - any type which has a configured [[TypeHints custom deserializer]]
     *  - a supported collection type of any of the above (List, Seq, Map[String, _], Set)
     *
     * Example:
     *
     * {{{
     * > case class Person(name: String)
     * > JNothing.extractOrElse(Person("joe"))
     * res0: Person("joe")
     * }}}
     */
    def extractOrElse[A](default: => A)(implicit formats: Formats, mf: scala.reflect.Manifest[A]): A =
      Extraction.extractOpt(this)(formats, mf).getOrElse(default)

    def toOpt: Option[JValue] = this match {
      case JNothing => None
      case json     => Some(json)
    }
  }

  case object JNothing extends JValue {
    type Values = None.type
    def values = None
  }
  case object JNull extends JValue {
    type Values = Null
    def values = null
  }
  case class JString(s: String) extends JValue {
    type Values = String
    def values = s
  }
  case class JDouble(num: Double) extends JValue {
    type Values = Double
    def values = num
  }
  case class JInt(num: BigInt) extends JValue {
    type Values = BigInt
    def values = num
  }
  case class JBool(value: Boolean) extends JValue {
    type Values = Boolean
    def values = value
  }

  case class JObject(obj: List[JField]) extends JValue {
    type Values = Map[String, Any]
    def values = {
      obj.map {
        case JField(name, value) =>
          (name, value.values): (String, Any)
      }.toMap
    }

    override def equals(that: Any): Boolean = that match {
      case o: JObject => obj.toSet == o.obj.toSet
      case _ => false
    }

    override def hashCode = obj.toSet[JField].hashCode
  }
  case object JObject {
    def apply(fs: JField*): JObject = JObject(fs.toList)
  }

  case class JArray(arr: List[JValue]) extends JValue {
    type Values = List[Any]
    def values = arr.map(_.values)
    override def apply(i: Int): JValue = arr(i)
  }

  case class JField(name: String, value: JValue)

  private[json] def quote(s: String): String = {
    val buf = new StringBuilder
    appendEscapedString(buf, s, RenderSettings.compact)
    buf.toString
  }

  private def appendEscapedString(buf: Appendable, s: String, settings: RenderSettings) {
    s.foreach { c =>
      val strReplacement = c match {
        case '"'  => "\\\""
        case '\\' => "\\\\"
        case '\b' => "\\b"
        case '\f' => "\\f"
        case '\n' => "\\n"
        case '\r' => "\\r"
        case '\t' => "\\t"
        // Set.contains will cause boxing of c to Character, try and avoid this
        case c if ((c >= '\u0000' && c < '\u0020')) || (settings.escapeChars.nonEmpty && settings.escapeChars.contains(c)) =>
          "\\u%04x".format(c: Int)

        case _ => ""
      }

      // Use Char version of append if we can, as it's cheaper.
      if (strReplacement.isEmpty) {
        buf.append(c)
      } else {
        buf.append(strReplacement)
      }
    }
  }

  object RenderSettings {
    /**
     * Pretty-print JSON with 2-space indentation.
     */
    val pretty = RenderSettings(2)
    /**
     * Compact print JSON on one line.
     */
    val compact = RenderSettings(0)

    /**
     * Ranges of chars that should be escaped if this JSON is to be evaluated
     * directly as JavaScript (rather than by a valid JSON parser).
     */
    val jsEscapeChars =
      List(('\u00ad', '\u00ad'),
           ('\u0600', '\u0604'),
           ('\u070f', '\u070f'),
           ('\u17b4', '\u17b5'),
           ('\u200c', '\u200f'),
           ('\u2028', '\u202f'),
           ('\u2060', '\u206f'),
           ('\ufeff', '\ufeff'),
           ('\ufff0', '\uffff'))
        .foldLeft(Set[Char]()) {
          case (set, (start, end)) =>
            set ++ (start to end).toSet
        }

    /**
     * Pretty-print JSON with 2-space indentation and escape all JS-sensitive
     * characters.
     */
    val prettyJs = RenderSettings(2, jsEscapeChars)
    /**
     * Compact print JSON on one line and escape all JS-sensitive characters.
     */
    val compactJs = RenderSettings(0, jsEscapeChars)
  }

  /**
   * Parent trait for double renderers, which decide how doubles contained in
   * a JDouble are rendered to JSON string.
   */
  sealed trait DoubleRenderer extends Function1[Double,String] {
    def apply(double: Double): String
  }
  /**
   * A `DoubleRenderer` that renders special values `NaN`, `-Infinity`, and
   * `Infinity` as-is using `toString`. This is not valid JSON, meaning JSON
   * libraries generally won't be able to parse it (including lift-json!), but
   * JavaScript can eval it. Other double values are also rendered the same
   * way.
   *
   * Usage is not recommended.
   */
  case object RenderSpecialDoubleValuesAsIs extends DoubleRenderer {
    def apply(double: Double): String = {
      double.toString
    }
  }
  /**
   * A `DoubleRenderer` that renders special values `NaN`, `-Infinity`, and
   * `Infinity` as `null`. Other doubles are rendered normally using
   * `toString`.
   */
  case object RenderSpecialDoubleValuesAsNull extends DoubleRenderer {
    def apply(double: Double): String = {
      if (double.isNaN || double.isInfinity) {
        "null"
      } else {
        double.toString
      }
    }
  }
  /**
   * A `DoubleRenderer` that throws an `IllegalArgumentException` when the
   * special values `NaN`, `-Infinity`, and `Infinity` are encountered. Other
   * doubles are rendered normally using `toString`.
   */
  case object FailToRenderSpecialDoubleValues extends DoubleRenderer {
    def apply(double: Double): String = {
      if (double.isNaN || double.isInfinity) {
        throw new IllegalArgumentException(s"Double value $double cannot be rendered to JSON with the current DoubleRenderer.")
      } else {
        double.toString
      }
    }
  }
  /**
   * RenderSettings allows for customizing how JSON is rendered to a String.
   * At the moment, you can customize the indentation (if 0, all the JSON is
   * printed on one line), the characters that should be escaped (in addition
   * to a base set that will always be escaped for valid JSON), and whether or
   * not a space should be included after a field name.
   *
   * @param doubleRendering Before Lift 3.1.0, the three special double values
   *    NaN, Infinity, and -Infinity were serialized as-is. This is invalid
   *    JSON, but valid JavaScript. We now default special double values to
   *    serialize as null, but provide both the old behavior and a new behavior
   *    that throws an exception upon finding these values. See
   *    `[[DoubleRenderer]]` and its subclasses for more.
   */
  case class RenderSettings(
    indent: Int,
    escapeChars: Set[Char] = Set.empty,
    spaceAfterFieldName: Boolean = false,
    doubleRenderer: DoubleRenderer = RenderSpecialDoubleValuesAsNull
  ) {
    val lineBreaks_? = indent > 0
  }

  /**
   * Render `value` using `[[RenderSettings.pretty]]`.
   */
  def prettyRender(value: JValue): String = {
    render(value, RenderSettings.pretty)
  }

  /**
   * Render `value` to the given `appendable` using `[[RenderSettings.pretty]]`.
   */
  def prettyRender(value: JValue, appendable: Appendable): String = {
    render(value, RenderSettings.pretty, appendable)
  }

  /** Renders JSON directly to string in compact format.
    * This is an optimized version of compact(render(value))
    * when the intermediate Document is not needed.
    */
  def compactRender(value: JValue): String = {
    render(value, RenderSettings.compact)
  }

  /**
   * Render `value` to the given `appendable` using `[[RenderSettings.compact]]`.
   */
  def compactRender(value: JValue, appendable: Appendable): String = {
    render(value, RenderSettings.compact, appendable)
  }

  /**
   * Render `value` to the given `appendable` (a `StringBuilder`, by default)
   * using the given `settings`. The appendable's `toString` will be called and
   * the result will be returned.
   */
  def render(value: JValue, settings: RenderSettings, appendable: Appendable = new StringBuilder()): String = {
    bufRender(value, appendable, settings).toString()
  }

  case class RenderIntermediaryDocument(value: JValue)
  def render(value: JValue) = RenderIntermediaryDocument(value)

  /**
   *
   * @param value the JSON to render
   * @param buf the buffer to render the JSON into. may not be empty
   */
  private def bufRender(value: JValue, buf: Appendable, settings: RenderSettings, indentLevel: Int = 0): Appendable = value match {
    case null          => buf.append("null")
    case JBool(true)   => buf.append("true")
    case JBool(false)  => buf.append("false")
    case JDouble(n)    => buf.append(settings.doubleRenderer(n))
    case JInt(n)       => buf.append(n.toString)
    case JNull         => buf.append("null")
    case JString(null) => buf.append("null")
    case JString(s)    => bufQuote(s, buf, settings)
    case JArray(arr)   => bufRenderArr(arr, buf, settings, indentLevel)
    case JObject(obj)  => bufRenderObj(obj, buf, settings, indentLevel)
    case JNothing      => sys.error("can't render 'nothing'") //TODO: this should not throw an exception
  }

  private def bufRenderArr(values: List[JValue], buf: Appendable, settings: RenderSettings, indentLevel: Int): Appendable = {
    var firstEntry = true
    val currentIndent = indentLevel + settings.indent

    buf.append('[') //open array

    if (! values.isEmpty) {
      if (settings.lineBreaks_?) {
        buf.append('\n')
      }

      values.foreach { elem =>
        if (elem != JNothing) {
          if (firstEntry) {
            firstEntry = false
          } else {
            buf.append(',')

            if (settings.lineBreaks_?) {
              buf.append('\n')
            }
          }

          (0 until currentIndent).foreach(_ => buf.append(' '))
          bufRender(elem, buf, settings, currentIndent)
        }
      }

      if (settings.lineBreaks_?) {
        buf.append('\n')
      }

      (0 until indentLevel).foreach(_ => buf.append(' '))
    }

    buf.append(']')
    buf
  }

  private def bufRenderObj(fields: List[JField], buf: Appendable, settings: RenderSettings, indentLevel: Int): Appendable = {
    var firstEntry = true
    val currentIndent = indentLevel + settings.indent

    buf.append('{') //open bracket

    if (! fields.isEmpty) {
      if (settings.lineBreaks_?) {
        buf.append('\n')
      }

      fields.foreach {
        case JField(name, value) if value != JNothing =>
          if (firstEntry) {
            firstEntry = false
          } else {
            buf.append(',')

            if (settings.lineBreaks_?) {
              buf.append('\n')
            }
          }

          (0 until currentIndent).foreach(_ => buf.append(' '))

          bufQuote(name, buf, settings)
          buf.append(':')
          if (settings.spaceAfterFieldName) {
            buf.append(' ')
          }
          bufRender(value, buf, settings, currentIndent)

        case _ => // omit fields with value of JNothing
      }

      if (settings.lineBreaks_?) {
        buf.append('\n')
      }

      (0 until indentLevel).foreach(_ => buf.append(' '))
    }

    buf.append('}') //close bracket
    buf
  }

  private def bufQuote(s: String, buf: Appendable, settings: RenderSettings): Appendable = {
    buf.append('"') //open quote
    appendEscapedString(buf, s, settings)
    buf.append('"') //close quote
    buf
  }

}

/** Basic implicit conversions from primitive types into JSON.
  * Example:<pre>
  * import net.liftweb.json.Implicits._
  * JObject(JField("name", "joe") :: Nil) == JObject(JField("name", JString("joe")) :: Nil)
  * </pre>
  */
object Implicits extends Implicits
trait Implicits {
  implicit def int2jvalue(x: Int) = JInt(x)
  implicit def long2jvalue(x: Long) = JInt(x)
  implicit def bigint2jvalue(x: BigInt) = JInt(x)
  implicit def double2jvalue(x: Double) = JDouble(x)
  implicit def float2jvalue(x: Float) = JDouble(x)
  implicit def bigdecimal2jvalue(x: BigDecimal) = JDouble(x.doubleValue)
  implicit def boolean2jvalue(x: Boolean) = JBool(x)
  implicit def string2jvalue(x: String) = JString(x)
}

/** A DSL to produce valid JSON.
  * Example:<pre>
  * import net.liftweb.json.JsonDSL._
  * ("name", "joe") ~ ("age", 15) == JObject(JField("name",JString("joe")) :: JField("age",JInt(15)) :: Nil)
  * </pre>
  */
object JsonDSL extends JsonDSL
trait JsonDSL extends Implicits {
  implicit def seq2jvalue[A <% JValue](s: Traversable[A]) =
    JArray(s.toList.map { a => val v: JValue = a; v })

  implicit def map2jvalue[A <% JValue](m: Map[String, A]) =
    JObject(m.toList.map { case (k, v) => JField(k, v) })

  implicit def option2jvalue[A <% JValue](opt: Option[A]): JValue = opt match {
    case Some(x) => x
    case None => JNothing
  }

  implicit def symbol2jvalue(x: Symbol) = JString(x.name)
  implicit def pair2jvalue[A <% JValue](t: (String, A)) = JObject(List(JField(t._1, t._2)))
  implicit def list2jvalue(l: List[JField]) = JObject(l)
  implicit def jobject2assoc(o: JObject) = new JsonListAssoc(o.obj)
  implicit def pair2Assoc[A <% JValue](t: (String, A)) = new JsonAssoc(t)

  class JsonAssoc[A <% JValue](left: (String, A)) {
    def ~[B <% JValue](right: (String, B)) = {
      val l: JValue = left._2
      val r: JValue = right._2
      JObject(JField(left._1, l) :: JField(right._1, r) :: Nil)
    }

    def ~(right: JObject) = {
      val l: JValue = left._2
      JObject(JField(left._1, l) :: right.obj)
    }
  }

  class JsonListAssoc(left: List[JField]) {
    def ~(right: (String, JValue)) = JObject(left ::: List(JField(right._1, right._2)))
    def ~(right: JObject) = JObject(left ::: right.obj)
  }
}
