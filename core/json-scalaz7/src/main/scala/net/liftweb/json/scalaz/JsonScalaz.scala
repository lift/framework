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

package net.liftweb.json.scalaz

// FIXME Needed to due to https://issues.scala-lang.org/browse/SI-6541,
// which causes existential types to be inferred for the generated
// unapply of a case class with a wildcard parameterized type.
// Ostensibly should be fixed in 2.12, which means we're a ways away
// from being able to remove this, though.
import scala.language.existentials

import scalaz.{Equal, Kleisli, Monoid, Semigroup, Show, ValidationNel}
import scalaz.Validation._
import scalaz.std.option._
import net.liftweb.json._

trait Types {
  type Result[+A] = ValidationNel[Error, A]

  sealed trait Error
  case class UnexpectedJSONError(was: JValue, expected: Class[_ <: JValue]) extends Error
  case class NoSuchFieldError(name: String, json: JValue) extends Error
  case class UncategorizedError(key: String, desc: String, args: List[Any]) extends Error

  case object Fail {
    def apply[A](key: String, desc: String, args: List[Any]): Result[A] = 
      failure(UncategorizedError(key, desc, args)).toValidationNel

    def apply[A](key: String, desc: String): Result[A] = 
      failure(UncategorizedError(key, desc, Nil)).toValidationNel
  }

  implicit def JValueShow[A <: JValue]: Show[A] = new Show[A] {
    override def shows(json: A): String = compact(render(json))
  }

  implicit def JValueMonoid: Monoid[JValue] = Monoid.instance(_ ++ _, JNothing)
  implicit def JValueSemigroup: Semigroup[JValue] = Semigroup.instance(_ ++ _)
  implicit def JValueEqual: Equal[JValue] = Equal.equalA

  trait JSONR[A] {
    def read(json: JValue): Result[A]
  }

  trait JSONW[A] {
    def write(value: A): JValue
  }

  trait JSON[A] extends JSONR[A] with JSONW[A]

  implicit def Result2JSONR[A](f: JValue => Result[A]): JSONR[A] = new JSONR[A] {
    def read(json: JValue) = f(json)
  }

  def fromJSON[A: JSONR](json: JValue): Result[A] = implicitly[JSONR[A]].read(json)
  def toJSON[A: JSONW](value: A): JValue = implicitly[JSONW[A]].write(value)

  def field[A: JSONR](name: String)(json: JValue): Result[A] = json match {
    case JObject(fs) => 
      fs.find(_.name == name)
        .map(f => implicitly[JSONR[A]].read(f.value))
        .orElse(implicitly[JSONR[A]].read(JNothing).fold(_ => none, x => some(success(x))))
        .getOrElse(failure(NoSuchFieldError(name, json)).toValidationNel)
    case x => failure(UnexpectedJSONError(x, classOf[JObject])).toValidationNel
  }

  def validate[A: JSONR](name: String): Kleisli[Result, JValue, A] = Kleisli(field[A](name))

  def makeObj(fields: Traversable[(String, JValue)]): JObject = 
    JObject(fields.toList.map { case (n, v) => JField(n, v) })
}

object JsonScalaz extends Types with Lifting with Base with Tuples
