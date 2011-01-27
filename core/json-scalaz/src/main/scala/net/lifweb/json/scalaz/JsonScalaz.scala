package net.liftweb.json.scalaz

import scalaz._
import Scalaz._
import net.liftweb.json._
import net.liftweb.json.JsonAST._

object JsonScalaz {
  trait JSON[A] {
//    def read(json: JValue): Validation[(JValue, Class[A]), A]
    def read(json: JValue): Validation[String, A]
    def write(value: A): JValue
  }

  implicit def boolJSON: JSON[Boolean] = new JSON[Boolean] {
    def read(json: JValue) = json match {
      case JBool(b) => success(b)
      case x => failure(x.toString)
    }

    def write(value: Boolean) = JBool(value)
  }

  implicit def intJSON: JSON[Int] = new JSON[Int] {
    def read(json: JValue) = json match {
      case JInt(x) => success(x.intValue)
      case x => failure(x.toString)
    }

    def write(value: Int) = JInt(BigInt(value))
  }

  implicit def longJSON: JSON[Long] = new JSON[Long] {
    def read(json: JValue) = json match {
      case JInt(x) => success(x.longValue)
      case x => failure(x.toString)
    }

    def write(value: Long) = JInt(BigInt(value))
  }

  implicit def doubleJSON: JSON[Double] = new JSON[Double] {
    def read(json: JValue) = json match {
      case JDouble(x) => success(x)
      case x => failure(x.toString)
    }

    def write(value: Double) = JDouble(value)
  }

  implicit def stringJSON: JSON[String] = new JSON[String] {
    def read(json: JValue) = json match {
      case JString(x) => success(x)
      case x => failure(x.toString)
    }

    def write(value: String) = JString(value)
  }

  implicit def bigintJSON: JSON[BigInt] = new JSON[BigInt] {
    def read(json: JValue) = json match {
      case JInt(x) => success(x)
      case x => failure(x.toString)
    }

    def write(value: BigInt) = JInt(value)
  }

  implicit def jvalueJSON: JSON[JValue] = new JSON[JValue] {
    def read(json: JValue) = success(json)
    def write(value: JValue) = value
  }

  // FIXME abstract for any Traversable?
  implicit def listJSON[A: JSON]: JSON[List[A]] = new JSON[List[A]] {
    def read(json: JValue) = json match {
      case JArray(xs) => 
        xs.map(JsonScalaz.read[A])
          .sequence[PartialApply1Of2[Validation, String]#Apply, A]
      case x => failure(x.toString)
    }

    def write(values: List[A]) = JArray(values.map(x => JsonScalaz.write(x)))
  }

  def read[A: JSON](json: JValue) = implicitly[JSON[A]].read(json)
  def write[A: JSON](value: A) = implicitly[JSON[A]].write(value)
  def field[A: JSON](json: JValue, name: String) = json match {
    case JObject(fs) => 
      fs.find(_.name == name)
        .map(f => implicitly[JSON[A]].read(f.value))
        .getOrElse(failure("no such field '" + name + "'"))
    case x => failure(x.toString)
  }
}
