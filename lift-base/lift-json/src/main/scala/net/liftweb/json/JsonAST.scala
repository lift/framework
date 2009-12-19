package net.liftweb.json

/*
 * Copyright 2009 WorldWide Conferencing, LLC
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

object JsonAST {
  import scala.text.Document
  import scala.text.Document._

  def concat(xs: JValue*) = xs.foldLeft(JNothing: JValue)(_ ++ _)  

  sealed abstract class JValue extends Merge.Mergeable with Diff.Diffable {
    type Values

    def \(nameToFind: String): JValue = {
      val p = (json: JValue) => json match {
        case JField(name, value) if name == nameToFind => true
        case _ => false
      }
      findDirect(children, p) match {
        case Nil => JNothing
        case x :: Nil => x
        case x => JArray(x)
      }
    }

    private def findDirect(xs: List[JValue], p: JValue => Boolean): List[JValue] = xs.flatMap {
      case JObject(l) => l.filter {
        case x if p(x) => true
        case _ => false
      }
      case JArray(l) => findDirect(l, p)
      case x if p(x) => x :: Nil
      case _ => Nil
    }

    // FIXME this must be tail recursive
    def \\(nameToFind: String): JValue = {
      def find(json: JValue): List[JField] = json match {
        case JObject(l) => l.foldLeft(List[JField]())((a, e) => a ::: find(e))
        case JArray(l) => l.foldLeft(List[JField]())((a, e) => a ::: find(e))
        case field @ JField(name, value) if name == nameToFind => field :: find(value)
        case JField(_, value) => find(value)
        case _ => Nil
      }
      find(this) match {
        case x :: Nil => x
        case x => JObject(x)
      }
    }

    def \[A <: JValue](clazz: Class[A]): List[A#Values] = 
      findDirect(children, typePredicate(clazz) _).asInstanceOf[List[A]] map { _.values }

    def \\[A <: JValue](clazz: Class[A]): List[A#Values] = 
      (this filter typePredicate(clazz) _).asInstanceOf[List[A]] map { _.values }

    private def typePredicate[A <: JValue](clazz: Class[A])(json: JValue) = json match {
      case x if x.getClass == clazz => true
      case _ => false
    }

    def apply(i: Int): JValue = JNothing

    def values: Values

    def children = this match {
      case JObject(l) => l
      case JArray(l) => l
      case JField(n, v) => List(v)
      case _ => Nil
    }

    def fold[A](z: A)(f: (A, JValue) => A): A = {
      def rec(acc: A, v: JValue) = {
        val newAcc = f(acc, v)
        v match {
          case JObject(l) => l.foldLeft(newAcc)((a, e) => e.fold(a)(f))
          case JArray(l) => l.foldLeft(newAcc)((a, e) => e.fold(a)(f))
          case JField(_, value) => value.fold(newAcc)(f)
          case _ => newAcc
        }
      }
      rec(z, this)
    }

    def map(f: JValue => JValue): JValue = {
      def rec(v: JValue): JValue = v match {
        case JObject(l) => f(JObject(l.map(f => rec(f) match {
          case x: JField => x
          case x => JField(f.name, x)
        })))
        case JArray(l) => f(JArray(l.map(rec)))
        case JField(name, value) => f(JField(name, rec(value)))
        case x => f(x)
      }
      rec(this)
    }

    def find(p: JValue => Boolean): Option[JValue] = {
      def find(json: JValue): Option[JValue] = {
        if (p(json)) return Some(json)
        json match {
          case JObject(l) => l.flatMap(find _).firstOption
          case JArray(l) => l.flatMap(find _).firstOption
          case JField(_, value) => find(value)
          case _ => None
        }
      }
      find(this)
    }

    def filter(p: JValue => Boolean): List[JValue] = 
      fold(List[JValue]())((acc, e) => if (p(e)) e :: acc else acc).reverse

    def ++(other: JValue) = {
      def append(value1: JValue, value2: JValue): JValue = (value1, value2) match {
        case (JNothing, x) => x
        case (x, JNothing) => x
        case (JObject(xs), x: JField) => JObject(xs ::: List(x))
        case (x: JField, JObject(xs)) => JObject(x :: xs)
        case (JArray(xs), JArray(ys)) => JArray(xs ::: ys)
        case (JArray(xs), v: JValue) => JArray(xs ::: List(v))
        case (v: JValue, JArray(xs)) => JArray(v :: xs)
        case (f1: JField, f2: JField) => JObject(f1 :: f2 :: Nil)
        case (JField(n, v1), v2: JValue) => JField(n, append(v1, v2))
        case (x, y) => JArray(x :: y :: Nil)
      }
      append(this, other)
    }

    def remove(p: JValue => Boolean): JValue = this map {
      case x if p(x) => JNothing
      case x => x
    }

    def extract[A](implicit formats: Formats, mf: scala.reflect.Manifest[A]) = 
      Extraction.extract(this)(formats, mf)
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
  case class JField(name: String, value: JValue) extends JValue {
    type Values = (String, value.Values)
    def values = (name, value.values)
    override def apply(i: Int): JValue = value(i)
  }
  case class JObject(obj: List[JField]) extends JValue {
    type Values = Map[String, Any]
    def values = Map() ++ obj.map(_.values : (String, Any))
  }
  case class JArray(arr: List[JValue]) extends JValue {
    type Values = List[Any]
    def values = arr.map(_.values)
    override def apply(i: Int): JValue = arr(i)
  }

  def render(value: JValue): Document = value match {
    case null          => text("null")
    case JBool(true)   => text("true")
    case JBool(false)  => text("false")
    case JDouble(n)    => text(n.toString)
    case JInt(n)       => text(n.toString)
    case JNull         => text("null")
    case JNothing      => error("can't render 'nothing'")
    case JString(null) => text("null")
    case JString(s)    => text("\"" + quote(s) + "\"")
    case JArray(arr)   => text("[") :: series(trimArr(arr).map(render(_))) :: text("]")
    case JField(n, v)  => text("\"" + n + "\":") :: render(v)
    case JObject(obj)  => 
      val nested = break :: fields(trimObj(obj).map(f => text("\"" + f.name + "\":") :: render(f.value)))
      text("{") :: nest(2, nested) :: break :: text("}")
  }

  private def trimArr(xs: List[JValue]) = xs.filter(_ != JNothing)
  private def trimObj(xs: List[JField]) = xs.filter(_.value != JNothing)
  private def fold(docs: List[Document]) = docs.foldLeft[Document](empty)(_ :: _)
  private def series(docs: List[Document]) = fold(punctuate(text(","), docs))
  private def fields(docs: List[Document]) = fold(punctuate(text(",") :: break, docs))
  private def punctuate(p: Document, docs: List[Document]): List[Document] = docs match {
    case Nil => Nil
    case List(d) => List(d)
    case d :: ds => (d :: p) :: punctuate(p, ds)
  }

  private def quote(s: String) = (s.map { 
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\b' => "\\b"
      case '\f' => "\\f"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c if ((c >= '\u0000' && c < '\u001f') || (c >= '\u0080' && c < '\u00a0') || (c >= '\u2000' && c < '\u2100')) => "\\u%04x".format(c: Int)
      case c => c
    }).mkString
}

object Implicits extends Implicits
trait Implicits {
  import JsonAST._

  implicit def int2jvalue(x: Int) = JInt(x)
  implicit def long2jvalue(x: Long) = JInt(x)
  implicit def bigint2jvalue(x: BigInt) = JInt(x)
  implicit def double2jvalue(x: Double) = JDouble(x)
  implicit def bigdecimal2jvalue(x: BigDecimal) = JDouble(x.doubleValue)
  implicit def boolean2jvalue(x: Boolean) = JBool(x)
  implicit def string2jvalue(x: String) = JString(x)
}

object JsonDSL extends Implicits with Printer {
  import JsonAST._

  implicit def seq2jvalue[A <% JValue](s: Seq[A]) = JArray(s.toList.map { a => val v: JValue = a; v })
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

object Printer extends Printer
trait Printer {
  import java.io._
  import scala.text._

  def compact(d: Document): String = compact(d, new StringWriter).toString

  def compact[A <: Writer](d: Document, out: A): A = {
    def layout(doc: Document): Unit = doc match {
      case DocText(s)      => out.write(s) 
      case DocCons(d1, d2) => layout(d1); layout(d2)
      case DocBreak        => 
      case DocNest(_, d)   => layout(d)
      case DocGroup(d)     => layout(d)
      case DocNil          => 
    }
    layout(d)
    out.flush
    out
  }

  def pretty(d: Document): String = pretty(d, new StringWriter).toString

  def pretty[A <: Writer](d: Document, out: A): A = {
    d.format(80, out)
    out
  }
}
