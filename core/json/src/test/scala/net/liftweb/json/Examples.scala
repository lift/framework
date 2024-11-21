/*
 * Copyright 2009-2013 WorldWide Conferencing, LLC
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

import org.specs2.mutable.Specification

class Examples extends AbstractExamples {
  override def print(value: JValue): String = compactRender(value)
}


trait AbstractExamples extends Specification {
  import Examples._
  import JsonAST.concat
  import JsonDSL._

  def print(value: JValue): String

  "Lotto example" in {
    val json = parse(lotto)
    val renderedLotto = print(json)
    json mustEqual parse(renderedLotto)
  }

  "Person example" in {
    val json = parse(person)
    val renderedPerson = prettyRender(json)
    (json mustEqual parse(renderedPerson)) and
      (print(json \\ "name") mustEqual """{"name":"Joe","name":"Marilyn"}""") and
      (print(json \ "person" \ "name") mustEqual "\"Joe\"")
  }

  "Transformation example" in {
    val uppercased = parse(person).transformField { case JField(n, v) => JField(n.toUpperCase, v) }
    val rendered = compactRender(uppercased)
    rendered mustEqual
      """{"PERSON":{"NAME":"Joe","AGE":35,"SPOUSE":{"PERSON":{"NAME":"Marilyn","AGE":33}}}}"""
  }

  "Remove example" in {
    val json = parse(person) removeField { _ == JField("name", "Marilyn") }
    compactRender(json \\ "name") mustEqual """{"name":"Joe"}"""
  }

  "Queries on person example" in {
    val json = parse(person)
    val filtered = json filterField {
      case JField("name", _) => true
      case _ => false
    }
    filtered mustEqual List(JField("name", JString("Joe")), JField("name", JString("Marilyn")))

    val found = json findField {
      case JField("name", _) => true
      case _ => false
    }
    found mustEqual Some(JField("name", JString("Joe")))
  }

  "Object array example" in {
    val json = parse(objArray)
    (print(json \ "children" \ "name") mustEqual """["Mary","Mazy"]""") and
    (print((json \ "children")(0) \ "name") mustEqual "\"Mary\"") and
    (print((json \ "children")(1) \ "name") mustEqual "\"Mazy\"") and
    ((for { JObject(o) <- json; JField("name", JString(y)) <- o } yield y) mustEqual List("joe", "Mary", "Mazy"))
  }

  "Unbox values using XPath-like type expression" in {
    (parse(objArray) \ "children" \\ classOf[JInt] mustEqual List(5, 3)) and
      (parse(lotto) \ "lotto" \ "winning-numbers" \ classOf[JInt] mustEqual List(2, 45, 34, 23, 7, 5, 3)) and
      (parse(lotto) \\ "winning-numbers" \ classOf[JInt] mustEqual List(2, 45, 34, 23, 7, 5, 3))
  }

  "Quoted example" in {
    val json = parse(quoted)
    List("foo \" \n \t \r bar") mustEqual json.values
  }

  "Null example" in {
    print(parse(""" {"name": null} """)) mustEqual """{"name":null}"""
  }

  "Null rendering example" in {
    print(nulls) mustEqual """{"f1":null,"f2":[null,"s"]}"""
  }

  "Symbol example" in {
    print(symbols) mustEqual """{"f1":"foo","f2":"bar"}"""
  }

  "Unicode example" in {
    parse("[\" \\u00e4\\u00e4li\\u00f6t\"]") mustEqual JArray(List(JString(" \u00e4\u00e4li\u00f6t")))
  }

  "Exponent example" in {
    (parse("""{"num": 2e5 }""") mustEqual JObject(List(JField("num", JDouble(200000.0))))) and
      (parse("""{"num": -2E5 }""") mustEqual JObject(List(JField("num", JDouble(-200000.0))))) and
      (parse("""{"num": 2.5e5 }""") mustEqual JObject(List(JField("num", JDouble(250000.0))))) and
      (parse("""{"num": 2.5e-5 }""") mustEqual JObject(List(JField("num", JDouble(2.5e-5)))))
  }

  "JSON building example" in {
    val json = JObject(JField("name", JString("joe")), JField("age", JInt(34))) ++ JObject(JField("name", ("mazy")), JField("age", JInt(31)))
    print(json) mustEqual """[{"name":"joe","age":34},{"name":"mazy","age":31}]"""
  }

  "JSON building with implicit primitive conversions example" in {
    import Implicits._
    val json = JObject(JField("name", "joe"), JField("age", 34)) ++ JObject(JField("name", "mazy"), JField("age", 31))
    print(json) mustEqual """[{"name":"joe","age":34},{"name":"mazy","age":31}]"""
  }

  "Example which collects all integers and forms a new JSON" in {
    val json = parse(person)
    val ints = json.fold(JNothing: JValue) { (a, v) => v match {
      case x: JInt => a ++ x
      case _ => a
    }}
    print(ints) mustEqual """[35,33]"""
  }

  "Generate JSON with DSL example" in {
    val json: JValue =
      ("id" -> 5) ~
      ("tags" -> Map("a" -> 5, "b" -> 7))
    print(json) mustEqual """{"id":5,"tags":{"a":5,"b":7}}"""
  }

  "Naked JArray with null values" in {
    val json = JArray(List(null))
    print(json) mustEqual """[null]"""
  }

}

object Examples {
  import JsonDSL._

  val lotto = """
{
  "lotto":{
    "lotto-id":5,
    "winning-numbers":[2,45,34,23,7,5,3],
    "winners":[ {
      "winner-id":23,
      "numbers":[2,45,34,23,3, 5]
    },{
      "winner-id" : 54 ,
      "numbers":[ 52,3, 12,11,18,22 ]
    }]
  }
}
"""

  val person = """
{
  "person": {
    "name": "Joe",
    "age": 35,
    "spouse": {
      "person": {
        "name": "Marilyn",
        "age": 33
      }
    }
  }
}
"""

  val personDSL =
    ("person" ->
      ("name" -> "Joe") ~
      ("age" -> 35) ~
      ("spouse" ->
        ("person" ->
          ("name" -> "Marilyn") ~
          ("age" -> 33)
        )
      )
    )

  val objArray =
"""
{ "name": "joe",
  "address": {
    "street": "Bulevard",
    "city": "Helsinki"
  },
  "children": [
    {
      "name": "Mary",
      "age": 5
    },
    {
      "name": "Mazy",
      "age": 3
    }
  ]
}
"""

  val nulls = ("f1" -> (null: String)) ~ ("f2" -> List(null, "s"))
  val quoted = """["foo \" \n \t \r bar"]"""
  val symbols = ("f1" -> Symbol("foo")) ~ ("f2" -> Symbol("bar"))
}
