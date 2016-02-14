/*
 * Copyright 2009-2011 WorldWide Conferencing, LLC
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

object ExtractionBugs extends Specification {
  "Extraction bugs Specification".title

  implicit val formats = DefaultFormats

  "ClassCastException (BigInt) regression 2 must pass" in {
    val opt = OptionOfInt(Some(39))
    Extraction.decompose(opt).extract[OptionOfInt].opt.get mustEqual 39
  }

  "Extraction should not fail when Maps values are Lists" in {
    val m = PMap(Map("a" -> List("b"), "c" -> List("d")))
    Extraction.decompose(m).extract[PMap] mustEqual m
  }

  "Extraction should always choose constructor with the most arguments if more than one constructor exists" in {
    val args = Meta.Reflection.primaryConstructorArgs(classOf[ManyConstructors])
    args.size mustEqual 4
  }

  "Extraction should handle AnyRef" in {
    implicit val formats = DefaultFormats.withHints(FullTypeHints(classOf[ExtractWithAnyRef] :: Nil))
    val json = JObject(JField("jsonClass", JString(classOf[ExtractWithAnyRef].getName)) :: Nil)
    val extracted = Extraction.extract[AnyRef](json)
    extracted mustEqual ExtractWithAnyRef()
  }

  "Extraction should work with unicode encoded field names (issue 1075)" in {
    parse("""{"foo.bar,baz":"x"}""").extract[UnicodeFieldNames] mustEqual UnicodeFieldNames("x")
  }

  "Extraction should not fail if case class has a companion object" in {
    parse("""{"nums":[10]}""").extract[HasCompanion] mustEqual HasCompanion(List(10))
  }

  "Issue 1169" in {
    val json = JsonParser.parse("""{"data":[{"one":1, "two":2}]}""")
    json.extract[Response] mustEqual Response(List(Map("one" -> 1, "two" -> 2)))
  }

  "Extraction should handle List[Option[String]]" in {
    val json = JsonParser.parse("""["one", "two", null]""")
    json.extract[List[Option[String]]] mustEqual List(Some("one"), Some("two"), None)
  }

  "Extraction should fail if you're attempting to extract an option and you're given data of the wrong type" in {
    val json = JsonParser.parse("""{"opt": "hi"}""")
    json.extract[OptionOfInt] must throwA[MappingException].like {
      case e => e.getMessage mustEqual "No usable value for opt\nDo not know how to convert JString(hi) into int"
    }

    val json2 = JString("hi")
    json2.extract[Option[Int]] must throwA[MappingException].like {
      case e => e.getMessage mustEqual "Do not know how to convert JString(hi) into int"
    }
  }

  "deserialize list of homogonous tuples" in {
    implicit val formats = DefaultFormats

    case class Holder(items: List[(String, String)])

    val holder = Holder(List(("string", "string")))
    val serialized = compactRender(Extraction.decompose(holder))

    val deserialized = parse(serialized).extract[Holder]
    deserialized must_== holder
  }

  "deserialize list of heterogenous tuples" in {
    implicit val formats = DefaultFormats

    // MSF: This currently doesn't work with scala primitives?! The type arguments appear as
    // java.lang.Object instead of scala.Int. :/
    case class Holder2(items: List[(String, Integer)])

    val holder = Holder2(List(("string", 10)))
    val serialized = compactRender(Extraction.decompose(holder))

    val deserialized = parse(serialized).extract[Holder2]
    deserialized must_== holder
  }

  case class Response(data: List[Map[String, Int]])

  case class OptionOfInt(opt: Option[Int])

  case class PMap(m: Map[String, List[String]])

  case class ManyConstructors(id: Long, name: String, lastName: String, email: String) {
    def this() = this(0, "John", "Doe", "")
    def this(name: String) = this(0, name, "Doe", "")
    def this(name: String, email: String) = this(0, name, "Doe", email)
  }

  case class ExtractWithAnyRef()

  case class UnicodeFieldNames(`foo.bar,baz`: String)

  object HasCompanion {
    def hello = "hello"
  }
  case class HasCompanion(nums: List[Int])
}
