package net.liftweb.markdown

/*
 * Copyright 2013 Lift Committers and Contributors
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
 *
 * Based on https://github.com/chenkelmann/actuarius originally developed by
 * Christoph Henkelmann http://henkelmann.eu/
 */

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.xml.Group
import scala.xml.NodeSeq

/**
 * Tests the parsing on block level.
 */
class BlockParsersTest extends AnyFlatSpec with Matchers with BlockParsers {

  "The BlockParsers" should "parse optional empty lines" in {
    val p  = optEmptyLines
    val el = new EmptyLine(" \n")
    apply(p, Nil) should equal(Nil)
    apply(p, List(el)) should equal(List(el))
    apply(p, List(el, el)) should equal(List(el, el))
  }

  it should "accept empty documents" in {
    val p  = markdown
    val el = new EmptyLine(" \n")
    apply(p, Nil) should equal(Nil)
    apply(p, List(el)) should equal(Nil)
    apply(p, List(el, el)) should equal(Nil)
  }

  it should "detect line types" in {
    val p = line(classOf[CodeLine])
    apply(p, List(new CodeLine("    ", "code"))) should equal(new CodeLine("    ", "code"))
    an[IllegalArgumentException] should be thrownBy (apply(p, List(new OtherLine("foo"))))
  }

  it should "correctly override list items markup" in {
    object MyDecorator   extends Decorator   {
      override def decorateItemOpen():  String = "<foo>"
      override def decorateItemClose(): String = "</foo>"
    }
    object MyTransformer extends Transformer {
      override def deco(): Decorator = MyDecorator
    }
    MyTransformer.apply("* Content") should equal("<ul>\n<foo>Content</foo></ul>\n")
  }
}
