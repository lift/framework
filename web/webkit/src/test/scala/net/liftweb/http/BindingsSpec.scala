/*
 * Copyright 2010 WorldWide Conferencing, LLC
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
package http {

import _root_.net.liftweb.util.Helpers._
import _root_.org.specs._
import _root_.org.specs.runner._
import _root_.org.specs.Sugar._
import scala.xml.NodeSeq
import scala.xml.Text
import _root_.net.liftweb.common._
import Bindings._

class BindingsSpecTest extends Runner(BindingsSpec) with JUnit with Console
object BindingsSpec extends Specification {
  case class MyClass(str: String, i: Int, other: MyOtherClass)
  case class MyOtherClass(foo: String)
  
  trait MyClassBinding extends DataBinding[MyClass] {
    implicit val otherBinding: DataBinding[MyOtherClass]
  
    override def apply(entity: MyClass) = (xhtml: NodeSeq) => {
      val otherTemplate = chooseTemplate("myclass", "other", xhtml)
      bind(
        "myclass", xhtml, 
        "str" -> Text("#" + entity.str + "#"),
        "i" -> Text(entity.i.toString),
        "other" -> entity.other.bind(otherTemplate)
      )
    }
  }
  
  object myOtherClassBinding extends DataBinding[MyOtherClass] {
    override def apply(other: MyOtherClass) = (xhtml: NodeSeq) => {
      bind("other", xhtml, "foo" -> Text("%" + other.foo + "%"))
    }
  }
  
  implicit object MyClassConcreteBinding extends MyClassBinding {
    override val otherBinding = myOtherClassBinding
  }

  val template = <div>
    <span><myclass:str/></span>
    <span><myclass:i/></span>
    <myclass:other>
      <span><other:foo/></span>
    </myclass:other>
  </div>

  val expected = <div>
    <span>#hi#</span>
    <span>1</span>
    <span>%bar%</span>
  </div>

  "Bindings.binder with an available implicit databinding" should {
    "allow the application of that databinding to an appropriate object" in {
      MyClass("hi", 1, MyOtherClass("bar")).bind(template) must equalIgnoreSpace(expected)
    }
  }

"SHtml" should {
  "deal with # in link" in {
    val session = new LiftSession("hello", "", Empty)

    val href = S.initIfUninitted(session) {
      val a = SHtml.link("/foo#bar", () => true, Text("Test"))

      (a \ "@href").text
    }

    href.endsWith("#bar") must_== true

  }
}

"CSS Selector Transforms" should {
  "retain attributes for input" in {
    val session = new LiftSession("hello", "", Empty)

    S.initIfUninitted(session) {
      val org = <span><input id="frog" class="dog cat"/></span>
      
      val res = ("#frog" #> SHtml.text("", s => ()) ).apply(org)

      (res \ "input" \ "@id").text must_== "frog"
      
      (res \ "input" \ "@class").text must_== "dog cat"
    }
  }
}
}

}}
