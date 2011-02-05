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
package util {

import _root_.org.specs._
import _root_.org.specs.runner._
import _root_.scala.xml._
import common._

object CssSelectorSpec extends Specification  {
  import BindHelpers._
  
  "CssSelector" should {
    "fail for garbage input" in {
      CssSelectorParser.parse(" 49234e23") must_== Empty
    }

    "select an id" in {
      CssSelectorParser.parse("#foo").open_! must_== IdSelector("foo", Empty)
    }

    "a selector with cruft at the end must fail" in {
      CssSelectorParser.parse("#foo I like yaks").isDefined must_== false
    }

    ":yak must not parse" in {
      CssSelectorParser.parse(":yak").isDefined must_== false
    }

    ":button must  parse" in {
      CssSelectorParser.parse(":button").open_! must_== 
      AttrSelector("type", "button", Empty)
    }

    ":checkbox must  parse" in {
      CssSelectorParser.parse(":checkbox").open_! must_== 
      AttrSelector("type", "checkbox", Empty)
    }

    ":file must  parse" in {
      CssSelectorParser.parse(":file").open_! must_== 
      AttrSelector("type", "file", Empty)
    }

    ":password must  parse" in {
      CssSelectorParser.parse(":password").open_! must_== 
      AttrSelector("type", "password", Empty)
    }

    ":radio must  parse" in {
      CssSelectorParser.parse(":radio").open_! must_== 
      AttrSelector("type", "radio", Empty)
    }

    ":reset must  parse" in {
      CssSelectorParser.parse(":reset").open_! must_== 
      AttrSelector("type", "reset", Empty)
    }

    ":submit must  parse" in {
      CssSelectorParser.parse(":submit").open_! must_== 
      AttrSelector("type", "submit", Empty)
    }

    ":text must  parse" in {
      CssSelectorParser.parse(":text").open_! must_== 
      AttrSelector("type", "text", Empty)
    }

    "select an id with attr subnodes" in {
      CssSelectorParser.parse("#foo  *[dog] ").open_! must_== 
      IdSelector("foo", Full(AttrSubNode("dog")))
    }

    "select an id with no star attr subnodes" in {
      CssSelectorParser.parse("#foo  [woof] ").open_! must_== 
      IdSelector("foo", Full(AttrSubNode("woof")))
    }

    "select attr/val pair" in {
      CssSelectorParser.parse("frog=dog") must_==
      Full(AttrSelector("frog", "dog", Empty))
    }


    "select attr/val pair single quote" in {
      CssSelectorParser.parse("frog='dog food' *") must_==
      Full(AttrSelector("frog", "dog food", Full(KidsSubNode())))
    }


    "select attr/val pair double quote" in {
      CssSelectorParser.parse("frog=\"dog breath\"") must_==
      Full(AttrSelector("frog", "dog breath", Empty))
    }

    "select name/val pair" in {
      CssSelectorParser.parse("name=dog") must_==
      Full(NameSelector("dog", Empty))
    }

    "select name/val pair" in {
      CssSelectorParser.parse("@dog") must_==
      Full(NameSelector("dog", Empty))
    }

    "select name/val pair" in {
      CssSelectorParser.parse("@dog *") must_==
      Full(NameSelector("dog", Full(KidsSubNode())))
    }

    "select name/val pair" in {
      CssSelectorParser.parse("@dog -*") must_==
      Full(NameSelector("dog", Full(PrependKidsSubNode())))
    }

    "select name/val pair" in {
      CssSelectorParser.parse("@dog *+") must_==
      Full(NameSelector("dog", Full(AppendKidsSubNode())))
    }


    "select name/val pair single quote" in {
      CssSelectorParser.parse("name='dog food' *") must_==
      Full(NameSelector("dog food", Full(KidsSubNode())))
    }


    "select name/val pair double quote" in {
      CssSelectorParser.parse("name=\"dog breath\"") must_==
      Full(NameSelector("dog breath", Empty))
    }

    "select a class" in {
      CssSelectorParser.parse(".foo").open_! must_== ClassSelector("foo", Empty)
    }

    "select a class with subnodes" in {
      CssSelectorParser.parse(".foo  * ").open_! must_== 
      ClassSelector("foo", Full(KidsSubNode()))
    }

    "Support selecting this node" in {
      CssSelectorParser.parse(".foo  ^^ ").open_! must_== 
      ClassSelector("foo", Full(SelectThisNode()))
    }

    "select a class with attr subnodes" in {
      CssSelectorParser.parse(".foo  *[dog] ").open_! must_== 
      ClassSelector("foo", Full(AttrSubNode("dog")))
    }

    "select an id with no star attr subnodes" in {
      CssSelectorParser.parse(".foo  [woof] ").open_! must_== 
      ClassSelector("foo", Full(AttrSubNode("woof")))
    }

  }
}
class CssSelectorSpecTest extends JUnit4(CssSelectorSpec)

}
}
