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
      CssSelectorParser.parse(".foo").open_! must_== IdSelector("foo", Empty)
    }

    "select an id with subnodes" in {
      CssSelectorParser.parse(".foo  * ").open_! must_== 
      IdSelector("foo", Full(KidsSubNode()))
    }

    "select an id with attr subnodes" in {
      CssSelectorParser.parse(".foo  *[dog] ").open_! must_== 
      IdSelector("foo", Full(AttrSubNode("dog")))
    }

    "select an id with no star attr subnodes" in {
      CssSelectorParser.parse(".foo  [woof] ").open_! must_== 
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


    "select name/val pair single quote" in {
      CssSelectorParser.parse("name='dog food' *") must_==
      Full(NameSelector("dog food", Full(KidsSubNode())))
    }


    "select name/val pair double quote" in {
      CssSelectorParser.parse("name=\"dog breath\"") must_==
      Full(NameSelector("dog breath", Empty))
    }

    "select a class" in {
      CssSelectorParser.parse("#foo").open_! must_== ClassSelector("foo", Empty)
    }

    "select a class with subnodes" in {
      CssSelectorParser.parse("#foo  * ").open_! must_== 
      ClassSelector("foo", Full(KidsSubNode()))
    }

    "select a class with attr subnodes" in {
      CssSelectorParser.parse("#foo  *[dog] ").open_! must_== 
      ClassSelector("foo", Full(AttrSubNode("dog")))
    }

    "select an id with no star attr subnodes" in {
      CssSelectorParser.parse("#foo  [woof] ").open_! must_== 
      ClassSelector("foo", Full(AttrSubNode("woof")))
    }

  }
}
class CssSelectorSpecTest extends JUnit4(CssSelectorSpec)

}
}
