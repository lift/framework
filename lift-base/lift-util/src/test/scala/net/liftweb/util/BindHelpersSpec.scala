/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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

object BindHelpersSpec extends Specification  {
  import BindHelpers._
  
  "the mixinAttributes function" should {
    "mixin in all the attributes" in {
      mixinAttributes(<input />)(<input id="10" class="wee" />) must ==/(<input class="wee" id="10"></input>)
    }

    "not mix in the element label" in {
      mixinAttributes(<input />)(<div id="10" class="wee" />) must ==/(<input class="wee" id="10"></input>)
    }

    "handle the empty cases gracefully" in {
      mixinAttributes(<input />)(<div />) must ==/(<input></input>)
    }

    "not lose existing attributes" in {
      mixinAttributes(<input id="10" />)(<div />) must ==/(<input id="10"></input>)
    }

    "replace attributes with updated values" in {
      mixinAttributes(<input id="10" />)(<div id="12" />) must ==/(<input id="12"></input>)
    }
  }

  "the chooseTemplate function" should {
    "select the node matching a given tag and prefix" in {
      chooseTemplate("choose", "tag", <h><choose:tag a="att1">that</choose:tag></h>) must ==/(Text("that"))
    }
    "select the first node matching a given tag and prefix" in {
      chooseTemplate("choose", "tag", <h><choose:tag>that</choose:tag><choose:tag>those</choose:tag></h>) must ==/(Text("that"))
    }
    "return an empty NodeSeq if no node is found" in {
      chooseTemplate("choose", "tag", <h></h>) must be_==(NodeSeq.Empty)
    }
  }
  "the bind(Map, NodeSeq) function" should {
    "replace the content of a lift:bind node with the content of a map where the key is the value of the attribute 'name'" in {
      val map = Map("hello" -> <h1></h1>, "world" -> <b></b>)
      val liftbind = <body>
        <lift:bind name="hello">changethis</lift:bind>
                     </body>

      bind(map, liftbind) must ==/(<body><h1></h1></body>)
    }

    "bind should not peserve attrs on a bound element" in {

      val res:NodeSeq = bind("ledger", <ledger:entry ledger:id="foo" ledger:class="bar" />, "entry" -> <foo/>)

      res must ==/(<foo/>)
    }
  }
  "the bindlist function" should {
    "replace the content of a lift:bind node with the content of a map where the key is the value of the attribute 'name'" in {
      val maps = List(Map("hello" -> <h1></h1>, "world" -> <b></b>))
      val liftbind = <body>
        <lift:bind name="hello">changethis</lift:bind>
                     </body>
      bindlist(maps, liftbind).get must ==/(<body><h1></h1></body>)
    }
  }
  "the bind(namespace, NodeSeq, BindParams*) function" should {
    "replace a node named 'namespace:bindparam name' in a NodeSeq with the String value of the BindParam" in {
      bind("user", <t><user:tag>replacethis</user:tag></t>, "tag" -> "world") must ==/(<t>world</t>)
    }
    "replace a node named 'namespace:bindparam name' in a NodeSeq with the Symbol value of the BindParam" in {
      bind("user", <t><user:tag>replacethis</user:tag></t>, "tag" -> 'world) must ==/(<t>world</t>)
    }
    "replace a node named 'namespace:bindparam name' in a NodeSeq with the NodeSeq value of the BindParam" in {
      bind("user", <user:tag>replacethis</user:tag>, "tag" -> <world></world>) must ==/(<world></world>)
    }
    "replace a node named 'namespace:bindparam name' in a NodeSeq with the NodeSeq value of the BindParam" in {
      bind("user", <user:tag>replacethis</user:tag>, "tag" -> <world></world>) must ==/(<world></world>)
    }
    "replace a node named 'namespace:bindparam name' in a NodeSeq with the function application of a FuncBindParam" in {
      bind("user", <t><user:tag>hello</user:tag></t>, FuncBindParam("tag", (n: NodeSeq) => Text(n.text + " world"))) must ==/(<t>hello world</t>)
    }
    "properly convert a NodeSeq => NodeSeq to a FuncBindParam" in {
      bind("user", <t><user:tag>hello</user:tag></t>, "tag" -> ((n: NodeSeq) => Text(n.text + " world"))) must ==/(<t>hello world</t>)
    }
    "replace an attribute value named 'namespace:bindparam name' in a NodeSeq with a value from a BindParam" in {
      bind("user", <t user:hello="toreplace"></t>, "hello" -> Text("world")) must ==/(<t user:hello="world"></t>)
    }
    "replace an attribute value named 'namespace:bindparam name' in a NodeSeq with a calculated value from a FuncBindParam" in {
      bind("user", <t user:tag="hello"></t>, FuncBindParam("tag", (n: NodeSeq) => Text(n.text + " world"))) must ==/(<t user:tag="hello world"></t>)
    }
    "replace an attribute named 'namespace:bindparam name' in a NodeSeq with a new attribute name and value from an AttrBindParam" in {
      bind("user", <t user:tag="toreplace"></t>, AttrBindParam("tag", Text("world"), "hello")) must ==/(<t hello="world"></t>)
    }

    "replace an attribute named 'namespace:bindparam name' in a NodeSeq with a new attribute name and value from an AttrBindParam using a String" in {
      bind("user", <t user:tag="toreplace"></t>, AttrBindParam("tag", "world", "hello")) must ==/(<t hello="world"></t>)
    }

    "replace an attribute named 'namespace:bindparam name' in a NodeSeq with a new attribute name and calculated value from an FuncAttrBindParam" in {
      bind("user", <t user:tag="dear"></t>, FuncAttrBindParam("tag", (n: NodeSeq) =>Text(n.text + " world"), "hello")) must ==/(<t hello="dear world"></t>)
    }

    "replace an attribute named 'namespace:bindparam name' in a NodeSeq with a blank attribute name and calculated value from an FuncAttrOptionBindParam" in {
      bind("user", <t user:tag="dear"></t>, FuncAttrOptionBindParam("tag", (n: NodeSeq) => None, "hello")) must ==/(<t/>)
    }


    "replace an attribute named 'namespace:bindparam name' in a NodeSeq with a new attribute name and calculated value from an FuncAttrOptionBindParam" in {
      bind("user", <t user:tag="dear"></t>, FuncAttrOptionBindParam("tag", (n: NodeSeq) => Some(Text(n.text + " world")), "hello")) must ==/(<t hello="dear world"></t>)
    }

    "replace an attribute named 'namespace:bindparam name' in a NodeSeq with a blank attribute name and calculated value from an FuncAttrBoxBindParam" in {
      bind("user", <t user:tag="dear"></t>, FuncAttrBoxBindParam("tag", (n: NodeSeq) => Empty, "hello")) must ==/(<t/>)
    }


    "replace an attribute named 'namespace:bindparam name' in a NodeSeq with a new attribute name and calculated value from an FuncAttrOptionBindParam" in {
      bind("user", <t user:tag="dear"></t>, FuncAttrBoxBindParam("tag", (n: NodeSeq) => Full(Text(n.text + " world")), "hello")) must ==/(<t hello="dear world"></t>)
    }



  }
  "the xmlParam function" should {
    "find the value of an attribute in an xml fragment" in {
      xmlParam(<t hello="world">world</t>, "hello") must_== Full("world")
    }
    "return Empty if the value is empty" in {
      xmlParam(<t hello="">world</t>, "hello") must_== Empty
    }
    "return Empty if the attribute is not found" in {
      xmlParam(<t hello="">world</t>, "notfound") must_== Empty
    }
  }

  "The bind helpers should deal correctly with <select>" should {
    "do <select> correctly" in {
      val xhtml = <select name="days"> <stats:options/> </select>

      def options(xhtml: NodeSeq): NodeSeq = {
        <option value="7">week</option><option value="30">month</option> }

      val res = bind("stats", xhtml, "options" -> options _)

      res must ==/(<select name="days"><option value="7">week</option><option value="30">month</option></select>)
    }

    "do <input> correctly" in {
      val xhtml = <input name="days"> <stats:options/> </input>

      def options(xhtml: NodeSeq): NodeSeq = {
        <option value="7">week</option><option value="30">month</option> }

      val res = bind("stats", xhtml, "options" -> options _)

      res must ==/(<input name="days"><option value="7">week</option><option value="30">month</option></input>)
    }

    "do <div> correctly" in {
      val xhtml = <div> <stats:options/> </div>

      def options(xhtml: NodeSeq): NodeSeq = {
        <option value="7">week</option><option value="30">month</option> }

      val res = bind("stats", xhtml, "options" -> options _)
      res must ==/(<div><option value="7">week</option><option value="30">month</option></div>)
    }
  }


  "the bindByName bind(namespace, NodeSeq, BindParams*) function" should {
    "mix in if the element if the type of the elements are the same" in {
      bind("user", <t><input name="user:tag" id="cookie"/></t>, "tag" -> <input name="111222"/>) must ==/(<t><input name="111222" id="cookie"/></t>)
    }
    "replace the element if the replacement element type is not a bindByName type" in {
      bind("user", <t><input name="user:tag" id="cookie"/></t>, "tag" -> "world") must ==/(<t>world</t>)
    }
    "replace the value is value is a null string" in {
      bind("user", <t><input name="user:tag" type="submit" value="press me"/></t>, "tag" -> <input name="111222" type="submit"/>) must ==/(<t><input name="111222" type="submit" value="press me"></input></t>)
    }
    "handle a checkbox" in {
      bind("user", <t><input id="acceptTerms" type="checkbox" name="user:tag"/></t>,
           "tag" -> (<input /> ++
                     <input type="checkbox" name="F893599644556MN4" value="true"/>) ) must ==/(<t><input></input><input name="F893599644556MN4" type="checkbox" value="true" id="acceptTerms"></input></t>)
    }
  }

  "head removal" should {
    "remove <head>" in {
      Helpers.stripHead(<head><i>hello</i></head>) must ==/(<i>hello</i>)
    }

    "ignore non-head" in {
      Helpers.stripHead(<head3><i>hello</i></head3>) must ==/(<head3><i>hello</i></head3>)
    }

    "String subhead" in {
      Helpers.stripHead(<head3><i><head>hello</head></i></head3>) must ==/(<head3><i>hello</i></head3>)
    }
  }

  "Binding attributes" should {
    "handle static, unprefixed attributes" in {
      BindHelpers.bind("test", 
                       <div><div test:x="replace" /></div>,
                       AttrBindParam("x", "staticUnprefixed", "id")) must ==/(<div><div id="staticUnprefixed" /></div>)
    }

    "handle dynamic, unprefixed attributes" in {
      // The Unprefixed attributes that Lift merges in cause the XML equals comparison to fail
      // stringifying and then reparsing fixes it.
      XML.loadString(
        BindHelpers.bind("test", 
                         <div><div test:x="dynamicUnprefixed" /></div>,
                         FuncAttrBindParam("x", {ns : NodeSeq => ns }, "id")).toString) must ==/(<div><div id="dynamicUnprefixed" /></div>)
    }

    "handle static, prefixed attributes" in {
      BindHelpers.bind("test", 
                       <div><div test:x="replace" /></div>,
                       AttrBindParam("x", "staticPrefixed", ("result","id"))) must ==/(<div><div result:id="staticPrefixed" /></div>)
    }

    "handle dynamic, prefixed attributes" in {
      BindHelpers.bind("test", 
                       <div><div test:x="dynamicPrefixed" /></div>,
                       FuncAttrBindParam("x", {ns : NodeSeq => ns}, ("result","id"))) must ==/(<div><div result:id="dynamicPrefixed" /></div>)
    }
  }
}
class BindHelpersTest extends JUnit4(BindHelpersSpec)

}
}
