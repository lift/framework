/*
 * Copyright 2010-2011 WorldWide Conferencing, LLC
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
package util

import scala.xml.quote._
import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification

import common._
import scala.xml._

import Helpers._

/**
 * Systems under specification for CSS Selector.
 */
object CssSelectorSpec extends Specification with XmlMatchers {
  "CSS Selector Specification".title

  "CssSelector" should {
    "fail for garbage input" in {
      CssSelectorParser.parse(" 49234e23").isDefined must_== false
    }

    "select an id" in {
      CssSelectorParser.parse("#foo").openOrThrowException("If the box is empty, we want a failure") must_== 
        IdSelector("foo", Empty)
    }

    "a selector with cruft at the end must fail" in {
      CssSelectorParser.parse("#foo I li**ke yaks").isDefined must_== false
    }

    ":yak must not parse" in {
      CssSelectorParser.parse(":yak").isDefined must_== false
    }

    ":button must  parse" in {
      CssSelectorParser.parse(":button").openOrThrowException("If the box is empty, we want a failure") must_== 
      AttrSelector("type", "button", Empty)
    }


    ":checkbox must  parse" in {
      CssSelectorParser.parse(":checkbox").openOrThrowException("If the box is empty, we want a failure") must_== 
      AttrSelector("type", "checkbox", Empty)
    }

    ":file must  parse" in {
      CssSelectorParser.parse(":file").openOrThrowException("If the box is empty, we want a failure") must_== 
      AttrSelector("type", "file", Empty)
    }

    ":password must  parse" in {
      CssSelectorParser.parse(":password").openOrThrowException("If the box is empty, we want a failure") must_== 
      AttrSelector("type", "password", Empty)
    }

    ":radio must  parse" in {
      CssSelectorParser.parse(":radio").openOrThrowException("If the box is empty, we want a failure") must_== 
      AttrSelector("type", "radio", Empty)
    }

    ":reset must  parse" in {
      CssSelectorParser.parse(":reset").openOrThrowException("If the box is empty, we want a failure") must_== 
      AttrSelector("type", "reset", Empty)
    }

    ":submit must  parse" in {
      CssSelectorParser.parse(":submit").openOrThrowException("If the box is empty, we want a failure") must_== 
      AttrSelector("type", "submit", Empty)
    }

    ":text must  parse" in {
      CssSelectorParser.parse(":text").openOrThrowException("If the box is empty, we want a failure") must_== 
      AttrSelector("type", "text", Empty)
    }

    "select an id with attr subnodes" in {
      CssSelectorParser.parse("#foo  *[dog] ").openOrThrowException("If the box is empty, we want a failure") must_== 
      IdSelector("foo", Full(AttrSubNode("dog")))
    }

    "select an id with no star attr subnodes" in {
      CssSelectorParser.parse("#foo  [woof] ").openOrThrowException("If the box is empty, we want a failure") must_== 
      IdSelector("foo", Full(AttrSubNode("woof")))
    }

    "select an id with attr append subnodes" in {
      CssSelectorParser.parse("#foo  *[dog+] ").openOrThrowException("If the box is empty, we want a failure") must_==
      IdSelector("foo", Full(AttrAppendSubNode("dog")))
    }

    "select an id with no star attr append subnodes" in {
      CssSelectorParser.parse("#foo  [woof+] ").openOrThrowException("If the box is empty, we want a failure") must_==
      IdSelector("foo", Full(AttrAppendSubNode("woof")))
    }

    "select an id with attr append subnodes" in {
      CssSelectorParser.parse("#foo  *[dog!] ").openOrThrowException("If the box is empty, we want a failure") must_==
      IdSelector("foo", Full(AttrRemoveSubNode("dog")))
    }

    "select an id with no star attr append subnodes" in {
      CssSelectorParser.parse("#foo  [woof!] ").openOrThrowException("If the box is empty, we want a failure") must_==
      IdSelector("foo", Full(AttrRemoveSubNode("woof")))
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

    "select name/val pair surround" in {
      CssSelectorParser.parse("@dog <*>") must_==
        Full(NameSelector("dog", Full(SurroundKids())))
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
      CssSelectorParser.parse(".foo").openOrThrowException("If the box is empty, we want a failure") must_== ClassSelector("foo", Empty)
    }

    "select a class with subnodes" in {
      CssSelectorParser.parse(".foo  * ").openOrThrowException("If the box is empty, we want a failure") must_== 
      ClassSelector("foo", Full(KidsSubNode()))
    }

    "Support selecting this node" in {
      CssSelectorParser.parse(".foo  ^^ ").openOrThrowException("If the box is empty, we want a failure") must_== 
      ClassSelector("foo", Full(SelectThisNode(false)))
    }

    "Support selecting this node" in {
      CssSelectorParser.parse(".foo  ^* ").openOrThrowException("If the box is empty, we want a failure") must_== 
      ClassSelector("foo", Full(SelectThisNode(true)))
    }

    "select a class with attr subnodes" in {
      CssSelectorParser.parse(".foo  *[dog] ").openOrThrowException("If the box is empty, we want a failure") must_== 
      ClassSelector("foo", Full(AttrSubNode("dog")))
    }

    "select an id with no star attr subnodes" in {
      CssSelectorParser.parse(".foo  [woof] ").openOrThrowException("If the box is empty, we want a failure") must_== 
      ClassSelector("foo", Full(AttrSubNode("woof")))
    }

    "select multiple depth" in {
      CssSelectorParser.parse("div .foo [woof] ").openOrThrowException("If the box is empty, we want a failure") must_==
        EnclosedSelector(ElemSelector("div", Empty), ClassSelector("foo", Full(AttrSubNode("woof"))))
    }

    "select multiple depth with star" in {
      CssSelectorParser.parse("div .foo * ").openOrThrowException("If the box is empty, we want a failure") must_==
        EnclosedSelector(ElemSelector("div", Empty), ClassSelector("foo", Full(KidsSubNode())))
    }

    "select multiple super depth with star" in {
      CssSelectorParser.parse("span div .foo * ").openOrThrowException("If the box is empty, we want a failure") must_==
        EnclosedSelector(ElemSelector("span", Empty), EnclosedSelector(ElemSelector("div", Empty), ClassSelector("foo", Full(KidsSubNode()))))
    }


  }

}

object CssBindHelpersSpec extends Specification with XmlMatchers {

  "css bind helpers" should {
    "clear clearable" in {
      ClearClearable(xml"""<b><span class="clearable"/></b>""") must ==/ (xml"<b/>")
    }

    "substitute a String by id" in {
      ("#foo" #> "hello").apply(xml"""<b><span id="foo"/></b>""") must ==/ (xml"<b>hello</b>")
    }


    "not duplicate classes" in {

      def anchor(quesType: String, value: String) = {
        xml"""<a href="foo" class="selected">(value)</a>"""
      }
      var page = 1
      var elements = List("1","2","3","4")

      val xml = xml"""<div class="lift:Bug.attack bug">
        <div id="question" class="question">
          <a href="#" class="L">1</a>
          <a href="#" class="U">1</a>
          <a href="#" class="D">1</a>
        </div>
        <div class="navigation">
          <button class="previous">Previous</button> <button class="next">Next</button>
        </div>
      </div>"""

      val sel = ".question" #> elements.map(value => {
        ".question [id]" #> ("question-" + value) &
          ".question [class]" #> ("question-" + value) &
          ".L" #> anchor("L", value) &
          ".U" #> anchor("U", value) &
          ".D" #> anchor("D", value)
      })

      val res = sel(xml)

      ((res \\ "a").head \ "@class").head.text must_== "selected L"
    }


    "Compound selector" in {
      val res =
        (".foo [href]" #> "http://dog.com" & ".bar [id]" #> "moo").apply(
            xml"""<a class="foo bar" href="#"/>""")
      (res \ "@href").text must_== "http://dog.com"
      (res \ "@id").text must_== "moo"
    }

    "not stack overflow on Elem" in {
      val xf = "* [id]" #> "xx" &
        "* [style]" #> "border:thin solid black" &
        "* *" #> xml"<a/>"
      success
    }

    "not stack overflow on Elem" in {
      val xf = "* [id]" #> "xx" &
        "* [style]" #> "border:thin solid black" &
        "* *+" #> xml"<a/>"

      xf(xml"<div/>")
      success
    }

    "not stack overflow on Elem" in {
      val xf = "* [id]" #> "xx" &
        "* [style]" #> "border:thin solid black" &
        "* -*" #> xml"<a/>"

      xf(xml"<div/>")
      success
    }

    "data-name selector works" in {
      val xf = ";frog" #> xml"<b>hi</b>"

      xf(xml"""<div><span data-name="frog">Moose</span></div>""") must ==/ (xml"""<div><b data-name="frog">hi</b></div>""")
    }

    "support modifying attributes along with body" in {
      val org = xml"<a>foo</a>"
      val func = "a [href]" #> "dog" & "a *" #> "bar"
      val res = func(org)

      res.toString must_== "<a href=\"dog\">bar</a>"
    }

    "substitute a String by id" in {
      ("#foo" replaceWith "hello").apply(xml"""<b><span id="foo"/></b>""") must ==/ (xml"<b>hello</b>")
    }

    "substitute a String by nested class" in {
      ("div .foo" #> "hello").apply(xml"""<b><div><span class="foo"/></div><span><span class="foo"/></span></b>""") must ==/ (xml"""<b><div>hello</div><span><span class="foo"/></span></b>""")
    }

    "substitute a String by deep nested class" in {
      ("#baz div .foo" #> "hello").apply(
        xml"""<b><span id="baz"><div><span class="foo"/></div></span><span><span class="foo"/></span></b>""") must ==/ (xml"""<b><span id="baz"><div>hello</div></span><span><span class="foo"/></span></b>""")
    }

    "insert a String by deep nested class" in {
      ("#baz div .foo *" #> "hello").apply(
        xml"""<b><span id="baz"><div><span class="foo"/></div></span><span><div><span class="foo"/></div></span></b>""") must ==/ (xml"""<b><span id="baz"><div><span class="foo">hello</span></div></span><span><div><span class="foo"/></div></span></b>""")
    }


    "Only apply to the top elem" in {
      val xf = "^ [href]" #> "wombat"

      xf(xml"<a><b>stuff</b></a>") must ==/ (xml"""<a href="wombat"><b>stuff</b></a>""")
    }



    "Select a node" in {
      ("#foo ^^" #> "hello").apply(xml"""<div><span id="foo"/></div>""") must ==/ (xml"""<span id="foo"/>""")
    }

    "Another nested select" in {
      val template = xml"""<span>
        <div id="meow">
          <lift:loc locid="asset.import.chooseFile"></lift:loc>
          <span id="file_upload"></span>
          <input type="submit" value="import" /><br></br>
        </div>
        <div id="get">
          <lift:loc locid="asset.import.chooseFile"></lift:loc>
          <span id="file_upload"></span>
          <input type="submit" value="import" /><br></br>
        </div>
      </span>"""

      val xf = "#get ^^" #> "ignore" & "#file_upload" #> xml"""<input type="moose"/>"""

      val ret = xf(template)

      ret(0).asInstanceOf[Elem].label must_== "div"
      ret.length must_== 1
      (ret \ "@id").text must_== "get"

      (ret \\ "input").length must_== 2

      ((ret \\ "input").toList(0) \ "@type").map(_.text) must_== List("moose")

    }

    "Child nested select" in {
      val template = xml"""<span>
        <div id="meow">
          <lift:loc locid="asset.import.chooseFile"></lift:loc>
          <span id="file_upload"></span>
          <input type="submit" value="import" /><br></br>
        </div>
        <div id="get">
          <lift:loc locid="asset.import.chooseFile"></lift:loc>
          <span id="file_upload"></span>
          <input type="submit" value="import" /><br></br>
        </div>
      </span>"""

      val xf = "#get ^*" #> "ignore" & "#file_upload" #> xml"""<input type="moose"/>"""

      val ret = xf(template)

      (ret \\ "div").length must_== 0

      (ret \\ "input").length must_== 2

      ((ret \\ "input").toList(0) \ "@type").map(_.text) must_== List("moose")

    }

    "Select a node and transform stuff" in {
      val ret = ("#foo ^^" #> "hello" &
        "span [id]" #> "bar")(xml"""<span id="foo"/>""")

      ret(0).asInstanceOf[Elem].label must_== "span"
      ret.length must_== 1
      (ret \ "@id").text must_== "bar"
    }


    "Select a node and transform stuff deeply nested" in {
      val ret = ("#foo ^^" #> "hello" &
        "span [id]" #> "bar")(xml"""<div><div><span id="foo"/></div></div>""")

      ret(0).asInstanceOf[Elem].label must_== "span"
      ret.length must_== 1
      (ret \ "@id").text must_== "bar"
    }


    "Select a node and transform stuff deeply nested 2" in {
      val ret = ("#foo ^^" #> "hello" &
        "span [id]" #> "bar")(xml"""<div><div><span id="foo2"/><span id="foo3"/><span dog="woof" id="foo"/></div></div>""")

      ret(0).asInstanceOf[Elem].label must_== "span"
      ret.length must_== 1
      (ret \ "@id").text must_== "bar"
      (ret \ "@dog").text must_== "woof"
    }



    "substitute multiple Strings by id" in {
      ("#foo" #> "hello" &
        "#baz" #> "bye"
        )(xml"""<b><div id="baz">Hello</div><span id="foo"/></b>""") must be_== (NodeSeq fromSeq xml"<b>${Text("bye")}${Text("hello")}</b>")
    }

    "bind href and None content" in {
      val opt: Option[String] = None
      val res = ("top *" #> opt &
        "top [href]" #> "frog")(xml"<top>cat</top>")

      res.text must_== ""
      (res \ "@href").text.mkString must_== "frog"
    }

    "bind href and Some content" in {
      val opt: Option[String] = Some("Dog")
      val res = ("top *" #> opt &
        "top [href]" #> "frog")(xml"<top>cat</top>")

      res.text must_== "Dog"
      (res \ "@href").text.mkString must_== "frog"
    }

    "bind href and Some content with multiple attrs" in {
      val opt: Option[String] = Some("Dog")
      val res = ("top *" #> opt &
        "top [meow]" #> "woof" &
        "top [href]" #> "frog")(xml"""<top href="#">cat</top>""")

      res.text must_== "Dog"
      (res \ "@href").text.mkString must_== "frog"
      (res \ "@meow").text.mkString must_== "woof"
    }

    "option transform on *" in {
      val opt: Option[String] = None
      val res = ("* *" #> opt.map(ignore => "Dog")).apply(xml"<top>cat</top>")
      res.head must_== xml"<top></top>"
    }

    "append attribute to a class with spaces" in {
      val stuff = List("a", "b")
      val res = ("* [class+]" #> stuff).apply(xml"""<top class="q">cat</top>""")
      (res \ "@class").text must_== "q a b"
    }

    "append attribute to an href" in {
      val stuff = List("&a=b", "&b=d")
      val res = ("* [href+]" #> stuff).apply(xml"""<top href="q?z=r">cat</top>""")
      (res \ "@href").text must_== "q?z=r&a=b&b=d"
    }

    "remove an attribute from a class" in {
      val func = ".foo [class!]" #> "andOther"

      (func(xml"""<span class="foo andOther" />""") \ "@class").text must_== "foo"
    }

    "remove an attribute from a class and the attribute if it's the only one left" in {
      val func = ".foo [class!]" #> "foo"
      val res = func(xml"""<span class="foo" />""")

      (res \ "@class").length must_== 0
    }



    "Remove a subnode's class attribute" in {

      val func = ".removeme !!" #> ("td [class!]" #> "removeme")
      val res = func.apply(xml"""<tr><td class="removeme fish">Hi</td></tr>""")

      ((res \ "td") \ "@class").text must_== "fish"
    }


    "not remove a non-existant class" in {
      val func = ".foo [class!]" #> "bar"
      val res = func(xml"""<span class="foo" />""")

      (res \ "@class").text must_== "foo"
    }


    "remove an attribute from an attribute" in {
      val func = "span [href!]" #> "foo"
      val res = func(xml"""<span href="foo" />""")

      (res \ "@href").length must_== 0
    }


    "not remove a non-existant href" in {
      val func = "span [href!]" #> "bar"
      val res = func(xml"""<span href="foo bar" />""")

      (res \ "@href").text must_== "foo bar"
    }

    "option transform on *" in {
      val opt: Option[Int] = Full(44)
      val res = ("* *" #> opt.map(ignore => "Dog")).apply(xml"<top>cat</top>")
      res must ==/ (xml"<top>Dog</top>")
    }


    "Java number support" in {
      val f = "a *" #> Full(new java.lang.Long(12))
      val xml = xml"<a>Hello</a>"

      f(xml) must ==/ (xml"<a>12</a>")
    }


    "Surround kids" in {
      val f = "a <*>" #> xml"<div></div>"
      val xml = xml"""<b>Meow <a href="dog">Cat</a> woof</b>"""

      f(xml) must ==/ (xml"""<b>Meow <a href="dog"><div>Cat</div></a> woof</b>""")
    }

    "Andreas's thing doesn't blow up" in {
      def cachedMessageList: Box[Box[String]] = Empty

      def messageListId = "Hello"

      def collapseUnless[A](isEmptyCond: Boolean)(f: => A): Box[A] = {
        if (!isEmptyCond) {
          Empty
        } else {
          Full(f)
        }
      }

      ".noMail" #> collapseUnless(cachedMessageList.map(_.isEmpty).openOr(true)) {
        "tbody [id]" #> messageListId &
          "*" #> PassThru
      }

      true must_== true
    }

    "other Andreas test" in {
      def renderBlogEntrySummary = {
        ".blogEntry" #> ((ns: NodeSeq) => {
          ("*" #> "Horse").apply(ns)
        })
      }



      def render = {

        "*" #> ((ns: NodeSeq) =>
          renderBlogEntrySummary.apply(ns) ++ xml"<a>hi</a>"
          )
      }

      render

      true must_== true
    }


    "option transform on *" in {
      val opt: Box[String] = Empty
      val res = ("* *" #> opt.map(ignore => "Dog")).apply(xml"<top>cat</top>")
      res.head must_== xml"<top></top>"
    }

    "option transform on *" in {
      val opt: Box[Int] = Some(44)
      val res = ("* *" #> opt.map(ignore => "Dog")).apply(xml"<top>cat</top>")
      res must ==/ (xml"<top>Dog</top>")
    }

    "transform on *" in {
      val res = ("* *" #> "Dog").apply(xml"<top>cat</top>")
      res must ==/ (xml"<top>Dog</top>")
    }

    "transform child content on *+" in {
      val res = ("* *+" #> "moose").apply(xml"<a>I like </a>")
      res.text must_== "I like moose"
    }

    "transform child content on -*" in {
      val res = ("* -*" #> "moose").apply(xml"<a> I like</a>")
      res.text must_== "moose I like"
    }

    "transform on li" in {
      val res = ("li *" #> List("Woof", "Bark") & ClearClearable)(
        xml"""<ul><li>meow</li><li class="clearable">a</li><li class="clearable">a</li></ul>""")
      res must ==/ (xml"<ul><li>Woof</li><li>Bark</li></ul>")
    }

    "substitute multiple Strings by id" in {
      (("#foo" replaceWith "hello") &
        ("#baz" replaceWith "bye")
        )(
        xml"""<b><div id="baz">Hello</div><span id="foo"/></b>"""
      ) must_== (NodeSeq fromSeq xml"<b>${Text("bye")}${Text("hello")}</b>")
    }

    "substitute multiple Strings with a List by id" in {
      ("#foo" #> "hello" &
        "#baz" #> List("bye", "bye"))(xml"""<b><div id="baz">Hello</div><span id="foo"/></b>""") must_== (NodeSeq fromSeq xml"<b>${Text("bye")}${Text("bye")}${Text("hello")}</b>")
    }

    "substitute multiple Strings with a List by id" in {
      (("#foo" replaceWith "hello") &
        ("#baz" replaceWith List("bye", "bye")))(xml"""<b><div id="baz">Hello</div><span id="foo"/></b>""") must_== (NodeSeq fromSeq xml"<b>${Text("bye")}${Text("bye")}${Text("hello")}</b>")
    }


    "substitute multiple Strings with a List of XML by id" in {
      val answer = ("#foo" #> "hello" &
        "#baz" #> List[NodeSeq](xml"<i/>", xml"<i>Meow</i>"))(xml"""<b><div frog="dog" id="baz">Hello</div><span id="foo"/></b>""")

      (answer \ "i").length must_== 2
      (answer \ "i")(0) must ==/ (xml"""<i id="baz" frog="dog"/>""")
      (answer \ "i")(1) must ==/ (xml"""<i frog="dog">Meow</i>""")
    }

    "substitute multiple Strings with a List of XML by id" in {
      val answer = (("#foo" replaceWith "hello") &
        ("#baz" replaceWith List[NodeSeq](xml"<i/>", xml"<i>Meow</i>")))(xml"""<b><div frog="dog" id="baz">Hello</div><span id="foo"/></b>""")

      (answer \ "i").length must_== 2
      (answer \ "i")(0) must ==/ (xml"""<i id="baz" frog="dog"/>""")
      (answer \ "i")(1) must ==/ (xml"""<i frog="dog">Meow</i>""")
    }

    "substitute by name" in {
      val answer = ("name=moose" #> xml"""<input name="goof"/>""").apply (
        xml"""<div><input name="moose" value="start" id="79"/></div>""")

      (answer \ "input")(0) must ==/ (xml"""<input name="goof" value="start" id="79"/>""")
    }


    "Deal with NodeSeq as a NodeSeq" in {
      val f = "h6 *" #> ((Text("Some awesome ") ++ xml"<strong>text</strong>" ++ Text(" here.")): NodeSeq)
      val xml = xml"<h6>Dude, where's my car?</h6>"

      val res = f(xml)
      res must ==/ (xml"<h6>Some awesome <strong>text</strong> here.</h6>")
    }

    "substitute by name" in {
      val answer = ("name=moose" replaceWith xml"""<input name="goof"/>""").apply (
        xml"""<div><input name="moose" value="start" id="79"/></div>""")

      (answer \ "input")(0) must ==/ (xml"""<input name="goof" value="start" id="79"/>""")
    }


    "substitute by name with attrs" in {
      val answer = ("name=moose" #> xml"""<input name="goof" value="8" id="88"/>""").apply (
        xml"""<div><input name="moose" value="start" id="79"/></div>""")

      (answer \ "input")(0) must ==/ (xml"""<input name="goof" value="8" id="88"/>""")
    }

    "substitute by name with attrs" in {
      val answer = ("name=moose" replaceWith xml"""<input name="goof" value="8" id="88"/>""").apply (
        xml"""<div><input name="moose" value="start" id="79"/></div>""")

      (answer \ "input")(0) must ==/ (xml"""<input name="goof" value="8" id="88"/>""")
    }


    "substitute by a selector with attrs" in {
      val answer = ("cute=moose" #> xml"""<input name="goof" value="8" id="88"/>""").apply (
        xml"""<div><input name="meow" cute="moose" value="start" id="79"/></div>""")

      (answer \ "input")(0) must ==/ (xml"""<input cute="moose" name="goof" value="8" id="88"/>""")
    }

    "substitute by a selector with attrs" in {
      val answer = ("cute=moose" replaceWith xml"""<input name="goof" value="8" id="88"/>""").apply (
        xml"""<div><input name="meow" cute="moose" value="start" id="79"/></div>""")

      (answer \ "input")(0) must ==/ (xml"""<input cute="moose" name="goof" value="8" id="88"/>""")
    }

    "Map of funcs" in {
      val func: NodeSeq => NodeSeq = "#horse" #> List(1,2,3).map(".item *" #> _)
      val answer: NodeSeq = func(xml"""<span><div id="horse">frog<span class="item">i</span></div></span>""")

      answer must ==/ (xml"""<span><div id="horse">frog<span class="item">1</span></div><div>frog<span class="item">2</span></div><div>frog<span class="item">3</span></div></span>""")

    }

    "maintain unique id attributes provided by transform" in {
      val func = ".thinglist *" #>
        (".thing" #> List("xx1", "xx2", "xx2", "xx2", "xx4").map(t => {
          ".thing [id]" #> t
        })
          )
      val answer = func(xml"""<ul class="thinglist"><li id="other" class="thing" /></ul>""")

      answer must ==/ (xml"""<ul class="thinglist"><li class="thing" id="xx1"></li><li class="thing" id="xx2"></li><li id="other" class="thing"></li><li class="thing"></li><li class="thing" id="xx4"></li></ul>""")
    }

    "merge classes" in {
      val answer = ("cute=moose" #> xml"""<input class="a" name="goof" value="8" id="88"/>""").apply (
        xml"""<div><input name="meow" class="b" cute="moose" value="start" id="79"/></div>""")

      (answer \ "input")(0) must ==/ (xml"""<input class="a b" cute="moose" name="goof" value="8" id="88"/>""")
    }


    "merge classes" in {
      val answer = ("cute=moose" replaceWith xml"""<input class="a" name="goof" value="8" id="88"/>""").apply (
        xml"""<div><input name="meow" class="b" cute="moose" value="start" id="79"/></div>""")

      (answer \ "input")(0) must ==/ (xml"""<input class="a b" cute="moose" name="goof" value="8" id="88"/>""")
    }




    "list of strings" in {
      val answer = ("#moose *" #> List("a", "b", "c", "woof") &
        ClearClearable).apply (
        xml"""<ul>
          <li id="moose">first</li>
          <li class="clearable">second</li>
          <li class="clearable">Third</li>
        </ul>""")

      val lis = (answer \ "li").toList

      lis.length must_== 4

      lis(0) must ==/ (xml"""<li id="moose">a</li>""")
      lis(3) must ==/ (xml"<li>woof</li>")
    }


    "list of Nodes" in {
      val answer = ("#moose *" #> List[NodeSeq](xml"""<i>"a"</i>""", Text("b"), Text("c"), xml"<b>woof</b>") &
        ClearClearable).apply (
        xml"""<ul>
          <li id="moose">first</li>
          <li class="clearable">second</li>
          <li class="clearable">Third</li>
        </ul>""")

      val lis = (answer \ "li").toList

      lis.length must_== 4

      lis(0) must ==/ (xml"""<li id="moose"><i>"a"</i></li>""")
      lis(3) must ==/ (xml"<li><b>woof</b></li>")
    }


    "set href" in {
      val answer = ("#moose [href]" #> "Hi" &
        ClearClearable).apply (
        xml"""<ul><a id="moose" href="meow">first</a><li class="clearable">second</li><li class="clearable">Third</li></ul>""")


      (answer \ "a" \ "@href").text must_== "Hi"
      (answer \ "li").length must_== 0
    }

    "set href and subnodes" in {
      val answer = ("#moose [href]" #> "Hi" &
        ClearClearable).apply (
        xml"""<ul><a id="moose" href="meow">first<li class="clearable">second</li><li class="clearable">Third</li></a></ul>""")


      (answer \ "a" \ "@href").text must_== "Hi"
      (answer \\ "li").length must_== 0
    }


    "list of strings" in {
      val answer = (("#moose *" replaceWith List("a", "b", "c", "woof")) &
        ClearClearable).apply (
        xml"""<ul>
          <li id="moose">first</li>
          <li class="clearable">second</li>
          <li class="clearable">Third</li>
        </ul>""")

      val lis = (answer \ "li").toList

      lis.length must_== 4

      lis(0) must ==/ (xml"""<li id="moose">a</li>""")
      lis(3) must ==/ (xml"<li>woof</li>")
    }

    "bind must bind to subnodes" in {
      val html = xml"""<ul class="users">
        <li class="user" userid="">
          <img class="userimg" src=""/>
        </li>
      </ul>"""

      val lst = List(1,2,3)

      val f = ".users *" #> ("li" #> lst.map(i => ".user [userid]" #> i))

      (f(html) \\ "ul").length must_== 1
      (f(html) \\ "li").length must_== 3
    }

    "list of Nodes" in {
      val answer = (("#moose *" replaceWith List[NodeSeq](xml"""<i>"a"</i>""", Text("b"), Text("c"), xml"<b>woof</b>")) &
        ClearClearable).apply (
        xml"""<ul>
          <li id="moose">first</li>
          <li class="clearable">second</li>
          <li class="clearable">Third</li>
        </ul>""")

      val lis = (answer \ "li").toList

      lis.length must_== 4

      lis(0) must ==/ (xml"""<li id="moose"><i>"a"</i></li>""")
      lis(3) must ==/ (xml"<li><b>woof</b></li>")
    }


    "set href" in {
      val answer = (("#moose [href]" replaceWith "Hi") &
        ClearClearable).apply (
        xml"""<ul><a id="moose" href="meow">first</a><li class="clearable">second</li><li class="clearable">Third</li></ul>""")


      (answer \ "a" \ "@href").text must_== "Hi"
      (answer \ "li").length must_== 0
    }

    "set href and subnodes" in {
      val answer = (("#moose [href]" replaceWith "Hi") &
        ClearClearable).apply (
        xml"""<ul><a id="moose" href="meow">first<li class="clearable">second</li><li class="clearable">Third</li></a></ul>""")


      (answer \ "a" \ "@href").text must_== "Hi"
      (answer \\ "li").length must_== 0
    }


  }
}


/**
 * This class doesn't actually perform any tests, but insures that
 * the implicit conversions work correctly
 */
object CheckTheImplicitConversionsForToCssBindPromoter {
  val bog = new CssBindPromoter(Empty, Empty)

  "foo" #> "baz"

  bog #> "Hello"
  bog #> xml"<span/>"
  bog #> 1
  bog #> 'foo
  bog #> 44L
  bog #> 1.22
  bog #> false

  bog #> List(xml"<span/>")
  bog #> Full(xml"<span/>")
  val e: Box[String] = Empty
  bog #> e
  bog #> Some(xml"<span/>")
  val n: Option[String] = None
  bog #> n


  bog #> List("Hello")
  bog #> List(1.22)
  bog #> List(44L)
  bog #> List(1)
  bog #> Full("Dog")
  bog #> Some("Moo")


  bog #> List((null: Bindable))
  bog #> Full((null: Bindable))
  bog #> Some((null: Bindable))

  bog #> nsToNs _
  bog #> nsToOptNs _
  bog #> nsToBoxNs _
  bog #> nsToSeqNs _

  bog #> nsToString _
  bog #> nsToOptString _
  bog #> nsToBoxString _
  bog #> nsToSeqString _

  val nsf: NodeSeq => NodeSeq = bog #> "Hello" &
    bog #> xml"<span/>" &
    bog #> 1 &
    bog #> 'foo &
    bog #> 44L &
    bog #> false

  "foo" #> "Hello"
  "foo" #> xml"<span/>"
  "foo" #> 1
  "foo" #> 'foo
  "foo" #> 44L
  "foo" #> false

  "foo" #> List(xml"<span/>")
  "foo" #> Full(xml"<span/>")
  "foo" #> Some(xml"<span/>")


  "foo" #> List("Hello")
  "foo" #> Full("Dog")
  "foo" #> Some("Moo")


  "foo" #> List((null: Bindable))
  "foo" #> Full((null: Bindable))
  "foo" #> Some((null: Bindable))

  "foo" #> nsToNs _
  "foo" #> nsToOptNs _
  "foo" #> nsToBoxNs _
  "foo" #> nsToSeqNs _

  "foo" #> nsToString _
  "foo" #> nsToOptString _
  "foo" #> nsToBoxString _
  "foo" #> nsToSeqString _

  "#foo" #> Set("a", "b", "c")

  val nsf2: NodeSeq => NodeSeq = "foo" #> "Hello" &
    "foo" #> xml"<span/>" &
    "foo" #> 1 &
    "foo" #> 'foo &
    "foo" #> 44L &
    "foo" #> false

  "bar" #> List("1","2","3").map(s => "baz" #> s)

  "bar" #> Full(1).map(s => ("baz" #> s): CssBindFunc)
  "bar" #> Some(1).map(s => ("baz" #> s): CssBindFunc)



  def nsToNs(in: NodeSeq): NodeSeq = in
  def nsToOptNs(in: NodeSeq): Option[NodeSeq] = Some(in)
  def nsToBoxNs(in: NodeSeq): Box[NodeSeq] = Full(in)
  def nsToSeqNs(in: NodeSeq): Seq[NodeSeq] = List(in)

  def nsToString(in: NodeSeq): String = in.text
  def nsToOptString(in: NodeSeq): Option[String] = Some(in.text)
  def nsToBoxString(in: NodeSeq): Box[String] = Full(in.text)
  def nsToSeqString(in: NodeSeq): Seq[String] = List(in.text)
}
