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
package http

import scala.xml._
import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification

import common._
import util.Helpers._


/**
 * System under specification for SnippetSpec.
 */
class SnippetSpec extends Specification with XmlMatchers {
  "SnippetSpec Specification".title

  def makeReq = Full(new Req(Req.NilPath, "", GetRequest, Empty, null,
                    System.nanoTime, System.nanoTime, false,
                    () => ParamCalcInfo(Nil, Map.empty, Nil, Empty), Map()))

  "Templates" should {
    "Correctly process lift:content_id" in {
      val ret = Templates.checkForContentId(<html lift:content_id="content">
                                     <head/>
                                     <body>
                                     <div id="content" class="lift:surround"/>
                                     </body>
                                     </html>)

      ret must ==/ (<div id="content" class="lift:surround"/>)
    }

    "Correctly process body class" in {
      val ret = Templates.checkForContentId(<html>
                                     <head/>
                                     <body class="lift:content_id=frog">
                                      <div><span> mooose dog
                                     <div id="frog" class="lift:surround"/>
                                      </span></div>
                                     </body>
                                     </html>)

      ret must ==/ (<div id="frog" class="lift:surround"/>)
    }

    "Correctly process l:content_id" in {
      val ret = Templates.checkForContentId(<html l:content_id="dog">
                                     <head/>
                                     <body>
                                     <lift:surround id="dog"><div/></lift:surround>
                                     </body>
                                     </html>)

      ret must ==/ (<lift:surround id="dog"><div/></lift:surround>)
    }

    "Correctly process not lift:designer_friendly" in {
      val xml = <html>
      <head/>
      <body>
      <div class="lift:surround"/>
      </body>
      </html>

      val ret = Templates.checkForContentId(xml)

      ret must ==/(xml)
    }

    "Snippet invocation works <lift:xxx/>" in {
      val res = <div/>

      val ret =
        S.statelessInit(Req.nil) {
          S.mapSnippetsWith("foo" -> ((a: NodeSeq) => a)) {
            for {
              s <- S.session
            } yield s.processSurroundAndInclude("test", <lift:foo>{res}</lift:foo>)
          }
        }

      ret.openOrThrowException("legacy code") must ==/( res)
    }


    "Snippet invocation works <l:xxx/>" in {
      val res = <div/>

      val ret =
        S.statelessInit(Req.nil) {
          S.mapSnippetsWith("foo" -> ((a: NodeSeq) => a)) {
            for {
              s <- S.session
            } yield s.processSurroundAndInclude("test", <l:foo>{res}</l:foo>)
          }
        }

      ret.openOrThrowException("legacy code") must ==/( res)
    }

    "Snippet invocation works class='l:foo'" in {
      val res = <div/>

      val ret =
        S.statelessInit(Req.nil) {
          S.mapSnippetsWith("foo" -> ((a: NodeSeq) => a)) {
            for {
              s <- S.session
            } yield s.processSurroundAndInclude("test", <div class="l:foo" />)
          }
        }

      ret.openOrThrowException("legacy code") must ==/( res)
    }

    "Snippet invocation works class='l:foo' and ? for attr sep" in {
      val res = <div/>

      def testAttrs(in: NodeSeq): NodeSeq = {
        S.attr("bing") === Full("bong")
        S.attr("fuzz") === Full("faz snark")
        S.attr("noodle") === Full("FatPoodle")
        in
      }

      val ret =
        S.statelessInit(Req.nil) {
          S.mapSnippetsWith("foo" -> testAttrs _) {
            for {
              s <- S.session
            } yield s.processSurroundAndInclude("test", <div class="l:foo?bing=bong?fuzz=faz+snark?noodle=FatPoodle" />)
          }
        }

      ret.openOrThrowException("legacy code") must ==/( res)
    }


    "Snippet invocation works class='l:foo' and ; for attr sep" in {
      val res = <div/>

      def testAttrs(in: NodeSeq): NodeSeq = {
        S.attr("bing") === Full("bong")
        S.attr("fuzz") === Full("faz snark")
        S.attr("noodle") === Full("FatPoodle")
        in
      }

      val ret =
        S.statelessInit(Req.nil) {
          S.mapSnippetsWith("foo" -> testAttrs _) {
            for {
              s <- S.session
            } yield s.processSurroundAndInclude("test", <div class="l:foo?bing=bong;fuzz=faz+snark;noodle=FatPoodle" />)
          }
        }

      ret.openOrThrowException("legacy code") must ==/( res)
    }


    "Snippet invocation works class='l:foo' and & for attr sep" in {
      val res = <div/>

      def testAttrs(in: NodeSeq): NodeSeq = {
        S.attr("bing") === Full("bong")
        S.attr("fuzz") === Full("faz snark")
        S.attr("noodle") === Full("FatPoodle")
        in
      }

      val ret =
        S.statelessInit(Req.nil) {
          S.mapSnippetsWith("foo" -> testAttrs _) {
            val clStr = "l:foo?bing=bong&amp;fuzz=faz+snark&amp;noodle=FatPoodle"
            for {
              s <- S.session
            } yield s.processSurroundAndInclude("test", <div class={clStr} />)
          }
        }

      ret.openOrThrowException("legacy code") must ==/( res)
    }


    "Snippet invocation works class='l:foo' and mixed attr sep" in {
      val res = <div/>

      def testAttrs(in: NodeSeq): NodeSeq = {
        S.attr("bing") === Full("bong")
        S.attr("fuzz") === Full("faz snark")
        S.attr("noodle") === Full("FatPoodle")
        in
      }

      val ret =
        S.statelessInit(Req.nil) {
          S.mapSnippetsWith("foo" -> testAttrs _) {
            for {
              s <- S.session
            } yield s.processSurroundAndInclude("test", <div class="l:foo?bing=bong?fuzz=faz+snark;noodle=FatPoodle" />)
          }
        }

      ret.openOrThrowException("legacy code") must ==/( res)
    }



    "Snippet invocation works class='lift:foo'" in {
      val res = <div/>

      val ret =
        S.statelessInit(Req.nil) {
          S.mapSnippetsWith("foo" -> ((a: NodeSeq) => a)) {
            for {
              s <- S.session
            } yield s.processSurroundAndInclude("test", <div class='lift:foo' />)
          }
        }

      ret.openOrThrowException("legacy code") must ==/( res)
    }

    "Snippet invocation fails class='l:bar'" in {
      val ret =
        S.statelessInit(Req.nil) {
          S.mapSnippetsWith("foo" -> ((a: NodeSeq) => a)) {
            for {
              s <- S.session
            } yield s.processSurroundAndInclude("test", <div class="lift:bar" />)
          }
        }

      (ret.openOrThrowException("legacy code") \ "@class").text === "snippeterror"
    }

    object myInfo extends SessionVar("")

    object ChangeVar {
      def foo(in: NodeSeq): NodeSeq = {
        myInfo.set(in.text)
        in
      }
    }

    object Funky {
      def foo(in: NodeSeq): NodeSeq = {
        SHtml.text("", s => myInfo.set(s))
        in
      }
    }

    "Snippet invocation fails in stateless mode" in {
      val res = <div>dog</div>

      val ret =
        S.statelessInit(Req.nil) {
          S.mapSnippetsWith("foo" -> ChangeVar.foo _) {
            for {
              s <- S.session
            } yield s.processSurroundAndInclude("test",
                                                <lift:foo>{res}</lift:foo>)
          }
        }

      (ret.openOrThrowException("legacy code") \ "@class").text === "snippeterror"
    }

    "Snippet invocation succeeds in normal mode" in {
      val res = <div>dog</div>
      val session = new LiftSession("", "hello", Empty)

      val ret =
        S.init(makeReq, session) {
          S.mapSnippetsWith("foo" -> ChangeVar.foo _) {
            for {
              s <- S.session
            } yield s.processSurroundAndInclude("test",
                                                <lift:foo>{res}</lift:foo>)
          }
        }

      ret.openOrThrowException("legacy code") must ==/(res)
    }

    "Snippet invocation fails in stateless mode (function table)" in {
      val res = <div>dog</div>

      val ret =
        S.statelessInit(Req.nil) {
          S.mapSnippetsWith("foo" -> Funky.foo _) {
            for {
              s <- S.session
            } yield s.processSurroundAndInclude("test",
                                                <lift:foo>{res}</lift:foo>)
          }
        }

      (ret.openOrThrowException("legacy code") \ "@class").text === "snippeterror"
    }

    "Snippet invocation succeeds in normal mode (function table)" in {
      val res = <div>dog</div>
      val session = new LiftSession("", "hello", Empty)

      val ret =
        S.init(makeReq, session) {
          S.mapSnippetsWith("foo" -> Funky.foo _) {
            for {
              s <- S.session
            } yield s.processSurroundAndInclude("test",
                                                <lift:foo>{res}</lift:foo>)
          }
        }

      ret.openOrThrowException("legacy code") must ==/(res)
    }

    "run string input" in {
      val session = new LiftSession("", "hello", Empty)

      S.init(makeReq, session) {
        val ret = SHtml.onSubmit(s => ())(<input/>)

        ret.size === 1
        (ret \ "@name").text.length must beGreaterThan(0)
      }
    }

    "run string checkbox must have hidden element" in {
      val session = new LiftSession("", "hello", Empty)

      S.init(makeReq, session) {
        val ret = SHtml.onSubmitBoolean(s => ())(<input type="checkbox"/>)

        ret.size === 2
        (ret \\ "input" ).flatMap(_ \ "@name").map(_.text).mkString.length must beGreaterThan(0)
      }
    }

    "Check snippets as Function1[NodeSeq, NodeSeq]" in {
      /* FIXME SBT: Very very inconsistent in the build environment, pendingUntilFixed misreports
      val session = new LiftSession("", "hello", Empty)

      val ret = S.init(makeReq, session) {
        for {
          s <- S.session
        } yield s.processSurroundAndInclude("test",
                                            <lift:Meower>Moo</lift:Meower>)
      }

      ret.openOrThrowException("legacy code") must ==/ (<yak/>)
      */
      pending
    }

    "Check snippets via run" in {
      /* FIXME SBT: Very very inconsistent in the build environment, pendingUntilFixed misreports
      val session = new LiftSession("", "hello", Empty)

      val ret = S.init(makeReq, session) {
        for {
          s <- S.session
        } yield s.processSurroundAndInclude("test",
                                            <input class="lift:Splunker"/>)
      }

      (ret.openOrThrowException("legacy code") \ "@name").text.length must beGreaterThan(0)
      */
      pending
    }



    "Eager Eval works" in {
      val session = new LiftSession("", "hello", Empty)

      S.init(makeReq, session) {
        S.mapSnippetsWith("foo" -> ChangeVar.foo _) {
          for {
            s <- S.session
          } yield s.processSurroundAndInclude("test",
                                              <div class="l:foo?eager_eval=true">a<lift:foo>b</lift:foo></div>)
        }
        myInfo.is === "ab"
      }
    }
  }

  "Snippet attributes" should {
    "properly reflect the full snippet stack with S.attrs" in {
      S.initIfUninitted(new LiftSession("", "", Empty)) {
        S.withAttrs(new UnprefixedAttribute("a", "a", Null)) {
          S.withAttrs(new UnprefixedAttribute("b", "b", new UnprefixedAttribute("c", "c", Null))) {
            S.withAttrs(new UnprefixedAttribute("d", "d", Null)) {
              S.attr("a") === Full("a")
              S.attr("b") === Full("b")
              S.attr("c") === Full("c")
              S.attr("d") === Full("d")

              // Also check currentAttrs (should be set only for the inner-most call)
              S.currentAttr("a") === Empty
              S.currentAttr("b") === Empty
              S.currentAttr("c") === Empty
              S.currentAttr("d") === Full("d")
            }
            // Make sure the stack is unwound
            S.attr("d") === Empty
          }
          S.attr("b") === Empty
          S.attr("c") === Empty
        }
        S.attr("a") === Empty
        S.attrs === Nil
        S.currentAttrs === Null
      }
    }

    "reflect only the last pushed values with S.currentAttrs" in {
      S.initIfUninitted(new LiftSession("", "", Empty)) {
        S.withAttrs(new UnprefixedAttribute("a", "a", Null)) {
          S.withAttrs(new UnprefixedAttribute("b", "b", new UnprefixedAttribute("c", "c", Null))) {
            S.withAttrs(new UnprefixedAttribute("d", "d", Null)) {
              S.currentAttr("a") === Empty
              S.currentAttr("b") === Empty
              S.currentAttr("c") === Empty
              S.currentAttr("d") === Full("d")
            }
            // Verify currentAttr only sees "b" and "c" at this level
            S.currentAttr("d") === Empty
          }
          // Verify currentAttr only sees "a" at this level
          S.currentAttr("b") === Empty
          S.currentAttr("c") === Empty
        }
        // Verify we've unwound completely
        S.currentAttrs === Null
      }
    }

    "handle prefixes" in {
      S.initIfUninitted(new LiftSession("", "", Empty)) {
        S.withAttrs(new PrefixedAttribute("foo", "a", "a", Null)) {
          S.withAttrs(new PrefixedAttribute("foo", "b", "b", Null)) {
            S.withAttrs(new PrefixedAttribute("bar", "a", "aBar", Null)) {
              S.attr("a") === Empty
              S.attr("foo", "a") === Full("a")

              S.attr("bar", "a") === Full("aBar")

              val fooMap = S.prefixedAttrsToMap("foo", Map())

              fooMap("a") === "a"
              fooMap("b") === "b"

              val barMap = S.prefixedAttrsToMap("bar")

              barMap("a") === "aBar"
              barMap.get("b") === None
            }
          }
        }
      }
    }
  }
}

