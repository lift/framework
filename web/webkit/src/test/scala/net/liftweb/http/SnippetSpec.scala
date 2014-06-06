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

import xml._
import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification

import common._
import util.Helpers._


/**
 * System under specification for SnippetSpec.
 */
object SnippetSpec extends Specification with XmlMatchers {
  "SnippetSpec Specification".title

  def makeReq = new Req(Req.NilPath, "", GetRequest, Empty, null,
                    System.nanoTime, System.nanoTime, false,
                    () => ParamCalcInfo(Nil, Map.empty, Nil, Empty), Map())

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

      ret must_== xml
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
        S.attr("bing") must_== Full("bong")
        S.attr("fuzz") must_== Full("faz snark")
        S.attr("noodle") must_== Full("FatPoodle")
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
        S.attr("bing") must_== Full("bong")
        S.attr("fuzz") must_== Full("faz snark")
        S.attr("noodle") must_== Full("FatPoodle")
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
        S.attr("bing") must_== Full("bong")
        S.attr("fuzz") must_== Full("faz snark")
        S.attr("noodle") must_== Full("FatPoodle")
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
        S.attr("bing") must_== Full("bong")
        S.attr("fuzz") must_== Full("faz snark")
        S.attr("noodle") must_== Full("FatPoodle")
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

      (ret.openOrThrowException("legacy code") \ "@class").text must_== "snippeterror"
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

      (ret.openOrThrowException("legacy code") \ "@class").text must_== "snippeterror"
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

      (ret.openOrThrowException("legacy code") \ "@class").text must_== "snippeterror"
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

        ret.size must_== 1
        (ret \ "@name").text.length must be > 0
      }
    }

    "run string checkbox must have hidden element" in {
      val session = new LiftSession("", "hello", Empty)

      S.init(makeReq, session) {
        val ret = SHtml.onSubmitBoolean(s => ())(<input type="checkbox"/>)

        ret.size must_== 2
        (ret \\ "input" ).flatMap(_ \ "@name").map(_.text).mkString.length must be > 0
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

      (ret.openOrThrowException("legacy code") \ "@name").text.length must be > 0
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
        myInfo.is must_== "ab"
      }
    }
  }

  "Snippet attributes" should {
    "properly reflect the full snippet stack with S.attrs" in {
      S.initIfUninitted(new LiftSession("", "", Empty)) {
        S.withAttrs(new UnprefixedAttribute("a", "a", Null)) {
          S.withAttrs(new UnprefixedAttribute("b", "b", new UnprefixedAttribute("c", "c", Null))) {
            S.withAttrs(new UnprefixedAttribute("d", "d", Null)) {
              S.attr("a") must_== Full("a")
              S.attr("b") must_== Full("b")
              S.attr("c") must_== Full("c")
              S.attr("d") must_== Full("d")

              // Also check currentAttrs (should be set only for the inner-most call)
              S.currentAttr("a") must_== Empty
              S.currentAttr("b") must_== Empty
              S.currentAttr("c") must_== Empty
              S.currentAttr("d") must_== Full("d")
            }
            // Make sure the stack is unwound
            S.attr("d") must_== Empty
          }
          S.attr("b") must_== Empty
          S.attr("c") must_== Empty
        }
        S.attr("a") must_== Empty
        S.attrs must_== Nil
        S.currentAttrs must_== Null
      }
    }

    "reflect only the last pushed values with S.currentAttrs" in {
      S.initIfUninitted(new LiftSession("", "", Empty)) {
        S.withAttrs(new UnprefixedAttribute("a", "a", Null)) {
          S.withAttrs(new UnprefixedAttribute("b", "b", new UnprefixedAttribute("c", "c", Null))) {
            S.withAttrs(new UnprefixedAttribute("d", "d", Null)) {
              S.currentAttr("a") must_== Empty
              S.currentAttr("b") must_== Empty
              S.currentAttr("c") must_== Empty
              S.currentAttr("d") must_== Full("d")
            }
          }
        }
      }
    }

    "handle prefixes" in {
      S.initIfUninitted(new LiftSession("", "", Empty)) {
        S.withAttrs(new PrefixedAttribute("foo", "a", "a", Null)) {
          S.withAttrs(new PrefixedAttribute("foo", "b", "b", Null)) {
            S.withAttrs(new PrefixedAttribute("bar", "a", "aBar", Null)) {
              S.attr("a") must_== Empty
              S.attr("foo", "a") must_== Full("a")

              S.attr("bar", "a") must_== Full("aBar")

              val fooMap = S.prefixedAttrsToMap("foo", Map())

              fooMap("a") must_== "a"
              fooMap("b") must_== "b"

              val barMap = S.prefixedAttrsToMap("bar")

              barMap("a") must_== "aBar"
              barMap.get("b") must_== None
            }
          }
        }
      }
    }
  }
}

