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

class SnippetSpecTest extends Runner(SnippetSpec) with JUnit with Console
object SnippetSpec extends Specification {
  def makeReq = new Req(Req.NilPath, "", GetRequest, Empty, null,
                    System.nanoTime, System.nanoTime, false,
                    () => ParamCalcInfo(Nil, Map.empty, Nil, Empty), Map())

  "LiftSession" should {
    "Correctly process lift:start_at" in {
      val session = new LiftSession("", "hello", Empty)
      val ret = session.checkStartAt(<html lift:start_at="content">
                                     <head/>
                                     <body>
                                     <div id="content" class="lift:surround"/>
                                     </body>
                                     </html>)

      ret must ==/ (<div id="content" class="lift:surround"/>)
    }

    "Correctly process l:start_at" in {
      val session = new LiftSession("", "hello", Empty)
      val ret = session.checkStartAt(<html l:start_at="dog">
                                     <head/>
                                     <body>
                                     <lift:surround id="dog"><div/></lift:surround>
                                     </body>
                                     </html>)

      ret must ==/ (<lift:surround id="dog"><div/></lift:surround>)
    }

    "Correctly process not lift:designer_friendly" in {
      val session = new LiftSession("", "hello", Empty)
      val xml = <html>
      <head/>
      <body>
      <div class="lift:surround"/>
      </body>
      </html>
      
      val ret = session.checkStartAt(xml)

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

      ret.open_! must ==/( res)
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

      ret.open_! must ==/( res)
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

      ret.open_! must ==/( res)
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

      ret.open_! must ==/( res)
    }

    "Snippet invocation fails class='l:bar'" in {
      val res = <div/>

      val ret =
        S.statelessInit(Req.nil) {
          S.mapSnippetsWith("foo" -> ((a: NodeSeq) => a)) {
            for {
              s <- S.session
            } yield s.processSurroundAndInclude("test", <div class="lift:bar" />)
          }
        }

      (ret.open_! \ "@class").text must_== "snippeterror"
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

      (ret.open_! \ "@class").text must_== "snippeterror"

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

      ret.open_! must ==/(res)
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

      (ret.open_! \ "@class").text must_== "snippeterror"
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

      ret.open_! must ==/(res)
    }

    "Eager Eval works" in {
      val res = <div>dog</div>
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

}


}}
