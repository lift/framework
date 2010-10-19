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
package builtin.snippet {

import _root_.org.specs._
import _root_.org.specs.runner._
import _root_.org.specs.Sugar._
import _root_.net.liftweb.http.{LiftRules,LiftSession,S}
import _root_.net.liftweb.common._

import _root_.scala.xml.{Null,Text,UnprefixedAttribute,XML}

class MsgSpecTest extends Runner(MsgSpec) with JUnit with Console
object MsgSpec extends Specification {
  def withSession[T](f: => T) : T =
    S.initIfUninitted(new LiftSession("test", "", Empty))(f)

  "The built-in Msg snippet" should {
    "Properly render static content for a given id" in {
      withSession {
        // Set some notices
        S.error("foo", "Error")
        S.warning("bar", "Warning")
        S.notice("foo", "Notice")

        // We reparse due to inconsistencies with UnparsedAttributes
        val result = S.setVars(new UnprefixedAttribute("id", Text("foo"), new UnprefixedAttribute("noticeClass", Text("funky"), Null))) {
          XML.loadString(Msg.render(<div/>).toString)
        }
        
        result must ==/(<span id="foo">Error, <span class="funky">Notice</span></span>)
      }
    }

    "Properly render AJAX content for a given id" in {
      // TODO : Figure out how to test this
    }
  }
}

}} // Close nested packages
