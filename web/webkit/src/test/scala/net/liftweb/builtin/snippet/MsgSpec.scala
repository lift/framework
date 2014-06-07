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
package builtin.snippet

import xml._
import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification

import common._
import http._

/**
 * System under specification for Msg.
 */
object MsgSpec extends Specification with XmlMatchers {
  "Msg Specification".title

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
        val result = S.withAttrs(new UnprefixedAttribute("id", Text("foo"), new UnprefixedAttribute("noticeClass", Text("funky"), Null))) {
          XML.loadString(Msg.render(<div/>).toString)
        }

        result must ==/(<span id="foo">Error, <span class="funky">Notice</span></span>)
      }
    }

    "Properly render AJAX content for a given id" in {
       withSession {
        // Set some notices
        S.error("foo", "Error")
        S.warning("bar", "Warning")
        S.notice("foo", "Notice")

        // We reparse due to inconsistencies with UnparsedAttributes
        val result = S.withAttrs(new UnprefixedAttribute("id", Text("foo"), new UnprefixedAttribute("noticeClass", Text("funky"), Null))) {
          Msg.render(<div/>).toString // render this first so attrs get captured
          LiftRules.noticesToJsCmd().toString.replace("\n", "")
        }

        result must_== """JsCmd(jQuery('#'+"foo").html("Error, <span class=\"funky\">Notice</span>");jQuery('#'+"bar").html("Warning");)"""
      }
    }
  }
}

