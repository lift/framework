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

package webapptest {
package snippet {

import _root_.scala.xml._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.S._
import _root_.net.liftweb.http.SHtml._
import _root_.net.liftweb.http.{RequestVar, LiftResponse, BasicResponse, JsonResponse}
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.common.Box._
import _root_.net.liftweb.http.js._
import JE._
import JsCmds._

import _root_.net.liftweb.widgets.tree.{TreeView, Tree}

class TreeViewDemo {

  def render(xhtml: Group): NodeSeq = {
    TreeView("example", JsObj(("animated" -> 90)))
  }

  def renderAsync(xhtml: Group): NodeSeq = {
    TreeView("async", JsObj(("animated" -> 90)), loadTree _, loadNode _)
  }

  def loadTree: List[Tree] = Tree("1. Pre Lunch (120 min)", "folder", false, false,
         Tree("1.1 The State of the Powerdome (30 min)", "file") ::
         Tree("1.2 The Future of jQuery (30 min)", "file") ::
         Tree("1.2 jQuery UI - A step to richnessy (60 min)", "file") ::
         Nil) ::
    Tree("2. Lunch  (60 min)", "file") ::
    Tree("3. After Lunch  (120+ min)", "folder",
         Tree("3.1 jQuery Calendar Success Story (20 min)", "file") ::
         Tree("3.2 jQuery and Ruby Web Frameworks (20 min)", "file") ::
         Tree("3.3 Hey, I Can Do That! (20 min)", "file") ::
         Tree("3.4 Taconite and Form (20 min)", "file") ::
         Tree("3.5 Server-side JavaScript with jQuery and AOLserver (20 min)", "file") ::
         Tree("3.6 This will load the children dynamically via Ajax", Full("36"), Full("folder"), false, true, Nil) ::
         Tree("3.7 Visualizations with JavaScript and Canvas (20 min)", "file") ::
         Tree("3.8 Growing jQuery (20 min)", "file") ::
         Nil

    ) ::
    Nil




  def loadNode(id: String): List[Tree] = {
    println("Node id = " + id);
    Thread.sleep(1000)

    Tree("1. Review of existing structures", "folder", true, false,
         Tree("1.1 jQuery core", "file") ::
         Tree("1.2 metaplugins", "file") ::
         Nil
    ) ::
    Tree("2. Wrapper plugins", "file") ::
    Tree("3. Summary", "file") ::
    Tree("4. Questions and answers", "file") ::
    Nil

  }
 }

}
}
