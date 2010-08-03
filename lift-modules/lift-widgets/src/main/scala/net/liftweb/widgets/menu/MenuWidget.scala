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
package widgets {
package menu {

import _root_.scala.xml.{NodeSeq, Node, Elem, PCData, Text}
import _root_.net.liftweb.http.{LiftRules, S}
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.sitemap._
import JsCmds._
import JE._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._

/**
 * Defines the three superfish menu styles: Horizontal, Vertical, and Navbar. See
 * the superfish docs at http://users.tpg.com.au/j_birch/plugins/superfish/ for examples.
 */
object MenuStyle extends Enumeration("sf-menu", "sf-menu sf-vertical", "sf-menu sf-navbar") {
  val HORIZONTAL, VERTICAL, NAVBAR = Value
}

object MenuWidget {

  // TODO: This whole set of methods would benefit from default parameters when 2.8 comes out
  def apply() : NodeSeq = apply(MenuStyle.HORIZONTAL)

  def apply(groups : List[String]) : NodeSeq = apply(groups, MenuStyle.HORIZONTAL)

  def apply(style: MenuStyle.Value) = new MenuWidget(style, JsObj()) render

  def apply(groups : List[String], style : MenuStyle.Value) : NodeSeq = 
    new MenuWidget(style, JsObj(), groups) render

  def apply(jsObj: JsObj) = new MenuWidget(MenuStyle.HORIZONTAL, jsObj) render

  def apply(style: MenuStyle.Value, jsObj: JsObj) = new MenuWidget(style, jsObj) render

   /**
    * register the resources with lift (typically in boot)
    */
  def init() {
    import net.liftweb.http.ResourceServer

    ResourceServer.allow({
        case "menu" :: _ => true
     })
  }

}

/**
 * Builds a Menu widget based on the specified style, groups and JavaScript parameters.
 *
 * <p>Typical usage involves two parts:</p>
 *
 * <ol>
 *   <li>Call MenuWidget.init() in boot to set up resources, etc.</li>
 *   <li>Use the MenuWidget companion object to render a menu.</li>
 * </ol>
 * 
 * <p>For example, if we want a VERTICAL styled menu in our pages (HORIZONTAL is default),
 * we can define a "menubar" snippet method:</p>
 *
 * <pre name="code" class="scala">
 * import _root_.scala.xml.NodeSeq
 * import _root_.net.liftweb.widgets.menu.MenuWidget
 * 
 * class Menubar {
 *   def render (xhtml : NodeSeq) = MenuWidget()
 * }
 * </pre>
 *
 * <p>And then in our template(s), we simply use the following code wherever we want a menu:</p>
 *
 * <pre name="code" class="html">
 *   &lt;lift:Menubar /&gt;
 * </pre>
 */
class MenuWidget(style: MenuStyle.Value, jsObj: JsObj, groups : List[String]) {

  def this(style: MenuStyle.Value, jsObj: JsObj) = this(style, jsObj, Nil)

  def head: NodeSeq = <head>
      <link rel="stylesheet" href={"/" + LiftRules.resourceServerPath + "/menu/superfish.css"} type="text/css"/>{
        style match {
	      case MenuStyle.VERTICAL =>  <link rel="stylesheet" href={"/" + LiftRules.resourceServerPath + "/menu/superfish-vertical.css"} type="text/css"/>
	      case MenuStyle.NAVBAR =>  <link rel="stylesheet" href={"/" + LiftRules.resourceServerPath + "/menu/superfish-navbar.css"} type="text/css"/>
          case _ => NodeSeq.Empty
	    }
      }
      <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/menu/superfish.js"}/>
      <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/menu/jquery.hoverIntent.js"}/>
      {Script(JsRaw("""
         jQuery(document).ready(function() {
            jQuery('ul.sf-menu').superfish(""" + jsObj.toJsCmd + """);
          })
         """))
       }
    </head>


  def render : NodeSeq = {
    
    head ++ <div>{groups match {
      case Nil => <lift:Menu.builder expandAll="true" top:class={style.toString} />
      case xs => groups.flatMap(group => <lift:Menu.builder expandAll="true" top:class={style.toString} group={group} />)
    }}</div>
  }

}

}
}
}
