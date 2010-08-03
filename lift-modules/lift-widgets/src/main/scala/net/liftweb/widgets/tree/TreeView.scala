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
package tree {

import _root_.scala.xml._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.common.{Box, Full, Empty}
import _root_.net.liftweb.http.S._
import _root_.net.liftweb.http.LiftRules
import _root_.net.liftweb.http.{LiftResponse, JsonResponse}
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.http.js.jquery._
import JsCmds._
import JE._
import JqJsCmds._
import JqJE._

object TreeView {

  def apply(id: String, jsObj: JsObj) = new TreeView().onLoad(id, jsObj)
  def apply(id: String, jsObj: JsObj, loadTree : () => List[Tree], loadNode: (String) => List[Tree]) =
    new TreeView().onLoadAsync(id, jsObj, loadTree, loadNode)

  /**
   * Call this function typically in boot
   */
  def init() {
    import _root_.net.liftweb.http.ResourceServer
    ResourceServer.allow({
      case "tree" :: _ => true
    })

  }
}

class TreeView {

  /**
   * Makes a static tree out of the <ul><li> lists. The tree is buid when page loads.
   *
   * @param id - the id of the empty <ul> element that will be populated with the tree
   * @param jsObj - the JSON object passed to the treeview function
   *
   */
  def onLoad(id: String, jsObj: JsObj) : NodeSeq = {
    <head>
      <link rel="stylesheet" href={"/" + LiftRules.resourceServerPath + "/tree/jquery.treeview.css"} type="text/css"/>
      <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/tree/jquery.treeview.js"}/>
       {Script(OnLoad(JqId(id) ~> new JsExp with JsMember {
           def toJsCmd = "treeview(" + jsObj.toJsCmd + ")"
         }))
       }
    </head>

  }

  /**
   * Makes the tree to be loaded when the page loads
   *
   * @param id - the id of the empty <ul> element that will be populated with the tree
   * @param jsObj - the JSON object passed to the treeview function
   * @param loadTree - the function that will be called when the entire tree will be dynamically loaded
   * @param loadNode - the function that will be called when a tree node (other then root) will be retrieved via Ajax
   *
   */
  def onLoadAsync(id: String, jsObj: JsObj, loadTree : () => List[Tree], loadNode: (String) => List[Tree]): NodeSeq = {
    val (gc, js) = makeTreeView(id, jsObj, loadTree, loadNode)

     <head>
       <link rel="stylesheet" href={"/" + LiftRules.resourceServerPath + "/tree/jquery.treeview.css"} type="text/css"/>
       <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/tree/jquery.treeview.js"}/>
       <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/tree/jquery.treeview.async.js"}/>
	    {
	      Script(OnLoad(js.cmd))
	    }
     </head>

  }


  /**
   * @param id - the id of the empty <ul> element that will be populated with the tree
   * @param jsObj - the JSON object passed to the treeview function
   * @param loadTree - the function that will be called when the entire tree will be dynamically loaded
   * @param loadNode - the function that will be called when a tree node (other then root) will be retrieved via Ajax
   *
   * @return JsExp - the Java Script expression that calls the treeview function on the element denominated by the id
   *
   */
  def makeTreeView(id: String, jsObj: JsObj, loadTree : () => List[Tree], loadNode: (String) => List[Tree]): (String, JsExp) = {
     val treeFunc : () => LiftResponse = {
     case _ => request match {
       case Full(req) => req.params.get("root") match {
         case Some("source" :: _) => JsonResponse(JsRaw(Tree.toJSON(loadTree())))
         case Some(nodeId :: _) => JsonResponse(JsRaw(Tree.toJSON(loadNode(nodeId))))
         case _ => JsonResponse(JsRaw("[]"))
       }
       case _ => JsonResponse(JsRaw("[]"))
       }
     }

     fmapFunc(NFuncHolder(treeFunc)){key =>

       val url = encodeURL(contextPath +
	  		 "/"+LiftRules.ajaxPath)+"?"+key+"=_"

       val obj: JsObj = JsObj(("url" -> Str(url)) :: jsObj.props:_*)

       (key, JqId(id) ~> new JsExp with JsMember {
         def toJsCmd = "treeview(" + obj.toJsCmd + ")"
       })
     }
  }

}

object Tree {
  def apply(text:String) = new Tree(text, Empty, Empty, false, false, Nil)
  def apply(text:String, id: String, hasChildren: Boolean) = new Tree(text, Full(id), Empty, false, true, Nil)
  def apply(text:String, classes: String) = new Tree(text, Empty, Full(classes), false, false, Nil)
  def apply(text:String, children: List[Tree]) = new Tree(text, Empty, Empty, false, false, children)
  def apply(text:String, classes: String, children: List[Tree]) = new Tree(text, Empty, Full(classes), false, false, children)
  def apply(text:String, classes: String, expanded: Boolean, hasChildren: Boolean, children: List[Tree]) =
    new Tree(text, Empty, Full(classes), expanded, hasChildren, children)
  def apply(text:String, id: String, classes: String, expanded: Boolean, hasChildren: Boolean, children: List[Tree]) =
    new Tree(text, Full(id), Full(classes), expanded, hasChildren, children)

  def toJSON(nodes: List[Tree]): String = nodes.map(_ toJSON).mkString("[", ", ", "]")
}

/**
 * Server side representation of a node of the tree widget
 */
case class Tree(text:String,
                id: Box[String],
                classes: Box[String],
                expanded: Boolean,
                hasChildren: Boolean,
                children: List[Tree]) {

  def toJSON: String = {

      "{ \"text\": \"" + text + "\"" +
        id.map(id => ", \"id\": \"" + id + "\"").openOr("") +
        classes.map(cls => ", \"classes\": \"" + cls + "\"").openOr("") +
        (hasChildren match { case true => ", \"hasChildren\": true" case _ =>  ""}) +
        (expanded match { case true => ", \"expanded\": true" case _ =>  ""}) +
        (children match {
          case Nil => ""
          case childs => ", \"children\": " + childs.map(_ toJSON).mkString("[", ", ", "]")
        }) +
        " }"
  }

}

}
}
}
