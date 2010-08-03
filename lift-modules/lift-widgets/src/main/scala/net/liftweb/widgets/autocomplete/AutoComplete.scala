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
package autocomplete {

import _root_.scala.xml.{NodeSeq, Node, Elem, PCData, Text}
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.js._
import JsCmds._
import JE._
import S._
import SHtml._
import Helpers._

object AutoComplete {

  def apply(start: String, 
            options: (String, Int) => Seq[String],
            onSubmit: String => Unit, 
            attrs: (String, String)*) = new AutoComplete().render(start, options, onSubmit, attrs:_*)
  
  def apply(start: String, 
            options: (String, Int) => Seq[String],
            onSubmit: String => Unit, 
            jsonOptions: List[(String,String)],
            attrs: (String, String)*) = new AutoComplete().render(start, options, onSubmit, jsonOptions ,attrs:_*)

  def autocompleteObj[T](options: Seq[(T, String)], 
                         default: Box[T],
                         onSubmit: T => Unit): Elem = new AutoComplete().autocompleteObj(options, default, onSubmit)

  def autocompleteObj[T](options: Seq[(T, String)], 
                          default: Box[T],
                          jsonOptions: List[(String,String)],
                          onSubmit: T => Unit): Elem = new AutoComplete().autocompleteObj(options, default, jsonOptions, onSubmit)

    
    
  /**
   * register the resources with lift (typically in boot)
   */
  def init() {
    import net.liftweb.http.ResourceServer

    ResourceServer.allow({
        case "autocomplete" :: _ => true
     })
  }

}

class AutoComplete {
  
  /**
   * Create an autocomplete form based on a sequence.
   */
  def autocompleteObj[T](options: Seq[(T, String)], 
                         default: Box[T],
                         onSubmit: T => Unit): Elem = {
     val jsonOptions :List[(String,String)] = List()
     autocompleteObj(options, default, jsonOptions, onSubmit)
  }
  
  /**
   * Create an autocomplete form based on a sequence.
   */
   def autocompleteObj[T](options: Seq[(T, String)], 
                          default: Box[T],
                          jsonOptions: List[(String,String)],
                          onSubmit: T => Unit): Elem = {
    val (nonces, defaultNonce, secureOnSubmit) = secureOptions(options, default, onSubmit)
    val defaultString = default.flatMap(d => options.find(_._1 == d).map(_._2))

    autocomplete_*(nonces, defaultString, defaultNonce, secureOnSubmit, jsonOptions)
  }

  private def autocomplete_*(options: Seq[(String, String)], default: Box[String],
                     defaultNonce: Box[String], onSubmit: AFuncHolder, jsonOptions: List[(String,String)]): Elem = {
    val id = Helpers.nextFuncName

    fmapFunc(onSubmit){hidden =>
      
      val data = JsArray(options.map { 
        case (nonce, name) => JsObj("name" -> name, "nonce" -> nonce)
      } :_*)
      
    /* merge the options that the user wants */
      val jqOptions =  ("minChars","0") ::
                       ("matchContains","true") ::
                       ("formatItem","function(row, i, max) { return row.name; }") ::
                       Nil ::: jsonOptions
	  val json = jqOptions.map(t => t._1 + ":" + t._2).mkString("{", ",", "}")
      val autocompleteOptions = JsRaw(json)

      val onLoad = JsRaw("""
      jQuery(document).ready(function(){
        var data = """+data.toJsCmd+""";
        jQuery("#"""+id+"""").autocomplete(data, """+autocompleteOptions.toJsCmd+""").result(function(event, dt, formatted) {
          jQuery("#"""+hidden+"""").val(formatted);
        });
      });""")

      <span>
        <head>
          <link rel="stylesheet" href={"/" + LiftRules.resourceServerPath +"/autocomplete/jquery.autocomplete.css"} type="text/css" />
          <script type="text/javascript" src={"/" + LiftRules.resourceServerPath +"/autocomplete/jquery.autocomplete.js"} />
          {Script(onLoad)}
        </head>
        <input type="text" id={id} value={default.openOr("")} />
        <input type="hidden" name={hidden} id={hidden} value={defaultNonce.openOr("")} />
      </span>
     }
  }
  
  private def secureOptions[T](options: Seq[(T, String)], default: Box[T],
                                     onSubmit: T => Unit): (Seq[(String, String)], Box[String], AFuncHolder) = {
    val secure = options.map{case (obj, txt) => (obj, randomString(20), txt)}
    val defaultNonce = default.flatMap(d => secure.find(_._1 == d).map(_._2))
    val nonces = secure.map{case (obj, nonce, txt) => (nonce, txt)}
    def process(nonce: String): Unit = secure.find(_._2 == nonce).map(x => onSubmit(x._1))
    (nonces, defaultNonce, SFuncHolder(process))
  }
  
  
  /**
   * Render a text field with Ajax autocomplete support
   * 
   * @param start - the initial input string
   * @param option - the function to be called when user is typing text. The text and th options limit is provided to this functions
   * @param attrs - the attributes that can be added to the input text field 
   */
  def render(start: String, 
             options: (String, Int) => Seq[String], 
             onSubmit: String => Unit, 
             attrs: (String, String)*): Elem = {
    
    val jsonOptions :List[(String,String)] = List()
    render(start, options, onSubmit, jsonOptions, attrs:_*)
    
  }
  
  /**
   * Render a text field with Ajax autocomplete support
   * 
   * @param start - the initial input string
   * @param option - the function to be called when user is typing text. The text and th options limit is provided to this functions
   * @param attrs - the attributes that can be added to the input text field 
   * @param jsonOptions - a list of pairs that will be send along to the jQuery().AutoComplete call (for customization purposes)
   */
   def render(start: String, 
              options: (String, Int) => Seq[String], 
              onSubmit: String => Unit, 
              jsonOptions: List[(String,String)], 
              attrs: (String, String)*): Elem = {
    
    val f = (ignore: String) => {
      val q = S.param("q").openOr("")
      val limit = S.param("limit").flatMap(asInt).openOr(10)
      PlainTextResponse(options(q, limit).map(s => s+"|"+s).mkString("\n"))
    }


    fmapFunc(SFuncHolder(f)){ func =>
      val what: String = encodeURL(S.contextPath + "/" + LiftRules.ajaxPath+"?"+func+"=foo")

      val id = Helpers.nextFuncName
      fmapFunc(SFuncHolder(onSubmit)){hidden =>

     /* merge the options that the user wants */
      val jqOptions =  ("minChars","0") ::
                       ("matchContains","true") ::
                       Nil ::: jsonOptions
      val json = jqOptions.map(t => t._1 + ":" + t._2).mkString("{", ",", "}")
      val autocompleteOptions = JsRaw(json)
    
      val onLoad = JsRaw("""
      jQuery(document).ready(function(){
        var data = """+what.encJs+""";
        jQuery("#"""+id+"""").autocomplete(data, """+autocompleteOptions.toJsCmd+""").result(function(event, dt, formatted) {
          jQuery("#"""+hidden+"""").val(formatted);
        });
      });""")

      <span>
        <head>
          <link rel="stylesheet" href={"/" + LiftRules.resourceServerPath +"/autocomplete/jquery.autocomplete.css"} type="text/css" />
          <script type="text/javascript" src={"/" + LiftRules.resourceServerPath +"/autocomplete/jquery.autocomplete.js"} />
          {Script(onLoad)}
        </head>
        {
          attrs.foldLeft(<input type="text" id={id} value={start} />)(_ % _)
        }
        <input type="hidden" name={hidden} id={hidden} value={start} />
      </span>
    }
   }
  }
}

}
}
}
