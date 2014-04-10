/*
 * Copyright 2007-2011 WorldWide Conferencing, LLC
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
package js 
package jquery 

import scala.xml.NodeSeq
import net.liftweb.util.Helpers._
import net.liftweb.util.Helpers
import net.liftweb.util.TimeHelpers
import net.liftweb.common._
import net.liftweb.util._

import net.liftweb.http.js.{JsExp, JE}
import JE._
import JsCmds._


/**
 * A singleton that vends various different functions for WiringUI support
 */
object JqWiringSupport {
  import js.JsCmds._
  /**
   * Fade out the old value and fade in the new value
   * using jQuery fast fade.
   */
  def fade: (String, Boolean, JsCmd) => JsCmd = {
    (id: String, first: Boolean, cmd: JsCmd) => {
      if (first) cmd
      else {
        val sel = "jQuery('#'+"+id.encJs+")"
        Run(sel+".fadeOut('fast', function() {"+
            cmd.toJsCmd+" "+sel+".fadeIn('fast');})")
      }
    }
  }

  /**
   * Hide the old value, set to new value and slide down.
   */
  def slideDown: (String, Boolean, JsCmd) => JsCmd = {
    (id: String, first: Boolean, cmd: JsCmd) => {
      if (first) cmd
      else {
        val sel = "jQuery('#'+"+id.encJs+")"
        Run(sel+".hide(); "+cmd.toJsCmd+" "+sel+".slideDown('fast')")
      }
    }
  }

  /**
   * Hide the old value, set to new value and slide down.
   */
  def slideUp: (String, Boolean, JsCmd) => JsCmd = {
    (id: String, first: Boolean, cmd: JsCmd) => {
      if (first) cmd
      else {
        val sel = "jQuery('#'+"+id.encJs+")"
        Run(sel+".hide(); "+cmd.toJsCmd+" "+sel+".slideUp('fast')")
      }
    }
  }

  /**
   * Takes two sequences, the id of a containing component and a couple of
   * functions and generates the jQuery-based JavaScript to update the browser
   * DOM with the deltas between the old list and the new list.
   */
  def calculateDeltas[T](oldList: Seq[T], newList: Seq[T],id: String)(calcId: T => String, calcNodeSeq: T => NodeSeq): JsCmd = 
    calculateDeltas[T](Full(oldList), newList, id)(calcId, calcNodeSeq)

  /**
   * Takes two sequences, the id of a containing component and a couple of
   * functions and generates the jQuery-based JavaScript to update the browser
   * DOM with the deltas between the old list and the new list.
   *
   * @param oldList -- the old list.  If it is Empty, then it is treated as Nil
   * @param newList -- the new version of the list of items
   * @param id -- the id of the enclosing DOM node.  Used for appending and inserting DOM nodes
   * @param calcId -- given a T, calculate the id of the DOM node for the T
   * @param calcNodeSeq -- given a T, calculate the DOM that represents the T
   *
   * @return the JsCmd that inserts, appends, removes, etc. the DOM so that
   * the DOM represents the new List
   */
  def calculateDeltas[T](oldList: Box[Seq[T]], newList: Seq[T],id: String)(calcId: T => String, calcNodeSeq: T => NodeSeq): JsCmd = {
    Helpers.delta(oldList, newList) {
      case RemoveDelta(ci) => new JsCmd {
        def toJsCmd = "jQuery('#'+"+calcId(ci).encJs+").remove();"
      }
      
      case AppendDelta(ci) => 
        new JsCmd {
          val toJsCmd = 
            fixHtmlFunc("inline", calcNodeSeq(ci)) {
              "jQuery('#'+"+id.encJs+").append("+
              _+
              ");"}
        }

      case InsertAtStartDelta(ci) => 
        new JsCmd {
          val toJsCmd = 
            fixHtmlFunc("inline", calcNodeSeq(ci)) {
              "jQuery('#'+"+id.encJs+").prepend("+
              _+
              ");"}
        }

      case InsertAfterDelta(ci, prior) => 
        new JsCmd {
          val toJsCmd = 
            fixHtmlFunc("inline", calcNodeSeq(ci)) {
              "jQuery('#'+"+calcId(prior).encJs+").after("+
              _+
              ");"}
        }
    }
  }
                           
}

/**
 * Contains Scala JsExps for jQuery behaviors.
 *
 * These functions are meant to be combined using the ~> operator. For
 * example:
 *
 *   <pre>JqJE.Jq("button") ~> JqClick(AnonFunc(...))</pre>
 *
 * Documentation on the case classes themselves will point to the
 * relevant jQuery documentation, if there is any.
 */
object JqJE {
  /**
   * Changes the scroll position of each matched element to its maximum.
   */
  case object JqScrollToBottom extends JsExp with JsMember {
    def toJsCmd = "each(function(i) {this.scrollTop=this.scrollHeight;})"
  }

  /**
   * Calls the jQuery click function with the parameter in exp.
   *
   * Used to set a click handler function (also see AnonFunc).
   *
   * See http://api.jquery.com/click/ .
   */
  case class JqClick(exp: JsExp) extends JsExp with JsMember {
    def toJsCmd = "click(" + exp.toJsCmd + ")"
  }

  /**
   * Calls the jQuery attr function with the given key.
   *
   * Used to get the value of the given attribute.
   * 
   * See http://api.jquery.com/attr/ .
   */
  case class JqGetAttr(key: String) extends JsExp with JsMember {
    def toJsCmd = "attr(" + key.encJs + ")"
  }

  /**
   * Calls the main jQuery (or $) function with the parameter in query.
   *
   * Used to get a set of elements to apply the other JqJE expressions to.
   *
   * See http://api.jquery.com/jQuery/ .
   */
  case class Jq(query: JsExp) extends JsExp {
    override def toJsCmd = "jQuery(" + query.toJsCmd + ")"
  }

  /**
   * Calls the main jQuery (or $) function with "document". This returns
   * the jQueryied document object (e.g., for calling ready()).
   *
   * See http://api.jquery.com/jQuery/ .
   */
  case object JqDoc extends JsExp {
    override def toJsCmd = "jQuery(document)"
  }

  /**
   * For every passed tuple, executes the given JsCmd when the given
   * Char is pressed by the user. Watches using the jQuery keypress
   * function.
   *
   * See http://api.jquery.com/keypress/ .
   */
  case class JqKeypress(what: (Char, JsCmd)*) extends JsExp with JsMember {
    override def toJsCmd = "keypress(function(e) {" +
            what.map {
              case (chr, cmd) =>
                "if (e.which == " + chr.toInt + ") {" + cmd.toJsCmd + "}"
            }.mkString(" else \n") +
            "})"
  }

  /**
   * A JQuery query for an element based on the id of the element
   */
  case class JqId(id: JsExp) extends JsExp {
    override def toJsCmd = "jQuery('#'+" + id.toJsCmd + ")"
  }

  /**
   * Calls the jQuery attr function with the given key and the given value.
   *
   * Used to set the given attribute to the given value.
   *
   * See http://api.jquery.com/attr/ .
   */
  case class JqAttr(key: String, value: JsExp) extends JsExp with JsMember {
    def toJsCmd = "attr(" + key.encJs + ", " + value.toJsCmd + ")"
  }

  /**
   * Calls the jQuery append function with the given content.
   *
   * Used to append the given content to the matched elements.
   *
   * See http://api.jquery.com/append/ .
   */
  case class JqAppend(content: NodeSeq) extends JsExp with JsMember {
    override val toJsCmd = 
      "append("+fixHtmlFunc("inline", content){a => a}+")"      
  }

  /**
   * Calls the jQuery remove function.
   *
   * Used to remove the matched elements from the DOM.
   *
   * See http://api.jquery.com/remove/ .
   */
  case class JqRemove() extends JsExp with JsMember {
    override def toJsCmd = "remove()"
  }


  /**
   * Calls the jQuery appendTo function with the given content.
   *
   * Used to wrap the matched elements in the given content.
   *
   * See http://api.jquery.com/appendTo/ .
   */
  case class JqAppendTo(content: NodeSeq) extends JsExp with JsMember {
    override val toJsCmd =       
      "appendTo("+fixHtmlFunc("inline", content){str => str}+ ")"
  }

  /**
   * Calls the jQuery prepend function with the given content.
   *
   * Used to prepend the given content to each matched element.
   *
   * See http://api.jquery.com/prepend/ .
   */
  case class JqPrepend(content: NodeSeq) extends JsExp with JsMember {
    override val toJsCmd = 
    "prepend(" + fixHtmlFunc("inline", content){str => str }+ ")"
  }

  /**
   * Calls the jQuery prependTo function with the given content.
   *
   * Used to prepend the matched elements to the given content.
   *
   * See http://api.jquery.com/prependTo/ .
   */
  case class JqPrependTo(content: NodeSeq) extends JsExp with JsMember {
    override val toJsCmd = 
    "prependTo(" + fixHtmlFunc("inline", content){str => str} + ")"
  }

  /**
   * Calls the jQuery css function with the given name and value.
   *
   * Used to set the value of the given CSS property to the given value.
   *
   * See http://api.jquery.com/css/ .
   */
  case class JqCss (name: JsExp, value: JsExp) extends JsExp with JsMember {
    override def toJsCmd = "css(" + name.toJsCmd + "," + value.toJsCmd + ")"
  }

  /**
   * Calls the jQuery empty function followed by calling the jQuery
   * after function with the given content.
   *
   * The intent is to empty the matched nodes and stick the given
   * content at their tails. Like a cleaner innerHTML.
   *
   * See http://api.jquery.com/empty/ and http://api.jquery.com/after/ .
   */
  case class JqEmptyAfter(content: NodeSeq) extends JsExp with JsMember {
    override val toJsCmd = 
    "empty().after(" + fixHtmlFunc("inline", content){str => str} + ")"
  }

  /**
   * Calls the jQuery replaceWith function with the given content.
   *
   * Used to replace the matched elements with the given content.
   *
   * See http://api.jquery.com/replaceWith/ .
   */
  case class JqReplace(content: NodeSeq) extends JsExp with JsMember {
    override val toJsCmd = fixHtmlCmdFunc("inline", content){"replaceWith(" + _ + ")"}
  }

  object JqHtml {
    /**
     * Calls the jQuery html function with no parameters.
     *
     * Used to get the inner HTML of the matched elements.
     *
     * See http://api.jquery.com/html/ .
     */
    def apply(): JsExp with JsMember = new JsExp with JsMember {
      def toJsCmd = "html()"
    }

    /**
     * Calls the jQuery html function with the given content.
     *
     * Used to set the inner HTML of each matched element.
     *
     * See http://api.jquery.com/html/ .
     */
    def apply(content: NodeSeq): JsExp with JsMember = new JsExp with JsMember {
      val toJsCmd = fixHtmlCmdFunc("inline", content){"html(" + _ + ")"}
    }
  }

  object JqText {
    /**
     * Calls the jQuery text function with no parameters.
     *
     * Used to get the combined text contents of the matched elements.
     *
     * See http://api.jquery.com/text/ .
     */
    def apply(): JsExp with JsMember = new JsExp with JsMember {
      def toJsCmd = "text()"
    }

    /**
     * Calls the jQuery text function with the given content.
     *
     * Used to set the text contents of the matched elements.
     *
     * See http://api.jquery.com/text/ .
     */
    def apply(content: String): JsExp with JsMember = new JsExp with JsMember {
      def toJsCmd = "text(" + content.encJs + ")"
    }
  }

  /**
   * Calls the jQuery serialize function.
   *
   * Used to serialize input elements or forms into query string data.
   *
   * See http://api.jquery.com/serialize/ .
   */
  case object JqSerialize extends JsExp with JsMember {
    def toJsCmd = "serialize()"
  }

  /**
   * Calls the jQuery serializeArray function.
   *
   * Used to serialize the matched elements into a JSON array containing
   * objects with name and value properties.
   *
   * See http://api.jquery.com/serializeArray/ .
   */
  case object JsonSerialize extends JsExp with JsMember {
    def toJsCmd = "serializeArray()"
  }

  case object JqTabsSelected extends JsExp with JsMember {
    def toJsCmd = "tabsSelected()"
  }

  object JqTabsClick {
    def apply(tab: JsExp): JsExp with JsMember =
      new JsExp with JsMember {
        def toJsCmd = "tabsClick(" + tab.toJsCmd + ")"
      }

    def apply(tab: Int): JsExp with JsMember =
      apply(Num(tab))
  }

  object JqTabs {
    def apply(in: JsExp): JsExp with JsMember =
      new JsExp with JsMember {
        def toJsCmd = "tabs(" + in.toJsCmd + ")"
      }

    def apply(): JsExp with JsMember =
      apply(JsRaw(""))
  }

}

object JqJsCmds {
  implicit def jsExpToJsCmd(in: JsExp) = in.cmd

  /**
   * Queues the JavaScript in cmd for execution when the document is
   * ready for processing
   */
  case class JqOnLoad(cmd: JsCmd) extends JsCmd {
    def toJsCmd = "jQuery(document).ready(function() {" + cmd.toJsCmd + "});"
  }

  /**
   * Append a NodeSeq to a node specified by uid using jQuery's append() method.
   */
  object AppendHtml {
    def apply(uid: String, content: NodeSeq): JsCmd =
      JqJE.JqId(JE.Str(uid)) ~> JqJE.JqAppend(content)
  }

  /**
   * AppendTo a NodeSeq to a node specified by uid using jQuery's appendTo() method.
   */
  object AppendToHtml {
    def apply(uid: String, content: NodeSeq): JsCmd =
      JqJE.JqId(JE.Str(uid)) ~> JqJE.JqAppendTo(content)
  }

  /**
   * Prepends a NodeSeq to a node specified by uid using jQuery's prepend() method.
   */
  object PrependHtml {
    def apply(uid: String, content: NodeSeq): JsCmd =
      JqJE.JqId(JE.Str(uid)) ~> JqJE.JqPrepend(content)
  }

  /**
   * Replaces the children of the node at  { @code uid } with  { @code content }
   */
  object EmptyAfter {
    def apply(uid: String, content: NodeSeq): JsCmd =
      JqJE.JqId(JE.Str(uid)) ~> JqJE.JqEmptyAfter(content)
  }

  /**
   * Prepends a NodeSeq to a node specified by uid using jQuery prependTo() method.
   */
  object PrependToHtml {
    def apply(uid: String, content: NodeSeq): JsCmd =
      JqJE.JqId(JE.Str(uid)) ~> JqJE.JqPrependTo(content)
  }

  /**
   * Replaces the content of the node with the provided id with the markup given by content
   */
  case class JqReplace(uid: String, content: NodeSeq) extends JsCmd {
    val toJsCmd = (JqJE.JqId(JE.Str(uid)) ~> JqJE.JqReplace(content)).cmd.toJsCmd
  }

  /**
   * Sets the inner HTML of the element denominated by the id
   */
  case class JqSetHtml(uid: String, content: NodeSeq) extends JsCmd {
    /**
     * Eagerly evaluate
     */
    val toJsCmd = (JqJE.JqId(JE.Str(uid)) ~> JqJE.JqHtml(content)).cmd.toJsCmd
  }

  /**
   * Show an element identified by uid.
   * There are two apply methods, one takes just the id, the other takes the id and timespan
   * that represents how long the animation will last
   */
  object Show {
    /**
     * Show an element based on the ID uid
     */
    def apply(uid: String) = new Show(uid, Empty)

    /**
     *
     * Show an element identified by uid
     *
     * @param uid the element id
     * @param time the duration of the effect.
     */
    def apply(uid: String, time: TimeSpan) = new Show(uid, Full(time))
  }

  /**
   * Show an element identified by uid
   *
   * @param uid the element id
   * @param time the duration of the effect.
   */
  class Show(val uid: String, val time: Box[TimeSpan]) extends JsCmd with HasTime {
    def toJsCmd = "try{jQuery(" + ("#" + uid).encJs + ").show(" + timeStr + ");} catch (e) {}"
  }

  /**
   * Hide an element identified by uid.
   * There are two apply methods, one takes just the id, the other takes the id and timespan
   * that represents how long the animation will last
   */
  object Hide {
    /**
     * Hide an element based on the ID uid
     */
    def apply(uid: String) = new Hide(uid, Empty)

    /**
     * Hide an element identified by uid
     *
     * @param uid the element id
     * @param time the duration of the effect.
     */
    def apply(uid: String, time: TimeSpan) = new Hide(uid, Full(time))
  }

  /**
   * Hide an element identified by uid and the animation will last @time
   */
  class Hide(val uid: String, val time: Box[TimeSpan]) extends JsCmd with HasTime {
    def toJsCmd = "try{jQuery(" + ("#" + uid).encJs + ").hide(" + timeStr + ");} catch (e) {}"
  }

  /**
   * Show a message msg in the element with id where for duration milliseconds and fade out in fadeout milliseconds
   */
  case class DisplayMessage(where: String, msg: NodeSeq, duration: TimeSpan, fadeTime: TimeSpan) extends JsCmd {
    def toJsCmd = (Show(where) & JqSetHtml(where, msg) & After(duration, Hide(where, fadeTime))).toJsCmd
  }

  /**
  * The companion object to FadeOut that provides an alternative factory
  */
  object FadeOut {
    /**
    * Fade Out with the default duration and fadeTime provided by JsRules
    */
    def apply(id: String) = new FadeOut(id, JsRules.prefadeDuration, JsRules.fadeTime)
  }

  /**
   * Fades out the element having the provided id, by waiting
   * for the given duration and fading out during fadeTime
   */
  case class FadeOut(id: String, duration: TimeSpan, fadeTime: TimeSpan) extends JsCmd {
    def toJsCmd = (After(duration, JqJE.JqId(id) ~> (new JsRaw("fadeOut(" + fadeTime.millis + ")") with JsMember))).toJsCmd
  }

  /**
  * The companion object to FadeIn that provides an alternative factory
  */
  object FadeIn {
    /**
    * Fade In with the default duration and fadeTime provided by JsRules
    */
    def apply(id: String) = new FadeIn(id, JsRules.prefadeDuration, JsRules.fadeTime)
  }

  /**
   * Fades in the element having the provided id, by waiting
   * for the given duration and fading in during fadeTime
   * and use @fadeTime
   */
  case class FadeIn(id: String, duration: TimeSpan, fadeTime: TimeSpan) extends JsCmd {
    def toJsCmd = (After(duration, JqJE.JqId(id) ~> (new JsRaw("fadeIn(" + fadeTime.millis + ")") with JsMember))).toJsCmd
  }

  /**
   * Companion object for ModelDialog that provides two alternative factories
   */
  object ModalDialog {

    /**
     * Requires the jQuery blockUI plugin
     *
     * @param html the html for the ModalDialog
     */
    def apply(html: NodeSeq) = new ModalDialog(html, Empty)

    /**
     * Requires the jQuery blockUI plugin
     *
     * @param html the html for the ModalDialog
     * @param css the css to apply to the dialog
     */
    def apply(html: NodeSeq, css: JsObj) = new ModalDialog(html, Full(css))
  }

  /**
   * Requires the jQuery blockUI plugin
   */
  class ModalDialog(html: NodeSeq, css: Box[JsObj]) extends JsCmd {
    /*
    private def contentAsJsStr = {
    val w = new java.io.StringWriter
    
    S.htmlProperties.
    htmlWriter(Group(S.session.
                     map(s =>
                       s.fixHtml(s.processSurroundAndInclude("Modal Dialog",
                                                             html))).
                     openOr(html)),
               w)
    w.toString.encJs
    }
*/

    val toJsCmd = fixHtmlCmdFunc("inline", html){str => 
      "jQuery.blockUI({ message: " + str +
      (css.map(",  css: " + _.toJsCmd + " ").openOr("")) + "});"}
  }

  /**
   * Remove the jQuery.Block dialog
   */
  case object Unblock extends JsCmd {
    def toJsCmd = "jQuery.unblockUI();"
  }


}
