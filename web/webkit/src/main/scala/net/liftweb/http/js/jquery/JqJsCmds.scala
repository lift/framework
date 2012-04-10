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
 * Classes mixing JQueryRight are also mixing JsMember. As JQueryRight
 * is deprecated clases mixing this trait with stop doing so soon and they
 * will mixin JsMember instead.
 */
@deprecated(""" As JQueryRight
is deprecated clases mixing this trait with stop doing so soon and they
will mixin JsMember instead.""")
trait JQueryRight {
  this: JsExp =>
  def toJsCmd: String
}
/**
 * Classes mixing JQuryLeft will soon stop doing so. Extending/Mixing JsExp will be enough
 */ 
@deprecated("Classes mixing JQuryLeft will soon stop doing so. Extending/Mixing JsExp will be enough")
trait JQueryLeft {
  this: JsExp =>
}

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

object JqJE {
  case object JqScrollToBottom extends JsExp with JsMember with JQueryRight with JQueryLeft {
    def toJsCmd = "each(function(i) {this.scrollTop=this.scrollHeight;})"
  }

  /**
   * Bind an event handler to the "click" JavaScript event, or trigger that event on an element.
   */
  case class JqClick(exp: JsExp) extends JsExp with JsMember with JQueryLeft with JQueryRight {
    def toJsCmd = "click(" + exp.toJsCmd + ")"
  }

  /**
   * Get the value of the first attribute specified by key
   */
  case class JqGetAttr(key: String) extends JsExp with JsMember with JQueryRight with JQueryLeft {
    def toJsCmd = "attr(" + key.encJs + ")"
  }

  /**
   * A JQuery query
   */
  case class Jq(query: JsExp) extends JsExp with JQueryLeft {
    override def toJsCmd = "jQuery(" + query.toJsCmd + ")"
  }

  case object JqDoc extends JsExp with JQueryLeft {
    override def toJsCmd = "jQuery(document)"
  }

  /**
   * Execute a JsCmd when the Char is pressed. Uses jQuery keypress
   * @param what a tuple of (Char, JsCmd)
   */
  case class JqKeypress(what: (Char, JsCmd)*) extends JsExp with JsMember with JQueryRight {
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
  case class JqId(id: JsExp) extends JsExp with JQueryLeft {
    override def toJsCmd = "jQuery('#'+" + id.toJsCmd + ")"
  }

  /**
   * Set the value of an attribute key, the value "value"
   * @param key the attribute to set its value
   * @param value the value :)
   */
  case class JqAttr(key: String, value: JsExp) extends JsExp with JsMember with JQueryRight with JQueryLeft {
    def toJsCmd = "attr(" + key.encJs + ", " + value.toJsCmd + ")"
  }

  /**
   * Append content to a JQuery
   */
  case class JqAppend(content: NodeSeq) extends JsExp with JsMember with JQueryRight with JQueryLeft {
    override val toJsCmd = 
      "append("+fixHtmlFunc("inline", content){a => a}+")"      
  }

  /**
   * Remove JQuery
   */
  case class JqRemove() extends JsExp with JsMember with JQueryRight with JQueryLeft {
    override def toJsCmd = "remove()"
  }


  /**
   * AppendTo content to a JQuery
   */
  case class JqAppendTo(content: NodeSeq) extends JsExp with JsMember with JQueryRight with JQueryLeft {
    override val toJsCmd =       
      "appendTo("+fixHtmlFunc("inline", content){str => str}+ ")"
  }

  /**
   * Prepend content to a JQuery
   */
  case class JqPrepend(content: NodeSeq) extends JsExp with JsMember with JQueryRight with JQueryLeft {
    override val toJsCmd = 
    "prepend(" + fixHtmlFunc("inline", content){str => str }+ ")"
  }

  /**
   * PrependTo content to a JQuery
   */
  case class JqPrependTo(content: NodeSeq) extends JsExp with JsMember with JQueryRight with JQueryLeft {
    override val toJsCmd = 
    "prependTo(" + fixHtmlFunc("inline", content){str => str} + ")"
  }

  /**
   * Set the css value of an element
   */
  case class JqCss (name: JsExp, value: JsExp) extends JsExp with JsMember with JQueryRight with JQueryLeft {
    override def toJsCmd = "css(" + name.toJsCmd + "," + value.toJsCmd + ")"
  }

  /**
   * EmptyAfter will empty the node at the given uid and stick the given content behind it. Like
   * a cleaner innerHTML.
   */
  case class JqEmptyAfter(content: NodeSeq) extends JsExp with JsMember with JQueryRight with JQueryLeft {
    override val toJsCmd = 
    "empty().after(" + fixHtmlFunc("inline", content){str => str} + ")"
  }

  /**
   * Replace the html node with content
   */
  case class JqReplace(content: NodeSeq) extends JsExp with JsMember {
    override val toJsCmd = fixHtmlCmdFunc("inline", content){"replaceWith(" + _ + ")"}
  }

  object JqHtml {
    def apply(): JsExp with JsMember with JQueryRight = new JsExp with JsMember with JQueryRight {
      def toJsCmd = "html()"
    }

    def apply(content: NodeSeq): JsExp with JsMember with JQueryRight with JQueryLeft = new JsExp with JsMember with JQueryRight with JQueryLeft {
      val toJsCmd = fixHtmlCmdFunc("inline", content){"html(" + _ + ")"}
    }
  }

  object JqText {
    /**
     * Get the combined text contents of each element in the set of matched elements, including their descendants.
     */
    def apply(): JsExp with JsMember with JQueryRight = new JsExp with JsMember with JQueryRight {
      def toJsCmd = "text()"
    }

    /**
     * Sets the content of an element to "content", html tags are escaped.
     */
    def apply(content: String): JsExp with JsMember with JQueryRight with JQueryLeft = new JsExp with JsMember with JQueryRight with JQueryLeft {
      def toJsCmd = "text(" + content.encJs + ")"
    }
  }

  /**
   * Serialize input elements intoa string data. ALso works for serializing forms
   */
  case object JqSerialize extends JsExp with JsMember with JQueryRight {
    def toJsCmd = "serialize()"
  }

  /**
   * Serialize the jquery into a JSON array
   */
  case object JsonSerialize extends JsExp with JsMember with JQueryRight {
    def toJsCmd = "serializeArray()"
  }

  case object JqTabsSelected extends JsExp with JsMember with JQueryRight {
    def toJsCmd = "tabsSelected()"
  }

  object JqTabsClick {
    def apply(tab: JsExp): JsExp with JsMember with JQueryRight with JQueryLeft =
      new JsExp with JsMember with JQueryRight with JQueryLeft {
        def toJsCmd = "tabsClick(" + tab.toJsCmd + ")"
      }

    def apply(tab: Int): JsExp with JsMember with JQueryRight with JQueryLeft =
      apply(Num(tab))
  }

  object JqTabs {
    def apply(in: JsExp): JsExp with JsMember with JQueryRight with JQueryLeft =
      new JsExp with JsMember with JQueryRight with JQueryLeft {
        def toJsCmd = "tabs(" + in.toJsCmd + ")"
      }

    def apply(): JsExp with JsMember with JQueryRight with JQueryLeft =
      apply(JsRaw(""))
  }

}

object JqJsCmds {
  implicit def jsExpToJsCmd(in: JsExp) = in.cmd

  /**
   * Sets the JavScript that will be executed when document is ready
   * for processing
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
   * Hide an element identified by uid
   */
  object Hide {
    /**
     * Hide an element identified by uid
     */
    def apply(uid: String) = new Hide(uid, Empty)

    /**
     * Hide an element identified by uid and the animation will last @time
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
   * Show a message @msg in the id @where for @duration milliseconds and fade out in @fadeout milliseconds
   *
   * @param where the id of where to show the message
   * @param msg the message as a NodeSeq
   * @param duration show the msessage for @duration in milliseconds or "slow", "fast"
   * @param fadeTime fadeout in @fadeout milliseconds or "slow", "fast"
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
   * for the given duration and fades out during fadeTime
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
   * Fade in an element with @id for @duration in milliseconds or "slow", "fast"
   * and use @fadeTime
   *
   * @param id
   * @param duration
   * @param fadeTime
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

  /**
   * Use SetValueAndFocus from JsCmds
   */
  @deprecated("Use SetValueAndFocus from JsCmds")
  case class SetValueAndFocus(id: String, value: String) extends JsCmd {
    def toJsCmd = "document.getElementById(" + id.encJs + ").value = " +
            value.encJs +
            "; document.getElementById(" + id.encJs + ").focus();"
  }

}
