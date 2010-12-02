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
package http {
package js {
package jquery {

import _root_.scala.xml.{NodeSeq, Group, Elem, Node, SpecialNode}
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util.Helpers
import _root_.net.liftweb.util.TimeHelpers
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._

import _root_.net.liftweb.http.js.{JsExp, JE}
import JE._
import JsCmds._

/**
 * Classes mixing JQueryRight are also mixing JsMember. As JQueryRight
 * is deprecated clases mixing this trait with stop doing so soon and they
 * will mixin JsMember instead.
 */
@deprecated
trait JQueryRight {
  this: JsExp =>
  def toJsCmd: String
}
/**
 * Classes mixing JQuryLeft will soon stop doing so. Extending/Mixing JsExp will be enough
 */ 
@deprecated
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
}

object JqJE {
  case object JqScrollToBottom extends JsExp with JsMember with JQueryRight with JQueryLeft {
    def toJsCmd = "each(function(i) {this.scrollTop=this.scrollHeight;})"
  }

  case class JqClick(exp: JsExp) extends JsExp with JsMember with JQueryLeft with JQueryRight {
    def toJsCmd = "click(" + exp.toJsCmd + ")"
  }

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

  case class JqAttr(key: String, value: JsExp) extends JsExp with JsMember with JQueryRight with JQueryLeft {
    def toJsCmd = "attr(" + key.encJs + ", " + value.toJsCmd + ")"
  }

  /**
   * Append content to a JQuery
   */
  case class JqAppend(content: NodeSeq) extends JsExp with JsMember with JQueryRight with JQueryLeft {
    override val toJsCmd = "append(" + fixHtml("inline", content) + ")"
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
    override val toJsCmd = "appendTo(" + fixHtml("inline", content) + ")"
  }

  /**
   * Prepend content to a JQuery
   */
  case class JqPrepend(content: NodeSeq) extends JsExp with JsMember with JQueryRight with JQueryLeft {
    override val toJsCmd = "prepend(" + fixHtml("inline", content) + ")"
  }

  /**
   * PrependTo content to a JQuery
   */
  case class JqPrependTo(content: NodeSeq) extends JsExp with JsMember with JQueryRight with JQueryLeft {
    override val toJsCmd = "prependTo(" + fixHtml("inline", content) + ")"
  }

  case class JqCss (name: JsExp, value: JsExp) extends JsExp with JsMember with JQueryRight with JQueryLeft {
    override def toJsCmd = "css(" + name.toJsCmd + "," + value.toJsCmd + ")"
  }

  /**
   * EmptyAfter will empty the node at the given uid and stick the given content behind it. Like
   * a cleaner innerHTML.
   */
  case class JqEmptyAfter(content: NodeSeq) extends JsExp with JsMember with JQueryRight with JQueryLeft {
    override val toJsCmd = "empty().after(" + fixHtml("inline", content) + ")"
  }

  object JqHtml {
    def apply() = new JsExp with JsMember with JQueryRight {
      def toJsCmd = "html()"
    }

    def apply(content: NodeSeq) = new JsExp with JsMember with JQueryRight with JQueryLeft {
      val toJsCmd = "html(" + fixHtml("inline", content) + ")"
    }
  }

  object JqText {
    def apply() = new JsExp with JsMember with JQueryRight {
      def toJsCmd = "text()"
    }

    def apply(content: String) = new JsExp with JsMember with JQueryRight with JQueryLeft {
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


  case class JqSetHtml(uid: String, content: NodeSeq) extends JsCmd {
    /**
     * Eagerly evaluate
     */
    val toJsCmd =
    "try{jQuery(" + ("#" + uid).encJs + ").each(function(i) {this.innerHTML = " + fixHtml(uid, content) + ";});} catch (e) {}"
  }

  object Show {
    def apply(uid: String) = new Show(uid, Empty)

    def apply(uid: String, time: TimeSpan) = new Show(uid, Full(time))
  }

  class Show(val uid: String, val time: Box[TimeSpan]) extends JsCmd with HasTime {
    def toJsCmd = "try{jQuery(" + ("#" + uid).encJs + ").show(" + timeStr + ");} catch (e) {}"
  }

  object Hide {
    def apply(uid: String) = new Hide(uid, Empty)

    def apply(uid: String, time: TimeSpan) = new Hide(uid, Full(time))
  }

  class Hide(val uid: String, val time: Box[TimeSpan]) extends JsCmd with HasTime {
    def toJsCmd = "try{jQuery(" + ("#" + uid).encJs + ").hide(" + timeStr + ");} catch (e) {}"
  }

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

  case class FadeIn(id: String, duration: TimeSpan, fadeTime: TimeSpan) extends JsCmd {
    def toJsCmd = (After(duration, JqJE.JqId(id) ~> (new JsRaw("fadeIn(" + fadeTime.millis + ")") with JsMember))).toJsCmd
  }

  object ModalDialog {
    def apply(html: NodeSeq) = new ModalDialog(html, Empty)

    def apply(html: NodeSeq, css: JsObj) = new ModalDialog(html, Full(css))
  }

  class ModalDialog(html: NodeSeq, css: Box[JsObj]) extends JsCmd {
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

    val toJsCmd = "jQuery.blockUI({ message: " + contentAsJsStr +
            (css.map(",  css: " + _.toJsCmd + " ").openOr("")) + "});"
  }

  case object Unblock extends JsCmd {
    def toJsCmd = "jQuery.unblockUI();"
  }

  /**
   * Use SetValueAndFocus from JsCmds
   */
  @deprecated
  case class SetValueAndFocus(id: String, value: String) extends JsCmd {
    def toJsCmd = "document.getElementById(" + id.encJs + ").value = " +
            value.encJs +
            "; document.getElementById(" + id.encJs + ").focus();"
  }

}

}
}
}
}
