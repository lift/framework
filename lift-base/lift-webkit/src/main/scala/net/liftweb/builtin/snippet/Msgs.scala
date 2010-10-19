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
package builtin {
package snippet {

import _root_.net.liftweb.http.{DispatchSnippet,LiftRules,NoticeType,S,SessionVar}
import _root_.scala.xml._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.http.js._
import JsCmds._
import _root_.net.liftweb.common.{Box, Full, Empty}


/**
 * This built in snippet renders messages (Errors, Warnings, Notices) in a <i>div</i>.
 * Typically it is used in templates as a place holder for any messages that are <b>not</b> associated with an ID.
 * Setting the attribute <i>showAll</i> to <i>true</i> will render all messages, with and without an ID.
 * This will lead to duplicate messages if additionally the <i>Msg</i> built in snippet is used to show
 * messages associated with an ID.
 *
 * E.g. (child nodes are optional)
 * <pre name="code" class="xml">
 * &lt;lift:Msgs showAll="false"&gt;
 *   &lt;lift:error_msg class="errorBox" &gt;Error!  The details are:&lt;/lift:error_msg&gt;
 *   &lt;lift:warning_msg&gt;Whoops, I had a problem:&lt;/lift:warning_msg&gt;
 *   &lt;lift:warning_class&gt;warningBox&lt;/lift:warning_class&gt;
 *   &lt;lift:notice_msg&gt;Note:&lt;/lift:notice_msg&gt;
 *   &lt;lift:notice_class&gt;noticeBox&lt;/lift:notice_class&gt;
 * &lt;/lift:snippet&gt;
 * </pre>
 *
 * JavaScript fadeout and effects for the three types of notices (Errors, Warnings and Notices) can
 * be configured via LiftRules.noticesAutoFadeOut and LiftRules.noticesEffects. Notices for individual
 * elements based on id can be rendered using the &lt;lift:msg/> tag.
 *
 * @see net.liftweb.builtin.snippet.Msg
 * @see net.liftweb.http.LiftRules#noticesAutoFadeOut
 * @see net.liftweb.http.LiftRules#noticesEffects
 */
object Msgs extends DispatchSnippet {
  // Dispatch to the render method no matter how we're called
  def dispatch: DispatchIt = {
    case _ => render
  }

  /**
   * This method performs extraction of custom formatting and then
   * renders the current notices.
   *
   * @see #renderNotices()
   */
  def render(styles: NodeSeq): NodeSeq = {
    // Capture the value for later AJAX updates
    ShowAll(toBoolean(S.attr("showAll")))

    // Extract user-specified titles and CSS classes for later use
    List((NoticeType.Error, MsgsErrorMeta),
         (NoticeType.Warning, MsgsWarningMeta),
         (NoticeType.Notice, MsgsNoticeMeta)).foreach {
      case (noticeType, ajaxStorage) => {
        // Extract the title if provided, or default to none
        val title : String = (styles \\ noticeType.titleTag).
          filter(_.prefix == "lift").map(_.text.trim).headOption getOrElse ""
  
        // Extract any provided classes for the messages
        val cssClasses = ((styles \\ noticeType.styleTag) ++
                          (styles \\ noticeType.titleTag \\ "@class")).
          toList.map(_.text.trim) match {
            case Nil => Empty
            case classes => Full(classes.mkString(" "))
          }
  
        // Save the settings for AJAX usage
        ajaxStorage(Full(AjaxMessageMeta(title,cssClasses)))
      }
    }

    // Delegate the actual rendering to a shared method so that we don't
    // duplicate code for the AJAX pipeline
    (<div>{renderNotices()}</div> % ("id" -> LiftRules.noticesContainerId)) ++
    noticesFadeOut(NoticeType.Notice) ++
    noticesFadeOut(NoticeType.Warning) ++
    noticesFadeOut(NoticeType.Error) ++
    effects(NoticeType.Notice) ++
    effects(NoticeType.Warning) ++
    effects(NoticeType.Error)
  }

  /**
   * This method renders the current notices to XHtml based on
   * the current user-specific formatting from the &lt;lift:Msgs> tag.
   */
  def renderNotices() : NodeSeq = {
    // Determine which formatting function to use based on tag usage
    val f = 
      if (ShowAll.is) {
        S.messages _
      } else {
        S.noIdMessages _
      }

    // Compute the formatted set of messages for a given input
    def computeMessageDiv (args : (List[(NodeSeq,Box[String])],NoticeType.Value,SessionVar[Box[AjaxMessageMeta]])) : NodeSeq = args match {
      case (messages,noticeType,ajaxStorage) =>

      // get current settings
      val title = ajaxStorage.get.map(_.title) openOr ""
      val styles = ajaxStorage.get.flatMap(_.cssClasses)

      // Compute the resulting div
      f(messages).toList.map(e => (<li>{e}</li>) ) match {
        case Nil => Nil
        case msgList => {
          val ret = <div id={noticeType.id}>{title}<ul>{msgList}</ul></div>
          styles.foldLeft(ret)((xml, style) => xml % new UnprefixedAttribute("class", Text(style), Null))
        }
      }
    }

    // Render all three types together
    List((S.errors, NoticeType.Error, MsgsErrorMeta),
         (S.warnings, NoticeType.Warning, MsgsWarningMeta),
         (S.notices, NoticeType.Notice, MsgsNoticeMeta)).flatMap(computeMessageDiv)
  }

  // This wraps the JavaScript fade and effect scripts into a script that runs onLoad
  private[snippet] def tailScript (script : JsCmd) : NodeSeq =
    <lift:tail>{Script(OnLoad(script))}</lift:tail>

  /**
   * This method produces appropriate JavaScript to fade out the given
   * notice type. The caller must provide a default value for cases where
   * fadeout is not configured, and may optionally provide a wrapping
   * function to transform the output.
   *
   * @see net.liftweb.http.LiftRules.noticesAutoFadeOut
   */
  def noticesFadeOut[T](noticeType: NoticeType.Value, default : T, wrap : Box[JsCmd => T]): T =
    LiftRules.noticesAutoFadeOut()(noticeType) flatMap {
      case (duration, fadeTime) => {
        wrap.map(_(LiftRules.jsArtifacts.fadeOut(noticeType.id, duration, fadeTime)))
      }
    } openOr default

  /**
   * This method produces an appropriate <script> tag to fade out the given
   * notice type.
   *
   * @see net.liftweb.http.LiftRules.noticesAutoFadeOut
   */
  def noticesFadeOut(noticeType: NoticeType.Value): NodeSeq = 
    noticesFadeOut(noticeType, NodeSeq.Empty, Full(tailScript))

  /**
   * This method produces appropriate JavaScript to apply effects to the given
   * notice type. The caller must provide a default value for cases where
   * effects are not configured, and may optionally provide a wrapping
   * function to transform the output.
   *
   * @see net.liftweb.http.LiftRules.noticesEffects
   */
  def effects[T](noticeType: Box[NoticeType.Value], id : String, default : T, wrap : Box[JsCmd => T]): T = 
    LiftRules.noticesEffects()(noticeType, id) match {
      case Full(jsCmd) => wrap.map(_(jsCmd)) openOr default
      case _ => default
    }

  /**
   * This method produces an appropriate <script> tag to apply effects to
   * the given notice type.
   *
   * @see net.liftweb.http.LiftRules.noticesEffects
   */
  def effects(noticeType: NoticeType.Value): NodeSeq =
    effects(Full(noticeType), noticeType.id, NodeSeq.Empty, Full(tailScript))
}

/**
 * This SessionVar holds formatting data for notice notices
 * so that the AJAX and static notice renderers use the same formatting.
 */
object MsgsNoticeMeta extends SessionVar[Box[AjaxMessageMeta]](Empty) {
    override private[liftweb] def magicSessionVar_? = true
}

/**
 * This SessionVar holds formatting data for warning notices
 * so that the AJAX and static notice renderers use the same formatting.
 */
object MsgsWarningMeta extends SessionVar[Box[AjaxMessageMeta]](Empty) {
    override private[liftweb] def magicSessionVar_? = true
}

/**
 * This SessionVar holds formatting data for error notices
 * so that the AJAX and static notice renderers use the same formatting.
 */
object MsgsErrorMeta extends SessionVar[Box[AjaxMessageMeta]](Empty) {
    override private[liftweb] def magicSessionVar_? = true
}

/**
 * This SessionVar records whether to show id-based messages in
 * addition to non-id messages.
 */
object ShowAll extends SessionVar[Boolean](false) {
    override private[liftweb] def magicSessionVar_? = true
}

/**
 * This case class is used to hold formatting data for the
 * notice groups so that AJAX and static notices
 * render consistently.
 */
case class AjaxMessageMeta(title: String, cssClasses: Box[String])

// Close nested packages
}
}
}
