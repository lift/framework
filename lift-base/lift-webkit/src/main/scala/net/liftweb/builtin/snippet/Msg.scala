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

import _root_.net.liftweb.http._
import _root_.net.liftweb.http.S._
import _root_.scala.xml._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.common.{Full, Empty}
import _root_.scala.collection.mutable.HashMap
import  _root_.net.liftweb.http.js._
import JsCmds._

/**
 * This class is a built in snippet that allows rendering only notices (Errors, Warnings, Notices)
 * that are associated with the id provided. This snippet also renders effects configured for the
 * given id. Typically this will be used near by form fields to indicate that a certain field
 * failed the validation. For example:
 *
 * <pre name="code" class="xml">
 *   &lt;input type="text" value="" name="132746123548765"/&gt;&lt;lift:msg id="user_msg"/&gt;
 * </pre>
 *
 * Additionally, you may specify additional CSS classes to be applied to each type of notice
 * using the followin attributes:
 *
 * <ul>
 *   <li>errorClass</li>
 *   <li>warningClass</li>
 *   <li>noticeClass</li>
 * </ul>
 * 
 * <pre name="code" class="xml">
 *   &lt;input type="text" value="" name="132746123548765"/&gt;&lt;lift:msg id="user_msg"
 *                                                        errorClass="error_class"
 *                                                        warningClass="warning_class"
 *                                                        noticeClass="notice_class"/&gt;
 * </pre>
 *
 * Notices for specific ids are set via the S.error(String,String) or S.error(String,NodeSeq)
 * methods. Global (non-id) notices are rendered via the Msgs snippet. 
 *
 * @see net.liftweb.builtin.snippet.Msgs
 * @see net.liftweb.http.S#error(String,String)
 * @see net.liftweb.http.S#error(String,NodeSeq)
 * @see net.liftweb.http.LiftRules#noticesEffects
 */
object Msg extends DispatchSnippet {
  def dispatch: DispatchIt = {
    case _ => render
  }

  /**
   * This method performs extraction of the CSS class attributes
   * as well as rendering of any messages for the given id.
   */
  def render(styles: NodeSeq): NodeSeq = {
    attr("id") match {
      case Full(id) => {
        // Extract the currently set CSS
        attr("errorClass").map(cls => MsgErrorMeta += (id -> cls))
        attr("warningClass").map(cls => MsgWarningMeta += (id -> cls))
        attr("noticeClass").map(cls => MsgNoticeMeta += (id -> cls))

        renderIdMsgs(id) ++ effects(id)
      }
      case _ => NodeSeq.Empty
    }
  }

  /**
   * This method renders the &lt;span/> for a given id's notices,
   * along with any effects configured for the id.
   *
   * @see net.liftweb.http.S#error(String,String)
   * @see net.liftweb.http.S#error(String,NodeSeq)
   * @see net.liftweb.http.LiftRules#noticesEffects
   */
  def renderIdMsgs(id : String) : NodeSeq = {
    val msgs : List[NodeSeq] = List((S.messagesById(id)(S.errors), MsgErrorMeta.get.get(id)),
                                    (S.messagesById(id)(S.warnings), MsgWarningMeta.get.get(id)),
                                    (S.messagesById(id)(S.notices), MsgNoticeMeta.get.get(id))).flatMap {
      case (msg, style) =>
        msg.toList match {
          case Nil => Nil
          case msgList => style match {
            case Some(s) => msgList.flatMap(t => <span>{t}</span> % ("class" -> s))
            case _ => msgList flatMap ( n => n )
          }
        }
    }

    // Join multiple messages together with a comma
    val commafied = msgs match {
      case Nil => Text("")
      case spans => spans.reduceLeft {
        (output,span) => output ++ Text(", ") ++ span
      }
    }

    <span>{commafied}</span> % ("id" -> id)
  }

  /**
   * This method renders a <script/> element that renders effects for
   * the given id.
   *
   * @see net.liftweb.builtin.snippet.Msgs#effects[T](Box[NoticeType.Value],String,T,Box[JsCmd => T])
   */
  def effects(id: String): NodeSeq = 
    Msgs.effects(Empty, id, NodeSeq.Empty, Full(Msgs.tailScript))
}

/**
 * This SessionVar holds a map of per-id CSS classes for error notices
 * so that the AJAX and static renderers use the same formatting.
 */
object MsgErrorMeta extends SessionVar[HashMap[String, String]](new HashMap) {
    override private[liftweb] def magicSessionVar_? = true
}

/**
 * This SessionVar holds a map of per-id CSS classes for warning notices
 * so that the AJAX and static renderers use the same formatting.
 */
object MsgWarningMeta extends SessionVar[HashMap[String, String]](new HashMap) {
    override private[liftweb] def magicSessionVar_? = true
}

/**
 * This SessionVar holds a map of per-id CSS classes for notice notices
 * so that the AJAX and static renderers use the same formatting.
 */
object MsgNoticeMeta extends SessionVar[HashMap[String, String]](new HashMap) {
    override private[liftweb] def magicSessionVar_? = true
}

// Close nested packages
}
}
}
