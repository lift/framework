/*
 * Copyright 2007-2012 WorldWide Conferencing, LLC
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
package builtin
package snippet

import xml._
import http._
import common._
import actor._
import util._
import http.js._
import JsCmds._
import JE._
import S._
import Helpers._

import comet.AsyncRenderComet

/**
 * Enclose your snippet tags on your template with LazyLoad and the snippet will execute
 * on a different thread, which avoids blocking the page render.
 */
object LazyLoad extends DispatchSnippet {
  def dispatch: DispatchIt = {
    case _ => render _
  }

  /**
   * Enclose your snippet like this:
   *
   * {{{
   *   <div data-lift="lazy-load">
   *     <div data-lift="MyLongRunningSnippet"></div>
   *   </div>
   * }}}
   *
   * You can also add the `template` attribute to the `lazy-load` snippet to
   * specify what to render as a placeholder:
   *
   * {{{
   *   <div data-lift="lazy-load?template=my-nice-wait-message-template">
   *     <div data-lift="MyLongRunningSnippet"></div>
   *   </div>
   * }}}
   *
   * Note that this must be a single element that will be replaced when the lazy
   * content is loaded. By default, an AJAX spinner is displayed inside a `div`.
   */
  def render(xhtml: NodeSeq): NodeSeq = {
    val placeholderId = Helpers.nextFuncName

    AsyncRenderComet.asyncRender(()=>Replace(placeholderId, xhtml)).map { _ =>
      ("^ [id]" #> placeholderId).apply(
        {
          for {
            templatePath <- S.attr("template")
            renderedTemplate <- S.eval(<lift:embed what={templatePath} />)
          } yield {
            renderedTemplate
          }
        } openOr {
          <div><img src="/images/ajax-loader.gif" alt="Loading"/></div>
        }
      )
    } match {
      case Full(placeholderHtml) => placeholderHtml
      case Failure(msg, _, _) => Comment(msg)
      case Empty => Comment("FIX"+"ME: Asynchronous rendering failed for unknown reason.")
    }
  }
}
