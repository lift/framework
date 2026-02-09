/*
 * Copyright 2007-2026 Lift Committers and Contributors
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

import scala.xml._
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
   * If you need to provide a custom `renderer` function, perhaps because you
   * need to do further wrapping beyond what `buildDeferredFunction` gives you,
   * then you can invoke this `render` method directly and pass it a function
   * that will take the id of the placeholder container and render whatever
   * needs to be rendered to a `JsCmd`. The snippet will:
   *  - Set up the placeholder markup with a unique id.
   *  - If the placeholder template `NodeSeq` isn't specified, first see if
   *    `S.attr("template")` is set and use that template if avilable, and then
   *    fall back on the default placeholder template.
   *  - Handle invoking the `AsyncRenderComet` correctly.
   *
   * The `renderer` function will be wrapped in current request state you must
   * do this manually using `buildDeferredFunction`. This method is for advanced
   * use; most folks will probably want to interact with the snippet by just
   * wrapping their snippet invocation in a `data-lift="lazy-load"` snippet.
   */
  def render(renderer: (String)=>JsCmd, placeholderTemplate: Box[NodeSeq] = Empty): NodeSeq = {
    val placeholderId = Helpers.nextFuncName

    handleMarkupBox(
      AsyncRenderComet.asyncRender(()=>renderer(placeholderId)).map { _ =>
        ("^ [id]" #> placeholderId).apply(
          placeholderTemplate or
          {
            for {
              templatePath <- S.attr("template")
              renderedTemplate <- S.eval(<lift:embed what={templatePath} />)
            } yield {
              renderedTemplate
            }
          } openOr {
            <div><img src={s"${LiftRules.assetRootPath}images/ajax-loader.gif"} alt="Loading"/></div>
          }
        )
      }
    )
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
   *
   * If you invoke this from Scala rather than markup, you can optionally
   * provide a `placeholderTemplate` that is a `NodeSeq` that should be used
   * while the async rendering takes place.
   */
  def render(xhtml: NodeSeq, placeholderTemplate: Box[NodeSeq]): NodeSeq = {
    render(Replace(_, xhtml), placeholderTemplate)
  }

  def render(xhtml: NodeSeq): NodeSeq = {
    render(xhtml, Empty)
  }

  // Helper to deal with Boxed markup.
  private def handleMarkupBox(markup: Box[NodeSeq]): NodeSeq = {
    markup match {
      case Full(html) => html
      case Failure(msg, _, _) => Comment(msg)
      case Empty => Comment("FIX"+"ME: Asynchronous rendering failed for unknown reason.")
    }
  }
}
