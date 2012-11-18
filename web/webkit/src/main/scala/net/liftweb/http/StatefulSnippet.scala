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

import net.liftweb.common._
import net.liftweb._
import util._
import Helpers._
import xml.{NodeSeq, Elem}

/**
 * The same StatefulSnippet instance is used across a given page rendering.
 * <br/>
 * If the StatefulSnippet is used to render a form, a hidden field is added to
 * the form that causes the same instance to be used on the page that is the
 * target of the form submission.
 * <br/>
 * If you want to keep the same snippet for a page rendered via a link (&lt;a
 * href...&gt;) use the StatefulSnippet.link method to create the link.  This will
 * cause the registerThisSnippet method to be called and the same instance will
 * be used on the target page.
 * <pre>
 * class CountGame extends StatefulSnippet  {
 *  val dispatch: DispatchIt =  {
 *    case "run" => run _
 * }
 *
 *  def run(xhtml: NodeSeq): NodeSeq =  {
 *    if (lastGuess == number)  {
 *      bind("count", chooseTemplate("choose", "win", xhtml), "number" --> number, "count" --> count)
 * } else  {
 *      bind("count", chooseTemplate("choose", "guess", xhtml),
 *        "input" --> text("", guess _),
 *        "last" --> lastGuess.map(v => if (v < number) v+" is low" else v+"is high").openOr("Make first Guess")
 *      )
 * }
 *
 *  private def guess(in: String)  {
 *    count += 1
 *    lastGuess = Full(toInt(in))
 * }
 *
 *  private val number = 1 + randomInt(100)
 *  private var lastGuess: Box[Int] = Empty
 *  private var count = 0
 *
 * }
 * </pre>
 */
trait StatefulSnippet extends DispatchSnippet {
  private[this] var _names: Set[String] = Set()
  def addName(name: String) {
    synchronized {
      _names = _names + name
    }
  }

  def names: Set[String] = synchronized {
    _names
  }

  def registerThisSnippet() = names.foreach(n => S.overrideSnippetForClass(n, this))

  def unregisterThisSnippet() =  names.foreach(n => S.unsetSnippetForClass(n))

  /**
   * create an anchor tag around a body
   *
   * @param to - the target
   * @param func - the function to invoke when the link is clicked
   * @param body - the NodeSeq to wrap in the anchor tag
   * @param attrs - the (optional) attributes for the HTML element
   */
  def link(to: String, func: () => Any, body: NodeSeq, attrs: SHtml.ElemAttr*): Elem =
    SHtml.link(to, () => { registerThisSnippet(); func() }, body, attrs: _*)

  /**
   * Redirect to another page, but make sure this StatefulSnippet is registered
   * on that page so the state continues on the new page
   */
  def redirectTo(where: String): Nothing = S.redirectTo(where, registerThisSnippet)

  /**
   * See Other to another page, but make sure this StatefulSnippet is registered
   * on that page so the state continues on the new page
   */
  def seeOther(where: String): Nothing = S.seeOther(where, registerThisSnippet)

  /**
   * Merge the SHtml into the form
   */
  private[http] def mergeIntoForm(isForm: Boolean, res: NodeSeq, toMerge: => NodeSeq): NodeSeq = {
    val formElem = Helpers.findOption(res){
      case e: Elem if e.label == "form" && null == e.prefix=> Some(e)
      case _ => None
    }

    if (formElem.isDefined) {
      import util.Helpers._

      ("form *" #> ((kids: NodeSeq) => toMerge ++ kids)).apply(res)
    } else if (isForm) {
      toMerge ++ res
    } else {
      res
    }
  }

}

/**
 * Mix this into a StatefulSnippet if you want a defined render method.
 */
trait RenderDispatch {
  /**
   * The predefined dispatch
   */
  def dispatch: PartialFunction[String, NodeSeq => NodeSeq] = Map("render" -> render _)

  /**
   * You have to define this method
   */
  def render(in: NodeSeq): NodeSeq

}

/**
 * Mix this into a StatefulSnippet if you want a defined render method.  Differs
 * from RenderDispatch because the render method returns a NodeSeq => NodeSeq
 */
trait RenderFuncDispatch {
  /**
   * The predefined dispatch
   */
  def dispatch: PartialFunction[String, NodeSeq => NodeSeq] = Map("render" -> render)

  /**
   * You have to define this method
   */
  def render: NodeSeq => NodeSeq
}

/**
 * The simple composition of StatefulSnippet, Whence and RenderFuncDispatch.
 * This is the common use of stateful snippets and makes things easier.
 */
trait SimpleStateful extends StatefulSnippet with Whence with RenderFuncDispatch

trait DispatchSnippet {
  type DispatchIt = PartialFunction[String, NodeSeq => NodeSeq]

  def dispatch: DispatchIt
}

/**
 * This trait indicates if the snippet instance should be kept around for the duration of
 * the Request.  There are cases when you don't want a snippet to be kept around.
 */
trait TransientSnippet {

  /**
   * Calculate if this snippet should be treated as transiet.
   */
  def transient_? = true
}

/**
 * The companion object to the TransientSnippet trait
 */
object TransientSnippet {
  /**
   * Compute if the instance should be treated as transient
   */
  def notTransient(obj: Any): Boolean = obj match {
    case t: TransientSnippet => !t.transient_?
    case _ => true
  }
}

/**
 * Mix this snippet into any snippet.  If the snippet is invoked in response to a
 * stateless request, then the behavior method is called with the method name of
 * the snippet (usually render, but there may be others if you specify a method
 * after the snippet name: MySnippet.dothing).
 */
trait StatelessBehavior {
  /**
   * Given the method name, return the transformation for the method
   */
  def statelessDispatch: PartialFunction[String, NodeSeq => NodeSeq]
}

/**
 * A simpler way to define behavior if the snippet is invoked.  Just implement the stateless method
 * and all methods for the snippet will use that behavior.
 */
trait SimpleStatelessBehavior extends StatelessBehavior {
  def stateless: NodeSeq => NodeSeq
  def statelessDispatch: PartialFunction[String, NodeSeq => NodeSeq] = {
    case _ => stateless
  }
}

/**
 * A "default" implementation of StatelessBehavior.  Just ignore everything and return an empty NodeSeq.
 */
trait BlankStatelessBehavior extends StatelessBehavior {
  def statelessDispatch: PartialFunction[String, NodeSeq => NodeSeq] = {
    case _ => _ => NodeSeq.Empty
  }
}
