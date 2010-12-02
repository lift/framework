/*
 * Copyright 2010 WorldWide Conferencing, LLC
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

import _root_.net.liftweb.util._
import js._
import JsCmds._
import _root_.scala.xml.{NodeSeq, Elem, Text}

/**
 * Surface a user interface on top of Wiring
 */
object WiringUI {
  /**
   * Given a NodeSeq, a Cell and a function that can generate
   * a NodeSeq => NodeSeq from the cell's value, register the
   * postPageJavaScript that will update the element with
   * a new value.
   *
   * @param in the NodeSeq that contains the view markup
   * @param cell the cell to associate with
   * @param f the function that performs the drawing
   *
   * @return the mutated NodeSeq (an id attribute may be added if
   * there's none already defined)
   */
  def apply[T](in: NodeSeq, cell: Cell[T])(f: T => NodeSeq => NodeSeq): NodeSeq = toNode(in, cell)((t, ns) => f(t)(ns))

  /**
   * Given a NodeSeq, a Cell and a function that can generate
   * a NodeSeq => NodeSeq from the cell's value, register the
   * postPageJavaScript that will update the element with
   * a new value.
   *
   * @param in the NodeSeq that contains the view markup
   * @param cell the cell to associate with
   * 
   * @param jsEffect a function that wraps the SetHtml JsCmd so
   * you can, for example, fade out the old value, set the new value and
   * fade it in.  The first parameter is the id of the element, the
   * second is a flag that's true if this is the first time the element is
   * being rendered (you might want to skip effects for the inital page load),
   * and the third parameter is the SetHtml JavaScript code.
   * 
   * @param f the function that performs the drawing
   *
   * @return the mutated NodeSeq (an id attribute may be added if
   * there's none already defined)
   */
  def apply[T](in: NodeSeq, cell: Cell[T], jsEffect: (String, Boolean, JsCmd) => JsCmd)(f: T => NodeSeq => NodeSeq): NodeSeq = toNode(in, cell, jsEffect)((t, ns) => f(t)(ns))

  /**
   * Given a NodeSeq, a Cell and a function that can generate
   * a NodeSeq from the cell's value and the template value, register the
   * postPageJavaScript that will update the element with
   * a new value.
   *
   * @param in the NodeSeq that contains the view markup
   * @param cell the cell to associate with
   * @param f the function that performs the drawing
   * 
   * @return the mutated NodeSeq (an id attribute may be added if
   * there's none already defined)
   */
  def toNode[T](in: NodeSeq, cell: Cell[T])(f: (T, NodeSeq) => NodeSeq): NodeSeq = toNode(in, cell, (id, first, js) => js)(f)

  /**
   * Given a NodeSeq, a Cell register the
   * postPageJavaScript that will update the element with
   * a new value.
   *
   * @param in the NodeSeq that contains the view markup
   * @param cell the cell to associate with
   * 
   * @return the mutated NodeSeq (an id attribute may be added if
   * there's none already defined)
   */
  def asText[T](in: NodeSeq, cell: Cell[T]): NodeSeq = 
    toNode(in, cell, (id, first, js) => js)((t, ns) => Text(t.toString))

  /**
   * Given a NodeSeq, a Cell register the
   * postPageJavaScript that will update the element with
   * a new value.
   *
   * @param in the NodeSeq that contains the view markup
   * @param cell the cell to associate with
   * 
   * @param jsEffect a function that wraps the SetHtml JsCmd so
   * you can, for example, fade out the old value, set the new value and
   * fade it in.  The first parameter is the id of the element, the
   * second is a flag that's true if this is the first time the element is
   * being rendered (you might want to skip effects for the inital page load),
   * and the third parameter is the SetHtml JavaScript code.
   * 
   * @return the mutated NodeSeq (an id attribute may be added if
   * there's none already defined)
   */
  def asText[T](in: NodeSeq, cell: Cell[T], jsEffect: (String, Boolean, JsCmd) => JsCmd): NodeSeq = 
    toNode(in, cell, jsEffect)((t, ns) => Text(t.toString))

  /**
   * Given a NodeSeq, a Cell and a function that can generate
   * a NodeSeq from the cell's value and the template value, register the
   * postPageJavaScript that will update the element with
   * a new value.
   *
   * @param in the NodeSeq that contains the view markup
   * @param cell the cell to associate with
   * 
   * @param jsEffect a function that wraps the SetHtml JsCmd so
   * you can, for example, fade out the old value, set the new value and
   * fade it in.  The first parameter is the id of the element, the
   * second is a flag that's true if this is the first time the element is
   * being rendered (you might want to skip effects for the inital page load),
   * and the third parameter is the SetHtml JavaScript code.
   * 
   * @param f the function that performs the drawing
   * 
   * @return the mutated NodeSeq (an id attribute may be added if
   * there's none already defined)
   */
  def toNode[T](in: NodeSeq, cell: Cell[T], jsEffect: (String, Boolean, JsCmd) => JsCmd)(f: (T, NodeSeq) => NodeSeq): NodeSeq = {
    val myElem: Elem = in.find {
      case e: Elem => true
      case _ => false
    }.map(_.asInstanceOf[Elem]).getOrElse(<span id={Helpers.nextFuncName}>{in}</span>)

    val (elem: Elem, id: String) = Helpers.findOrAddId(myElem)
    addJsFunc(cell, (t: T, first: Boolean) => {
      jsEffect(id, first, SetHtml(id, f(t, elem.child)))
    })
    elem
  }

  /**
   * Associate a Cell and a function that converts from the
   * cell's value to a JavaScript command to be sent to the
   * browser as part of the page's post-processing.
   *
   * @param cell the cell to associate the JavaScript to
   * @param f the function that takes the cell's value and a flag indicating
   * if this is the first time 
   */
  def addJsFunc[T](cell: Cell[T], f: (T, Boolean) => JsCmd) {
    val trc = TransientRequestCell(cell)
    var lastTime: Long = 0L
    var lastValue: T = null.asInstanceOf[T]
    for {
      sess <- S.session
    } sess.addPostPageJavaScript(() => {
      val (value, ct) = trc.get
      val first = lastTime == 0L
      if (first || (ct > lastTime && value != lastValue)) {
        lastValue = value
        lastTime = ct
        f(value, first)
      } else Noop
    })
  }
}

/**
 * Cache the value of the cell for the duration of the transient request
 */
private final case class TransientRequestCell[T](cell: Cell[T]) extends TransientRequestVar[(T, Long)](cell.currentValue) {
  override val __nameSalt = Helpers.nextFuncName
}

}
}
