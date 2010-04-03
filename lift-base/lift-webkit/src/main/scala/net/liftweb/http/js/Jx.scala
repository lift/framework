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

import _root_.scala.xml._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._

import JE._
import JsCmds._

trait JxYieldFunc {
  this: JxBase =>
  def yieldFunction: JsExp
}

trait JxBase {
  self: Node =>

  private val logger = Logger(classOf[JxBase])
  def appendToParent(parentName: String): JsCmd

  def label = "jx" 

  def addAttrs(varName: String, attrs: List[MetaData]): JsCmd = attrs.map {
    m =>
            m.value.map {
              case exp: JsExp =>
                JsRaw(varName + "." + m.key + " = " + exp.toJsCmd).cmd

              case cmd: JsCmd => val varName = "v" + Helpers.nextFuncName
              JsCrVar(varName, AnonFunc(cmd)) &
                      JsRaw(varName + "." + m.key + " = " + varName + "()")

              case JxAttr(cmd) =>
                JsRaw(varName + "." + m.key + " = " + cmd.toJsCmd).cmd

              case JxFuncAttr(cmd) =>
                JsRaw(varName + "." + m.key + " = " + AnonFunc(cmd).toJsCmd).cmd

              case x =>
                if (m.key == "class") {
                  // JsRaw(varName+".setAttribute('className',"+x.text.encJs+");").cmd

                  JsRaw(varName + ".className = " + x.text.encJs).cmd &
                          JsRaw(varName + ".setAttribute(" + m.key.encJs + "," + x.text.encJs + ");").cmd
                } else {
                  JsRaw(varName + ".setAttribute(" + m.key.encJs + "," + x.text.encJs + ");").cmd
                }
            }.foldLeft(Noop)(_ & _)
  }.foldLeft(Noop)(_ & _)

  private def fixText(in: String): String = (in, in.trim) match {
    case (x, y) if x == y => x
    case (x, y) if x startsWith y => y + " "
    case (x, y) if y.length == 0 => " "
    case (x, y) if x endsWith y => " " + y
    case (_, y) => " " + y + " "
  }

  def addToDocFrag(parent: String, elems: List[Node]): JsCmd = elems.map {
    case Jx(kids) => addToDocFrag(parent, kids.toList)
    case jb: JxBase => jb.appendToParent(parent)
    case Group(nodes) => addToDocFrag(parent, nodes.toList)
    case Text(txt) => JsRaw(parent + ".appendChild(document.createTextNode(" + fixText(txt).encJs + "));").cmd
    case a: Atom[_] => JsRaw(parent + ".appendChild(document.createTextNode(" + a.text.encJs + "));").cmd
    case e: _root_.scala.xml.Elem =>
      val varName = "v" + Helpers.nextFuncName
      JsCrVar(varName, JsRaw("document.createElement(" + e.label.encJs + ")")) &
              addAttrs(varName, e.attributes.toList) &
              JsRaw(parent + ".appendChild(" + varName + ")") &
              addToDocFrag(varName, e.child.toList)
    case ns: NodeSeq =>
      if (ns.length == 0) Noop
      else if (ns.length == 1) {
        logger.error("In addToDocFrag, got a " + ns + " of type " + ns.getClass.getName)
        Noop
      } else addToDocFrag(parent, ns.toList)

  }.foldLeft(Noop)(_ & _)
}


abstract class JxNodeBase extends Node with JxBase {
}

case class JxAttr(in: JsCmd) extends Node with JxBase {
  def child = Nil

  def appendToParent(parentName: String): JsCmd = {
    Noop
  }
}

case class JxFuncAttr(in: JsCmd) extends Node with JxBase {
  def child = Nil

  def appendToParent(parentName: String): JsCmd = {
    Noop
  }
}

case class JxMap(in: JsExp, what: JxYieldFunc) extends Node with JxBase {
  def child = Nil

  def appendToParent(parentName: String): JsCmd = {
    val ran = "v" + Helpers.nextFuncName
    val fr = "f" + Helpers.nextFuncName
    val cr = "c" + Helpers.nextFuncName
    JsCrVar(ran, in) &
            JsCrVar(fr, what.yieldFunction) &
            JsRaw("for (" + cr + " = 0; " + cr + " < " + ran + ".length; " + cr + "++) {" +
                    parentName + ".appendChild(" + fr + "(" + ran + "[" + cr + "]));" +
                    "}")
  }
}

case class JxCmd(in: JsCmd) extends Node with JxBase {
  def child = Nil

  def appendToParent(parentName: String) = in
}

case class JxMatch(exp: JsExp, cases: JxCase*) extends Node with JxBase {
  def child = Nil

  def appendToParent(parentName: String): JsCmd = {
    val vn = "v" + Helpers.nextFuncName
    JsCrVar(vn, exp) &
            JsRaw("if (false) {\n} " +
                    cases.map {
                      c =>
                              " else if (" + vn + " == " + c.toMatch.toJsCmd + ") {" +
                                      addToDocFrag(parentName, c.toDo.toList).toJsCmd +
                                      "\n}"
                    }.mkString("") +
                    " else {throw new Exception('Unmatched: '+" + vn + ");}")
  }
}

case class JxCase(toMatch: JsExp, toDo: NodeSeq)

case class JxIf(toTest: JsExp, ifTrue: NodeSeq) extends Node with JxBase {
  def child = Nil

  def appendToParent(parentName: String): JsCmd = {
    JsRaw("if (" + toTest.toJsCmd + ") {\n" +
            addToDocFrag(parentName, ifTrue.toList).toJsCmd +
            "}\n")
  }
}

case class JxIfElse(toTest: JsExp, ifTrue: NodeSeq, ifFalse: NodeSeq) extends Node with JxBase {
  def child = Nil

  def appendToParent(parentName: String): JsCmd = {
    JsRaw("if (" + toTest.toJsCmd + ") {\n" +
            addToDocFrag(parentName, ifTrue.toList).toJsCmd +
            "} else {\n" +
            addToDocFrag(parentName, ifFalse.toList).toJsCmd +
            "}\n")
  }
}



case class Jx(child: NodeSeq) extends Node with JxBase with JxYieldFunc {
  def appendToParent(parentName: String): JsCmd =
    addToDocFrag(parentName, child.toList)

  def yieldFunction: JsExp = toJs

  def toJs: JsExp = AnonFunc("it",
    JsCrVar("df", JsRaw("document.createDocumentFragment()")) &
            addToDocFrag("df", child.toList) &
            JsRaw("return df"))


}

}
}
}
