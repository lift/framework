/*
 * Copyright 2009-2011 WorldWide Conferencing, LLC
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

import scala.collection.Map
import scala.collection.mutable.{HashMap, ArrayBuffer, ListBuffer}
import scala.xml._

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http.js._
  import JsCmds.Noop
  import JE.{AnonFunc,Call,JsRaw}
import Helpers._

private[http] trait LiftMerge {
  self: LiftSession =>

  private def scriptUrl(scriptFile: String) = {
    S.encodeURL(s"${LiftRules.liftPath}/$scriptFile")
  }

  // Gather all page-specific JS into one JsCmd.
  private def assemblePageSpecificJavaScript(eventJs: JsCmd): JsCmd = {
    val allJs =
      LiftRules.javaScriptSettings.vend().map { settingsFn =>
        LiftJavaScript.initCmd(settingsFn(this))
      }.toList ++
      S.jsToAppend() ++
      List(eventJs)

    allJs.foldLeft(js.JsCmds.Noop)(_ & _)
  }

  private def pageScopedScriptFileWith(cmd: JsCmd) = {
    pageScript(Full(JavaScriptResponse(cmd, Nil, Nil, 200)))

    <script type="text/javascript" src={scriptUrl(s"page/${RenderVersion.get}.js")}></script>
  }

  /**
   * Manages the merge phase of the rendering pipeline
   */
  def merge(xhtml: NodeSeq, req: Req): Node = {
    val snippetHashs: HashMap[String, Box[NodeSeq]] = this.deferredSnippets.is
    val waitUntil = millis + LiftRules.lazySnippetTimeout.vend.millis
    val stripComments: Boolean = LiftRules.stripComments.vend

    def waitUntilSnippetsDone() {
      val myMillis = millis
      snippetHashs.synchronized {
        if (myMillis >= waitUntil || snippetHashs.isEmpty || !snippetHashs.values.toIterator.contains(Empty)) ()
        else {
          snippetHashs.wait(waitUntil - myMillis)
          waitUntilSnippetsDone()
        }
      }
    }

    waitUntilSnippetsDone()

    val processedSnippets: Map[String, NodeSeq] = Map(snippetHashs.toList.flatMap {
      case (name, Full(value)) => List((name, value))
      case (name, f: Failure) => List((name, LiftRules.deferredSnippetFailure.vend(f)))
      case (name, Empty) => List((name, LiftRules.deferredSnippetTimeout.vend))
      case _ => Nil
    }: _*)

    val hasHtmlHeadAndBody: Boolean = xhtml.find {
      case e: Elem if e.label == "html" =>
        e.child.find {
          case e: Elem if e.label == "head" => true
          case _ => false
        }.isDefined &&
                e.child.find {
                  case e: Elem if e.label == "body" => true
                  case _ => false
                }.isDefined
      case _ => false
    }.isDefined


    var htmlElement = <html xmlns="http://www.w3.org/1999/xhtml" xmlns:lift='http://liftweb.net'/>
    var headElement = <head/>
    var bodyElement = <body/>
    val headChildren = new ListBuffer[Node]
    val bodyChildren = new ListBuffer[Node]
    val addlHead = new ListBuffer[Node]
    addlHead ++= S.forHead()
    val addlTail = new ListBuffer[Node]
    addlTail ++= S.atEndOfBody()
    val eventAttributesByElementId = new HashMap[String,List[EventAttribute]]
    val rewrite = URLRewriter.rewriteFunc

    val contextPath: String = S.contextPath

    case class HtmlState(
      inHtml: Boolean = false, // once we've seen HTML
      inHead: Boolean = false, // once we've seen HEAD
      inBody: Boolean = false, // once we've seen BODY
      inHeadInBody: Boolean = false, // we've seen HEAD inside BODY
      inTailInBody: Boolean = false, // we've seen TAIL inside BODY
      justBody: Boolean = false, // we're in a descendant of BODY (not HEAD/TAIL)
      justHead: Boolean = false, // we're in a descendant of HEAD (not inside BODY)
      mergeHeadAndTail: Boolean = false
    )

    def _fixHtml(in: NodeSeq, startingState: HtmlState = HtmlState()): (NodeSeq, JsCmd) = {
      normalizeHtmlAndEventHandlers(
        in,
        startingState,
        {
          case (state @ HtmlState(_inHtml, _inHead, _inBody, _inHeadInBody, _inTailInBody, _justBody, _justHead, mergeHeadAndTail), element) =>
            var inHtml = _inHtml
            var inHead = _inHead
            var enteredHead = false
            var enteredBody = false
            var inBody = _inBody
            var bodyHead = false
            var bodyTail = false

            val childInfo =
              element.label match {
                case "html" if ! inHtml =>
                  htmlElement = element

                  state.copy(inHtml = true && mergeHeadAndTail)

                case "head" if inHtml && ! inBody =>
                  enteredHead = true && mergeHeadAndTail
                  headElement = element

                  state.copy(inHead = enteredHead)

                case label if mergeHeadAndTail && 
                              (label == "head" || label.startsWith("head_")) &&
                              inHtml &&
                              inBody =>
                  state.copy(inHeadInBody = true)

                case label if mergeHeadAndTail &&
                              label == "tail" &&
                              inHtml &&
                              inBody =>
                  state.copy(inTailInBody = true)

                case "body" if inHtml =>
                  enteredBody = true && mergeHeadAndTail
                  bodyElement = element

                  state.copy(inBody = enteredBody)

                case _ =>
                  state
              }

            element match {
              case e: Elem if e.label == "node" &&
                              e.prefix == "lift_deferred" =>
                val deferredNodes: Seq[(NodeSeq, JsCmd)] =
                  for {
                    idAttribute <- e.attributes("id").take(1)
                    id = idAttribute.text
                    nodes <- processedSnippets.get(id)
                  } yield {
                    _fixHtml(nodes, childInfo)
                  }

                (
                  childInfo,
                  deferredNodes.flatMap(_._1),
                  deferredNodes.map(_._2).foldLeft(Noop)(_ & _)
                )

              case _ =>
                if (_justHead) {
                  headChildren ++= element
                } else if (_justBody && !bodyHead && !bodyTail) {
                  bodyChildren ++= element
                } else if (_inHeadInBody) {
                  addlHead ++= element
                } else if (_inTailInBody) {
                  addlTail ++= element
                }

                if (bodyHead || bodyTail) {
                  (childInfo, NodeSeq.Empty, JsCmds.Noop)
                } else {
                  (childInfo, element, JsCmds.Noop)
                }
            }
        }: (HtmlState, Elem)=>(HtmlState, NodeSeq, JsCmd)
      )
    }

    if (!hasHtmlHeadAndBody) {
      val (fixedHtml, _) = _fixHtml(xhtml)

      fixedHtml.find {
        case e: Elem => true
        case _ => false
      } getOrElse Text("")
    } else {
      val (_, eventJs) = _fixHtml(xhtml, HtmlState(mergeHeadAndTail = true))

      val htmlKids = new ListBuffer[Node]

      val nl = Text("\n")

      for{
        node <- HeadHelper.removeHtmlDuplicates(addlHead.toList)
      } {
        headChildren += node
        headChildren += nl
      }

      // Appends ajax script to body
      if (LiftRules.autoIncludeAjaxCalc.vend().apply(this)) {
        bodyChildren +=
                <script src={S.encodeURL(contextPath + "/"+LiftRules.resourceServerPath+"/lift.js")}
                type="text/javascript"/>
        bodyChildren += nl
      }

      val pageJs = assemblePageSpecificJavaScript(eventJs)
      if (pageJs.toJsCmd.trim.nonEmpty) {
        addlTail += pageScopedScriptFileWith(pageJs)
      }

      for {
        node <- HeadHelper.removeHtmlDuplicates(addlTail.toList)
      } bodyChildren += node

      bodyChildren += nl

      val autoIncludeComet = LiftRules.autoIncludeComet(this)
      val bodyAttributes: List[(String, String)] =
        if (stateful_? && (autoIncludeComet || LiftRules.enableLiftGC)) {
          ("data-lift-gc" -> RenderVersion.get) ::
          (
            if (autoIncludeComet) {
              ("data-lift-session-id" -> (S.session.map(_.uniqueId) openOr "xx")) ::
              S.requestCometVersions.is.toList.map {
                case CometVersionPair(guid, version) =>
                  (s"data-lift-comet-$guid" -> version.toString)
              }
            } else {
              Nil
            }
          )
        } else {
          Nil
        }

      htmlKids += nl
      htmlKids += headElement.copy(child = headChildren.toList)
      htmlKids += nl
      htmlKids += bodyAttributes.foldLeft(bodyElement.copy(child = bodyChildren.toList))(_ % _)
      htmlKids += nl

      val tmpRet = Elem(htmlElement.prefix, htmlElement.label, htmlElement.attributes, htmlElement.scope, htmlElement.minimizeEmpty, htmlKids.toList: _*)

      val ret: Node = if (Props.devMode) {
        LiftRules.xhtmlValidator.toList.flatMap(_(tmpRet)) match {
          case Nil => tmpRet
          case xs =>
            import scala.xml.transform._

            val errors: NodeSeq = xs.map(e =>
                    <div style="border: red solid 2px">XHTML Validation error:{e.msg}at line{e.line + 1}and column{e.col}</div>)

            val rule = new RewriteRule {
              override def transform(n: Node) = n match {
                case e: Elem if e.label == "body" =>
                  e.copy(child = e.child ++ errors)

                case x => super.transform(x)
              }
            }
            (new RuleTransformer(rule)).transform(tmpRet)(0)
        }

      } else tmpRet

      ret
    }
  }
}

