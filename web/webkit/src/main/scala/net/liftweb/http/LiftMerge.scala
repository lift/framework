/*
 * Copyright 2009-2026 Lift Committers and Contributors
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

/**
 * Used to track state for LiftMerge's HTML normalization/merging process.
 */
private[this] case class HtmlState(
  htmlDescendant: Boolean = false, // any descendant of HTML
  headChild: Boolean = false, // direct child of a HEAD in its proper place
  bodyDescendant: Boolean = false, // any descendant of BODY
  headInBodyChild: Boolean = false, // direct child of a HEAD/HEAD_* somewhere in BODY
  tailInBodyChild: Boolean = false, // direct child of a TAIL somewhere in BODY
  bodyChild: Boolean = false, // direct child of body
  mergeHeadAndTail: Boolean // false if we're not doing head/tail merging
)

private[http] trait LiftMerge {
  self: LiftSession =>

  private def pageJsUrl = {
    S.encodeURL(s"${S.contextPath}/${LiftRules.pageJsFunc().mkString("/")}/${RenderVersion.get}.js")
  }

  // Gather all page-specific JS into one JsCmd.
  private[this] def assemblePageSpecificJavaScript(eventJs: JsCmd): JsCmd = {
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

    <script type="text/javascript" src={pageJsUrl}></script>
  }

  /**
   * Manages the merge phase of the rendering pipeline
   */
  def merge(xhtml: NodeSeq, req: Req): Node = {
    val snippetHashs: HashMap[String, Box[NodeSeq]] = this.deferredSnippets.is
    val waitUntil = millis + LiftRules.lazySnippetTimeout.vend.millis
    val stripComments: Boolean = LiftRules.stripComments.vend

    def waitUntilSnippetsDone(): Unit ={
      val myMillis = millis
      snippetHashs.synchronized {
        if (myMillis >= waitUntil || snippetHashs.isEmpty || !snippetHashs.values.iterator.contains(Empty)) ()
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
    val rewrite = URLRewriter.rewriteFunc

    val contextPath: String = S.contextPath

    def normalizeMergeAndExtractEvents(nodes: NodeSeq, startingState: HtmlState): NodesAndEventJs = {
      val HtmlState(htmlDescendant, headChild, bodyDescendant, headInBodyChild, tailInBodyChild, _bodyChild, mergeHeadAndTail) = startingState

      nodes.foldLeft(NodesAndEventJs(Vector[Node](), Noop)) {
        case (soFar, node) =>
          val childInfo =
            node match {
              case element: Elem if element.label == "html" && ! htmlDescendant =>
                htmlElement = element

                startingState.copy(htmlDescendant = true && mergeHeadAndTail)

              case element: Elem if element.label == "head" && htmlDescendant && ! bodyDescendant =>
                headElement = element

                startingState.copy(headChild = true && mergeHeadAndTail)

              case element: Elem if mergeHeadAndTail &&
                                    (element.label == "head" ||
                                      element.label.startsWith("head_")) &&
                                    htmlDescendant &&
                                    bodyDescendant =>
                startingState.copy(headInBodyChild = true)

              case element: Elem if mergeHeadAndTail &&
                                    element.label == "tail" &&
                                    htmlDescendant &&
                                    bodyDescendant =>
                startingState.copy(tailInBodyChild = true)

              case element: Elem if element.label == "body" && htmlDescendant =>
                bodyElement = element

                startingState.copy(bodyDescendant = true && mergeHeadAndTail, bodyChild = true && mergeHeadAndTail)

              case _ =>
                startingState.copy(headChild = false, headInBodyChild = false, tailInBodyChild = false, bodyChild = false)
            }

          val bodyHead = childInfo.headInBodyChild && ! headInBodyChild
          val bodyTail = childInfo.tailInBodyChild && ! tailInBodyChild

          HtmlNormalizer
            .normalizeNode(node, contextPath, stripComments, LiftRules.extractInlineJavaScript)
            .map {
              case normalized @ NodeAndEventJs(normalizedElement: Elem, _) =>
                val normalizedChildren =
                  normalizeMergeAndExtractEvents(normalizedElement.child, childInfo)

                normalized.copy(
                  normalizedElement.copy(child = normalizedChildren.nodes),
                  js = normalized.js & normalizedChildren.js
                )

              case other =>
                other
            }
            .map { (normalizedResults: NodeAndEventJs) =>
              node match {
                case e: Elem if e.label == "node" &&
                                e.prefix == "lift_deferred" =>
                  val deferredNodes: Seq[NodesAndEventJs] = {
                    for {
                      idAttribute <- e.attributes("id").take(1)
                      id = idAttribute.text
                      nodes <- processedSnippets.get(id)
                    } yield {
                      normalizeMergeAndExtractEvents(nodes, startingState)
                    }}.toSeq

                  deferredNodes.foldLeft(soFar.append(normalizedResults))(_ append _)

                case _ =>
                  if (headChild) {
                    headChildren ++= normalizedResults.node
                  } else if (headInBodyChild) {
                    addlHead ++= normalizedResults.node
                  } else if (tailInBodyChild) {
                    addlTail ++= normalizedResults.node
                  } else if (_bodyChild && ! bodyHead && ! bodyTail) {
                    bodyChildren ++= normalizedResults.node
                  }

                  if (bodyHead || bodyTail) {
                    soFar.append(normalizedResults.js)
                  } else {
                    soFar.append(normalizedResults)
                  }
              }
            } getOrElse {
              soFar
            }
        }
    }

    if (!hasHtmlHeadAndBody) {
      val NodesAndEventJs(fixedHtml, eventJs) =
        normalizeMergeAndExtractEvents(xhtml, HtmlState(mergeHeadAndTail = false))

      fixedHtml.collectFirst {
        case e: Elem =>
          val pageJs = assemblePageSpecificJavaScript(eventJs)
          val pageJsElement =
            List(pageJs)
              .filter(_.toJsCmd.trim.nonEmpty)
              .map(pageScopedScriptFileWith)

          e.copy(child = e.child.toSeq ++ pageJsElement)
      } getOrElse {
        Text("")
      }
    } else {
      val eventJs =
        normalizeMergeAndExtractEvents(xhtml, HtmlState(mergeHeadAndTail = true)).js

      val htmlKids = new ListBuffer[Node]

      val nl = Text("\n")

      for{
        node <- HeadHelper.removeHtmlDuplicates(addlHead.toList)
      } {
        headChildren += node
        headChildren += nl
      }

      // Appends ajax script to body
      if (LiftRules.autoIncludeAjaxCalc.vend()(this)) {
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
