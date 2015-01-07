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

import scala.collection.mutable.{HashMap, ArrayBuffer, ListBuffer}
import scala.xml._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http.js._
import Helpers._

private[http] trait LiftMerge {
  self: LiftSession =>

  private def scriptUrl(scriptFile: String) = {
    S.encodeURL(s"${LiftRules.liftPath}/$scriptFile")
  }

  // Gather all page-specific JS into one JsCmd.
  private def assemblePageSpecificJavaScript: JsCmd = {
    val allJs =
      LiftRules.javaScriptSettings.vend().map { settingsFn =>
        LiftJavaScript.initCmd(settingsFn(this))
      }.toList ++
      S.jsToAppend

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


    var htmlTag = <html xmlns="http://www.w3.org/1999/xhtml" xmlns:lift='http://liftweb.net'/>
    var headTag = <head/>
    var bodyTag = <body/>
    val headChildren = new ListBuffer[Node]
    val bodyChildren = new ListBuffer[Node]
    val addlHead = new ListBuffer[Node]
    addlHead ++= S.forHead()
    val addlTail = new ListBuffer[Node]
    addlTail ++= S.atEndOfBody()
    val rewrite = URLRewriter.rewriteFunc
    val fixHref = Req.fixHref

    val contextPath: String = S.contextPath

    def fixAttrs(original: MetaData, toFix: String, attrs: MetaData, fixURL: Boolean): MetaData = attrs match {
      case Null => Null
      case u: UnprefixedAttribute if u.key == toFix =>
        new UnprefixedAttribute(toFix, fixHref(contextPath, attrs.value, fixURL, rewrite), fixAttrs(original, toFix, attrs.next, fixURL))
      case _ => attrs.copy(fixAttrs(original, toFix, attrs.next, fixURL))

    }

    def _fixHtml(in: NodeSeq, _inHtml: Boolean, _inHead: Boolean, _justHead: Boolean, _inBody: Boolean, _justBody: Boolean, _bodyHead: Boolean, _bodyTail: Boolean, doMergy: Boolean): NodeSeq = {
      in.flatMap {
        v =>
                var inHtml = _inHtml
                var inHead = _inHead
                var justHead = false
                var justBody = false
                var inBody = _inBody
                var bodyHead = false
                var bodyTail = false

                v match {
                  case e: Elem if e.label == "html" &&
                  !inHtml => htmlTag = e; inHtml = true && doMergy

                  case e: Elem if e.label == "head" && inHtml &&
                  !inBody => headTag = e;
                  inHead = true && doMergy; justHead = true && doMergy

                  case e: Elem if (e.label == "head" ||
                                   e.label.startsWith("head_")) &&
                  inHtml && inBody => bodyHead = true && doMergy

                  case e: Elem if e.label == "tail" && inHtml &&
                  inBody => bodyTail = true && doMergy

                  case e: Elem if e.label == "body" && inHtml =>
                    bodyTag = e; inBody = true && doMergy;
                  justBody = true && doMergy

                  case _ =>
                }

                val ret: NodeSeq = v match {
                  case Group(nodes) => Group(_fixHtml(nodes, inHtml, inHead, justHead, inBody, justBody, bodyHead, bodyTail, doMergy))

                  // if it's a deferred node, grab it from the deferred list
                  case e: Elem if e.label == "node" && e.prefix == "lift_deferred" =>
                    for{
                      attr <- e.attributes("id").headOption.map(_.text).toList
                      nodes <- processedSnippets.get(attr).toList
                      node <- _fixHtml(nodes, inHtml, inHead, justHead, inBody, justBody, bodyHead, bodyTail, doMergy)
                    } yield node

                  case e: Elem if e.label == "form" => Elem(v.prefix, v.label, fixAttrs(v.attributes, "action", v.attributes, true), v.scope, e.minimizeEmpty, _fixHtml(v.child, inHtml, inHead, justHead, inBody, justBody, bodyHead, bodyTail, doMergy): _*)
                  case e: Elem if e.label == "script" => Elem(v.prefix, v.label, fixAttrs(v.attributes, "src", v.attributes, false), v.scope,  e.minimizeEmpty, _fixHtml(v.child, inHtml, inHead, justHead, inBody, justBody, bodyHead, bodyTail, doMergy): _*)
                  case e: Elem if e.label == "a" => Elem(v.prefix, v.label, fixAttrs(v.attributes, "href", v.attributes, true), v.scope,  e.minimizeEmpty, _fixHtml(v.child, inHtml, inHead, justHead, inBody, justBody, bodyHead, bodyTail, doMergy): _*)
                  case e: Elem if e.label == "link" => Elem(v.prefix, v.label, fixAttrs(v.attributes, "href", v.attributes, false), v.scope,  e.minimizeEmpty,_fixHtml(v.child, inHtml, inHead, justHead, inBody, justBody, bodyHead, bodyTail, doMergy): _*)
                  case e: Elem => Elem(v.prefix, v.label, fixAttrs(v.attributes, "src", v.attributes, true), v.scope,  e.minimizeEmpty, _fixHtml(v.child, inHtml, inHead, justHead, inBody, justBody, bodyHead, bodyTail, doMergy): _*)
                  case c: Comment if stripComments => NodeSeq.Empty
                  case _ => v
                }
                if (_justHead) headChildren ++= ret
                else if (_justBody && !bodyHead && !bodyTail) bodyChildren ++= ret
                else if (_bodyHead) addlHead ++= ret
                else if (_bodyTail) addlTail ++= ret

                if (bodyHead || bodyTail) Text("")
                else ret
      }
    }

    if (!hasHtmlHeadAndBody) {
      val fixedHtml = _fixHtml(xhtml, false, false, false, false, false, false, false, false)

      fixedHtml.find {
        case e: Elem => true
        case _ => false
      } getOrElse Text("")
    } else {
      _fixHtml(xhtml, false, false, false, false, false, false, false, true)

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

      val pageJs = assemblePageSpecificJavaScript
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
      htmlKids += headTag.copy(child = headChildren.toList)
      htmlKids += nl
      htmlKids += bodyAttributes.foldLeft(bodyTag.copy(child = bodyChildren.toList))(_ % _)
      htmlKids += nl

      val tmpRet = Elem(htmlTag.prefix, htmlTag.label, htmlTag.attributes, htmlTag.scope, htmlTag.minimizeEmpty, htmlKids.toList: _*)

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

