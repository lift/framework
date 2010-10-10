/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
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

import _root_.scala.collection.mutable.{HashMap, ArrayBuffer, ListBuffer}
import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http.js._
import Helpers._


private[http] trait LiftMerge {
  self: LiftSession =>

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
    val addlTail = new ListBuffer[Node]
    val cometTimes = new ListBuffer[CometVersionPair]
    val rewrite = URLRewriter.rewriteFunc
    val fixHref = Req.fixHref

    val contextPath: String = S.contextPath

    def fixAttrs(original: MetaData, toFix: String, attrs: MetaData, fixURL: Boolean): MetaData = attrs match {
      case Null => Null
      case p: PrefixedAttribute if p.key == "when" && p.pre == "lift" =>
        val when = p.value.text
        original.find(a => !a.isPrefixed && a.key == "id").map {
          id =>
                  cometTimes += CVP(id.value.text, when.toLong)
        }
        fixAttrs(original, toFix, p.next, fixURL)
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
                  case e: Elem if e.label == "html" && !inHtml => htmlTag = e; inHtml = true && doMergy
                  case e: Elem if e.label == "head" && inHtml && !inBody => headTag = e; inHead = true && doMergy; justHead = true && doMergy
                  case e: Elem if e.label == "head" && inHtml && inBody => bodyHead = true && doMergy
                  case e: Elem if e.label == "tail" && inHtml && inBody => bodyTail = true && doMergy
                  case e: Elem if e.label == "body" && inHtml => bodyTag = e; inBody = true && doMergy; justBody = true && doMergy

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

                  case e: Elem if e.label == "form" => Elem(v.prefix, v.label, fixAttrs(v.attributes, "action", v.attributes, true), v.scope, _fixHtml(v.child, inHtml, inHead, justHead, inBody, justBody, bodyHead, bodyTail, doMergy): _*)
                  case e: Elem if e.label == "script" => Elem(v.prefix, v.label, fixAttrs(v.attributes, "src", v.attributes, false), v.scope, _fixHtml(v.child, inHtml, inHead, justHead, inBody, justBody, bodyHead, bodyTail, doMergy): _*)
                  case e: Elem if e.label == "a" => Elem(v.prefix, v.label, fixAttrs(v.attributes, "href", v.attributes, true), v.scope, _fixHtml(v.child, inHtml, inHead, justHead, inBody, justBody, bodyHead, bodyTail, doMergy): _*)
                  case e: Elem if e.label == "link" => Elem(v.prefix, v.label, fixAttrs(v.attributes, "href", v.attributes, false), v.scope, _fixHtml(v.child, inHtml, inHead, justHead, inBody, justBody, bodyHead, bodyTail, doMergy): _*)
                  case e: Elem => Elem(v.prefix, v.label, fixAttrs(v.attributes, "src", v.attributes, true), v.scope, _fixHtml(v.child, inHtml, inHead, justHead, inBody, justBody, bodyHead, bodyTail, doMergy): _*)
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

      // Appends ajax stript to body
      if (LiftRules.autoIncludeAjax(this)) {
        bodyChildren +=
                <script src={S.encodeURL(contextPath + "/" +
                        LiftRules.ajaxPath +
                        "/" + LiftRules.ajaxScriptName())}
                type="text/javascript"/>
        bodyChildren += nl
      }

      val cometList = cometTimes.toList

      // Appends comet stript reference to head
      if (!cometList.isEmpty && LiftRules.autoIncludeComet(this)) {
        bodyChildren +=
                <script src={S.encodeURL(contextPath + "/" +
                        LiftRules.cometPath +
                        "/" + urlEncode(this.uniqueId) +
                        "/" + LiftRules.cometScriptName())}
                type="text/javascript"/>
        bodyChildren += nl
      }

      for{
        node <- HeadHelper.removeHtmlDuplicates(addlTail.toList)
      } bodyChildren += node

      bodyChildren += nl

      if (!cometList.isEmpty && LiftRules.autoIncludeComet(this)) {
        bodyChildren += JsCmds.Script(LiftRules.renderCometPageContents(this, cometList))
        bodyChildren += nl
      }

      if (LiftRules.enableLiftGC && stateful_?) {
        import js._
        import JsCmds._
        import JE._

        bodyChildren += JsCmds.Script((if (hasFuncsForOwner(RenderVersion.get)) OnLoad(JsRaw("liftAjax.lift_successRegisterGC()")) else Noop) &
                JsCrVar("lift_page", RenderVersion.get))
      }

      htmlKids += nl
      htmlKids += Elem(headTag.prefix, headTag.label, headTag.attributes, headTag.scope, headChildren.toList: _*)
      htmlKids += nl
      htmlKids += Elem(bodyTag.prefix, bodyTag.label, bodyTag.attributes, bodyTag.scope, bodyChildren.toList: _*)
      htmlKids += nl

      val tmpRet = Elem(htmlTag.prefix, htmlTag.label, htmlTag.attributes, htmlTag.scope, htmlKids.toList: _*)

      val ret: Node = if (Props.devMode) {
        LiftRules.xhtmlValidator.toList.flatMap(_(tmpRet)) match {
          case Nil => tmpRet
          case xs =>
            import _root_.scala.xml.transform._

            val errors: NodeSeq = xs.map(e =>
                    <div style="border: red solid 2px">XHTML Validation error:{e.msg}at line{e.line + 1}and column{e.col}</div>)

            val rule = new RewriteRule {
              override def transform(n: Node) = n match {
                case e: Elem if e.label == "body" =>
                  Elem(e.prefix, e.label, e.attributes, e.scope, e.child ++ errors: _*)

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

}
}
