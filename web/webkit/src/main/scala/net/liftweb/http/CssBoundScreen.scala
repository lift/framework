/*
 * Copyright 2011-2012 WorldWide Conferencing, LLC
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

import js.jquery.JqJE.JqId
import js.JsCmd
import js.JsCmds._
import js.jquery.JqJsCmds._
import js.JE
import net.liftweb.util._
import Helpers._
import net.liftweb.common._
import xml._

import FieldBinding._

trait CssBoundScreen extends ScreenWizardRendered with Loggable {
  self: AbstractScreen =>

  def formName: String

  def labelSuffix: NodeSeq = Text(":")

  protected lazy val cssClassBinding = new CssClassBinding

  protected val LocalActionRef: AnyVar[String, _]
  protected val LocalAction: AnyVar[String, _]
  protected val LocalActions: AnyVar[Map[String, () => JsCmd], _]

  val NextId: AnyVar[String, _]

  protected val PrevId: AnyVar[Box[String], _]
  protected val CancelId: AnyVar[String, _]

  protected def additionalFormBindings: Box[CssSel] = Empty

  private def traceInline[T](msg: => String, v: T): T = {
    logger.trace(msg)
    v
  }

  private object FieldBindingUtils {
    def sel(f: CssClassBinding => String, sel: String) = sel format (f(cssClassBinding))
    def replace(f: CssClassBinding => String) = ".%s" format (f(cssClassBinding))
    def replaceChildren(f: CssClassBinding => String) = ".%s *" format (f(cssClassBinding))

    def remove(f: CssClassBinding => String) =
      traceInline("Removing %s".format(f(cssClassBinding)), ".%s".format(f(cssClassBinding))) #> NodeSeq.Empty

    def nsSetChildren(f: CssClassBinding => String, value: NodeSeq) =
      traceInline("Binding %s to %s".format(replaceChildren(f), value), replaceChildren(f) #> value)

    def funcSetChildren(f: CssClassBinding => String, value: NodeSeq => NodeSeq) =
      traceInline("Binding %s to function".format(replaceChildren(f)), replaceChildren(f) #> value)

    def optSetChildren(f: CssClassBinding => String, value: Box[NodeSeq]) =
      traceInline("Binding %s to %s".format(replaceChildren(f), value), replaceChildren(f) #> value)

    def nsReplace(f: CssClassBinding=>String, value: NodeSeq) =
      traceInline("Binding %s to %s".format(replace(f), value), replace(f) #> value)

    def funcReplace(f: CssClassBinding=>String, value: NodeSeq => NodeSeq) =
      traceInline("Binding %s to function".format(replace(f)), replace(f) #> value)

    def optReplace(f: CssClassBinding=>String, value: Box[NodeSeq]) =
      traceInline("Binding %s to %s".format(replace(f), value), replace(f) #> value)

    def updateAttrs(metaData: MetaData): NodeSeq => NodeSeq = {
      case e:Elem => e % metaData
    }

    def update(f: CssClassBinding => String, metaData: MetaData) =
      traceInline("Update %s with %s".format(f(cssClassBinding), metaData),
        ".%s".format(f(cssClassBinding)) #> updateAttrs(metaData))
  }

  protected def bindLocalAction(selector: String, func: () => JsCmd): CssSel = {
    mapLocalAction(func)(name =>
      selector #> (
        SHtml.makeAjaxCall(LiftRules.jsArtifacts.serialize(NextId.get) + ("&" + LocalActionRef.get + "=" + name)).cmd
      ).toJsCmd)
  }

  protected def mapLocalAction[T](func: () => JsCmd)(f: String => T): T = {
    val name = randomString(20)
    LocalActions.set(LocalActions.is + (name -> func))
    f(name)
  }

  protected def setLocalAction(s: String) {
    logger.debug("Setting LocalAction (%s) to %s".format(
      Integer.toString(System.identityHashCode(LocalAction), 16), s))
    LocalAction.set(s)
  }

  override protected def renderAll(currentScreenNumber: Box[NodeSeq],
                          screenCount: Box[NodeSeq],
                          wizardTop: Box[Elem],
                          screenTop: Box[Elem],
                          fields: List[ScreenFieldInfo],
                          prev: Box[Elem],
                          cancel: Box[Elem],
                          next: Box[Elem],
                          finish: Box[Elem],
                          screenBottom: Box[Elem],
                          wizardBottom: Box[Elem],
                          nextId: (String, () => JsCmd),
                          prevId: Box[(String, () => JsCmd)],
                          cancelId: (String, () => JsCmd),
                          theScreen: AbstractScreen,
                          ajax_? : Boolean): NodeSeq = {

    import FieldBindingUtils._

    NextId.set(nextId._1)
    PrevId.set(prevId map (_._1))
    CancelId.set(cancelId._1)

    val notices: List[(NoticeType.Value, NodeSeq, Box[String])] = S.getAllNotices

    def fieldsWithStyle(style: BindingStyle, includeMissing: Boolean) =
      logger.trace("Looking for fields with style %s, includeMissing = %s".format(style, includeMissing),
        fields filter (field => field.binding map (_.bindingStyle == style) openOr (includeMissing)))

    def bindingInfoWithFields(style: BindingStyle) =
      logger.trace("Looking for fields with style %s".format(style),
        (for {
          field <- fields;
          bindingInfo <- field.binding if bindingInfo.bindingStyle == style
        } yield (bindingInfo, field)).toList)

    def templateFields: List[CssBindFunc] = List(sel(_.fieldContainer, ".%s") #> (fieldsWithStyle(Template, true) map (field => bindField(field))))

    def selfFields: List[CssBindFunc] =
      for ((bindingInfo, field) <- bindingInfoWithFields(Self))
      yield traceInline("Binding self field %s".format(bindingInfo.selector(formName)),
        bindingInfo.selector(formName) #> bindField(field))

    def defaultFields: List[CssBindFunc] =
      for ((bindingInfo, field) <- bindingInfoWithFields(Default))
      yield traceInline("Binding default field %s to %s".format(bindingInfo.selector(formName), defaultFieldNodeSeq),
        bindingInfo.selector(formName) #> bindField(field)(defaultFieldNodeSeq))

    def customFields: List[CssBindFunc] =
      for {
        field <- fields
        bindingInfo <- field.binding
        custom <- Some(bindingInfo.bindingStyle) collect { case c:Custom => c }
      } yield traceInline("Binding custom field %s to %s".format(bindingInfo.selector(formName), custom.template),
        bindingInfo.selector(formName) #> bindField(field)(custom.template))

    def bindFields: CssBindFunc = {
      logger.trace("Binding fields", fields)
      List(templateFields, selfFields, defaultFields, customFields).flatten.reduceLeft(_ & _)
    }

    def bindField(f: ScreenFieldInfo): NodeSeq => NodeSeq = {
      val theFormEarly = f.input
      val curId = theFormEarly.flatMap(Helpers.findId) or
        f.field.uniqueFieldId openOr Helpers.nextFuncName

      val theForm = theFormEarly.map{
        fe => {
          val f = Helpers.deepEnsureUniqueId(fe)
          val id = Helpers.findBox(f)(_.attribute("id").
            map(_.text).
            filter(_ == curId))
          if (id.isEmpty) {
            Helpers.ensureId(f, curId)
          } else {
            f
          }
        }
      }

      val myNotices = notices.filter(fi => fi._3.isDefined && fi._3 == curId)

      def bindLabel(): CssBindFunc = {
        val basicLabel = sel(_.label, ".%s [for]") #> curId & nsSetChildren(_.label, f.text ++ labelSuffix)
        myNotices match {
          case Nil => basicLabel
          case _ =>
            val maxN = myNotices.map(_._1).sortWith{_.id > _.id}.head // get the maximum type of notice (Error > Warning > Notice)
            val metaData: MetaData = noticeTypeToAttr(theScreen).map(_(maxN)) openOr Null
            basicLabel & update(_.label, metaData)
        }
      }

      def bindForm(): CssBindFunc =
        traceInline("Replacing %s with %s".format(replace(_.value), theForm),
          replace(_.value) #> theForm)

      def bindHelp(): CssBindFunc =
        f.help match {
          case Full(hlp) => nsSetChildren(_.help, hlp)
          case _ => remove(_.help)
        }

      def bindErrors(): CssBindFunc =
        myNotices match {
          case Nil => remove(_.errors)
          case xs => replaceChildren(_.errors) #> xs.map { case(noticeType, msg, _) =>
            val metaData: MetaData = noticeTypeToAttr(theScreen).map(_(noticeType)) openOr Null
            nsSetChildren(_.error, msg) & update(_.error, metaData)
          }
        }

      def bindAll() = bindLabel() & bindForm() & bindHelp() & bindErrors()

      f.transform map (func => bindAll() andThen func()) openOr (bindAll())
    }

    def url = S.uri

    val savAdditionalFormBindings = additionalFormBindings

    def bindErrors: CssBindFunc = notices.filter(_._3.isEmpty) match {
      case Nil => remove(_.globalErrors)
      case xs => replaceChildren(_.globalErrors) #> xs.map { case(noticeType, msg, _) =>
        val metaData: MetaData = noticeTypeToAttr(theScreen).map(_(noticeType)) openOr Null
        nsSetChildren(_.error, msg) & update(_.error, metaData)
      }
    }

    def bindFieldsWithAdditional(xhtml: NodeSeq) =
      (savAdditionalFormBindings map (bindFields & _) openOr (bindFields))(xhtml)

    def liftScreenAttr(s: String) =
      new UnprefixedAttribute("data-lift-screen-control", Text(s), Null)

    def bindForm(xhtml: NodeSeq): NodeSeq = {
      val fields = bindFieldsWithAdditional(xhtml)

      val snapshot = createSnapshot

      val ret =
        (<form id={nextId._1} action={url}
               method="post">{S.formGroup(-1)(SHtml.hidden(() =>
          snapshot.restore()) % liftScreenAttr("restoreAction"))}{fields}{
          S.formGroup(4)(
            SHtml.hidden(() =>
            {val res = nextId._2();
              if (!ajax_?) {
                val localSnapshot = createSnapshot
                S.seeOther(url, () => {
                  localSnapshot.restore
                })}
              res
            })) % liftScreenAttr("nextAction") }</form> %
          theScreen.additionalAttributes) ++
          prevId.toList.map{case (id, func) =>
            <form id={id} action={url} method="post">{
              SHtml.hidden(() => {snapshot.restore();
                val res = func();
                if (!ajax_?) {
                  val localSnapshot = createSnapshot;
                  S.seeOther(url, () => localSnapshot.restore)
                }
                res
              }) % liftScreenAttr("restoreAction")}</form>
          } ++
          <form id={cancelId._1} action={url} method="post">{SHtml.hidden(() => {
            snapshot.restore();
            val res = cancelId._2() // WizardRules.deregisterWizardSession(CurrentSession.is)
            if (!ajax_?) {
              S.seeOther(Referer.get)
            }
            res
          }) % liftScreenAttr("restoreAction")}</form>

      if (ajax_?) {
        SHtml.makeFormsAjax(ret)
      } else {
        ret
      }
    }

    def bindScreenInfo: CssBindFunc = (currentScreenNumber, screenCount) match {
      case (Full(num), Full(cnt)) =>
        replaceChildren(_.screenInfo) #> (nsSetChildren(_.screenNumber, num) & nsSetChildren(_.totalScreens, cnt))
      case _ => remove(_.screenInfo)
    }

    logger.trace("Preparing to bind", fields)

    val bindingFunc: CssBindFunc =
      bindScreenInfo &
      optSetChildren(_.wizardTop, wizardTop) &
      optSetChildren(_.screenTop, screenTop) &
      optSetChildren(_.wizardBottom, wizardBottom) &
      optSetChildren(_.screenBottom, screenBottom) &
      nsReplace(_.prev, prev openOr EntityRef("nbsp")) &
      nsReplace(_.next, ((next or finish) openOr EntityRef("nbsp"))) &
      nsReplace(_.cancel, cancel openOr EntityRef("nbsp")) &
      bindErrors &
      funcSetChildren(_.fields, bindForm _)

    val processed = S.session map (_.runTemplate("css-bound-screen", allTemplate)) openOr (allTemplate)

    (savAdditionalFormBindings map (bindingFunc & _) openOr (bindingFunc))(processed)
  }

  override protected def allTemplateNodeSeq: NodeSeq = {
    <div>
      <div class="screenInfo">
        Page <span class="screenNumber"></span> of <span class="totalScreens"></span>
      </div>
      <div class="wizardTop"></div>
      <div class="screenTop"></div>
      <div class="globalErrors">
        <div class="error"></div>
      </div>
      <div class="fields">
        <table>
          <tr class="fieldContainer">
            <td>
              <label class="label field"></label>
              <span class="help"></span>
              <div class="errors">
                <div class="error"></div>
              </div>
            </td>
            <td><span class="value fieldValue"></span></td>
          </tr>
        </table>
      </div>
      <div>
        <table>
          <tr>
            <td><button class="prev"></button></td>
            <td><button class="cancel"></button></td>
            <td><button class="next"></button> </td>
          </tr>
        </table>
      </div>
      <div class="screenBottom"></div>
      <div class="wizardBottom"></div>
    </div>
  }

  def defaultFieldNodeSeq: NodeSeq = NodeSeq.Empty

  class CssClassBinding {
    def screenInfo = "screenInfo"
    def wizardTop = "wizardTop"
    def screenTop = "screenTop"
    def globalErrors = "globalErrors"
    def fields = "fields"
    def fieldContainer = "fieldContainer"
    def label = "label"
    def help = "help"
    def errors = "errors"
    def error = "error"
    def value = "value"
    def prev = "prev"
    def cancel = "cancel"
    def next = "next"
    def screenBottom = "screenBottom"
    def wizardBottom = "wizardBottom"
    def screenNumber = "screenNumber"
    def totalScreens = "totalScreens"
  }
}
