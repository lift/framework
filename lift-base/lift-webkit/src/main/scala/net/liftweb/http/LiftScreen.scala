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

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._

import _root_.scala.xml._
import _root_.scala.reflect.Manifest

private[liftweb] trait AbstractScreen extends Factory {
  override def toString = screenName

  @volatile private[this] var _fieldList: List[() => FieldContainer] = Nil

  /**
   * any additional parameters that need to be put in the on the form (e.g., mime type)
   */
  def additionalAttributes: MetaData =
    if (hasUploadField) new UnprefixedAttribute("enctype", Text("multipart/form-data"), Null) else Null


  protected def _register(field: () => FieldContainer) {
    _fieldList = _fieldList ::: List(field)
  }

  protected def hasUploadField: Boolean = screenFields.foldLeft(false)(_ | _.uploadField_?)

  /**
   *  A list of fields in this screen
   */
  def screenFields: List[BaseField] = _fieldList.flatMap(_.apply().allFields)


  def screenTop: Box[Elem] = Empty

  def screenBottom: Box[Elem] = Empty

  /**
   * The name of the screen.  Override this to change the screen name
   */
  def screenName: String = "Screen"

  def screenNameAsHtml: NodeSeq = Text(screenName)

  def screenTitle: NodeSeq = screenNameAsHtml

  def screenTopText: Box[String] = Empty

  def screenTopTextAsHtml: Box[NodeSeq] = screenTopText.map(Text.apply)


  def cancelButton: Elem = <button>{S.??("Cancel")}</button>

  def finishButton: Elem = <button>{S.??("Finish")}</button>


  implicit def boxOfScreen[T <: AbstractScreen](in: T): Box[T] = Box !! in


  def validate: List[FieldError] = screenFields.flatMap(_.validate)

  protected def vendForm[T](implicit man: Manifest[T]): Box[(T, T => Unit) => NodeSeq] = Empty

  protected def vendAVar[T](dflt: => T): NonCleanAnyVar[T]

  trait Field extends BaseField {
    AbstractScreen.this._register(() => this)

    private val _currentValue: NonCleanAnyVar[ValueType] =
    vendAVar[ValueType](setFilter.foldLeft(default)((nv, f) => f(nv)))

    def default: ValueType

    def is = _currentValue.is

    /**
     * Set to true if this field is part of a multi-part mime upload
     */
    override def uploadField_? = false

    def get = is

    def set(v: ValueType) = _currentValue.set(setFilter.foldLeft(v)((nv, f) => f(nv)))

    implicit def manifest: Manifest[ValueType]

    protected def buildIt[T](implicit man: Manifest[T]): Manifest[T] = man

    def help: Box[String] = Empty

    override def helpAsHtml: Box[NodeSeq] = help.map(Text.apply)

    /**
     *  Is the field editable
     */
    def editable_? = true

    def toForm: Box[NodeSeq] = {
      val func: Box[(ValueType, ValueType => Unit) => NodeSeq] =
      AbstractScreen.this.vendForm(manifest) or otherFuncVendors(manifest) or
          LiftRules.vendForm(manifest)

      func.map(f => f(is, set _))
    }

    protected def otherFuncVendors(what: Manifest[ValueType]): Box[(ValueType, ValueType => Unit) => NodeSeq] = Empty


    def validate: List[FieldError] = validations.flatMap(_.apply(is))

    def validations: List[ValueType => List[FieldError]] = Nil

    def setFilter: List[ValueType => ValueType] = Nil

    override lazy val uniqueFieldId: Box[String] = Full(Helpers.hash(this.getClass.getName))

    override def toString = is.toString


  }
}


trait LiftScreen extends AbstractScreen with DispatchSnippet {
  def dispatch = {
    case _ => ignore => this.toForm
  }

  private object ScreenVars extends RequestVar[Map[String, (NonCleanAnyVar[_], Any)]](Map())
  private object PrevSnapshot extends RequestVar[Box[ScreenSnapshot]](Empty)
  private object Referer extends ScreenVar[String](S.referer openOr "/")
  private object FirstTime extends ScreenVar[Boolean](true)

  protected class ScreenSnapshot(private[http] val screenVars: Map[String, (NonCleanAnyVar[_], Any)],
                                 private[http] val snapshot: Box[ScreenSnapshot]) {
    def restore() {
      ScreenVars.set(screenVars)
      PrevSnapshot.set(snapshot)
    }
  }

  protected def vendAVar[T](dflt: => T): NonCleanAnyVar[T] = new ScreenVar[T](dflt) {
    override protected def __nameSalt = randomString(20)
  }

  protected def createSnapshot = new ScreenSnapshot(ScreenVars.is, PrevSnapshot.is)

  /**
   * Keep request-local information around without the nastiness of naming session variables
   * or the type-unsafety of casting the results.
   * RequestVars share their value through the scope of the current HTTP
   * request. They have no value at the beginning of request servicing
   * and their value is discarded at the end of request processing. They
   * are helpful to share values across many snippets.
   *
   * @param dflt - the default value of the session variable
   */
  abstract class ScreenVar[T](dflt: => T) extends NonCleanAnyVar[T](dflt) {
    override protected def findFunc(name: String): Box[T] = ScreenVarHandler.get(name)

    override protected def setFunc(name: String, value: T): Unit = ScreenVarHandler.set(name, this, value)

    override protected def clearFunc(name: String): Unit = ScreenVarHandler.clear(name)

    override protected def wasInitialized(name: String): Boolean = {
      val bn = name + "_inited_?"
      val old: Boolean = ScreenVarHandler.get(bn) openOr false
      ScreenVarHandler.set(bn, this, true)
      old
    }

    override protected def testWasSet(name: String): Boolean = {
      val bn = name + "_inited_?"
      ScreenVarHandler.get(name).isDefined || (ScreenVarHandler.get(bn) openOr false)
    }

    /**
     * Different Vars require different mechanisms for synchronization. This method implements
     * the Var specific synchronization mechanism
     */
    def doSync[F](f: => F): F = f // no sync necessary for RequestVars... always on the same thread
  }


  private object ScreenVarHandler {
    def get[T](name: String): Box[T] =
      ScreenVars.is.get(name).map(_._2.asInstanceOf[T])


    def set[T](name: String, from: ScreenVar[_], value: T): Unit =
      ScreenVars.set(ScreenVars.is + (name -> (from, value)))

    def clear(name: String): Unit =
      ScreenVars.set(ScreenVars.is - name)
  }

  protected def localSetup() {

  }

  def toForm = {
    Referer.is // touch to capture the referer

    if (FirstTime) {
      FirstTime.set(false)
      localSetup()
      val localSnapshot = createSnapshot
      S.redirectTo(S.uri, () => localSnapshot.restore)
    }

    val finishId = Helpers.nextFuncName
    val cancelId = Helpers.nextFuncName

    val theScreen = this

    val finishButton = theScreen.finishButton % ("onclick" -> ("document.getElementById(" + finishId.encJs + ").submit()"))

    val cancelButton: Elem = theScreen.cancelButton % ("onclick" -> ("document.getElementById(" + cancelId.encJs + ").submit()"))

    val url = S.uri

    renderAll(theScreen.screenTop,
      theScreen.screenFields.map(f => ScreenFieldInfo(f, f.displayHtml, f.helpAsHtml, f.toForm)),
      Full(cancelButton),
      Full(finishButton), theScreen.screenBottom, finishId, cancelId, theScreen)
  }

  protected case class ScreenFieldInfo(field: FieldIdentifier, text: NodeSeq, help: Box[NodeSeq], input: Box[NodeSeq])

  protected def renderAll(screenTop: Box[Elem],
                          fields: List[ScreenFieldInfo],
                          cancel: Box[Elem],
                          finish: Box[Elem],
                          screenBottom: Box[Elem],
                          finishId: String, cancelId: String, theScreen: AbstractScreen): NodeSeq = {

    val notices: List[(NoticeType.Value, NodeSeq, Box[String])] = S.getAllNotices


    def bindFieldLine(xhtml: NodeSeq): NodeSeq = {
      fields.flatMap {
        f =>
            val myNotices = notices.filter(fi => fi._3.isDefined && fi._3 == f.field.uniqueFieldId)
            bind("wizard", xhtml, "label" -> f.text, "form" -> f.input,
              "help" -> NodeSeq.Empty,
              FuncBindParam("field_errors", xml => {
                myNotices match {
                  case Nil => NodeSeq.Empty
                  case xs => bind("wizard", xml, "error" ->
                      (innerXml => xs.flatMap {case (_, msg, _) => bind("wizard", innerXml, "bind" -> msg)}))
                }
              }))
      }
    }

    def url = S.uri

    val snapshot = createSnapshot




    def bindFields(xhtml: NodeSeq): NodeSeq =
      (<form id={finishId} action={url} method="post">{S.formGroup(-1)(SHtml.hidden(() =>
          snapshot.restore()))}{bind("wizard", xhtml, "line" -> bindFieldLine _)}{S.formGroup(4)(SHtml.hidden(() =>
          {doFinish(); val localSnapshot = createSnapshot; S.redirectTo(url, () => localSnapshot.restore)}))}</form> %
          theScreen.additionalAttributes) ++
          <form id={cancelId} action={url} method="post">{SHtml.hidden(() => {
            snapshot.restore();
            S.redirectTo(Referer.is)
          })}</form>

    Helpers.bind("wizard", allTemplate,
      "screen_number" -> Text("1"),
      "total_screens" -> Text("1"),
      FuncBindParam("wizard_top", xml => NodeSeq.Empty),
      FuncBindParam("screen_top", xml => (screenTop.map(top => bind("wizard", xml, "bind" -%> top)) openOr NodeSeq.Empty)),
      FuncBindParam("wizard_bottom", xml => NodeSeq.Empty),
      FuncBindParam("screen_bottom", xml => (screenBottom.map(bottom => bind("wizard", xml, "bind" -%> bottom)) openOr NodeSeq.Empty)),
      "prev" -> (Unparsed("&nbsp;") : NodeSeq),
      "next" -> ((finish) openOr Unparsed("&nbsp;")),
      "cancel" -> (cancel openOr Unparsed("&nbsp;")),
      "errors" -> NodeSeq.Empty, // FIXME deal with errors
      FuncBindParam("fields", bindFields _))

  }


  protected def allTemplatePath: List[String] = LiftScreenRules.allTemplatePath.vend

  protected def allTemplateNodeSeq: NodeSeq =
    <div>
    <wizard:wizard_top> <div> <wizard:bind/> </div> </wizard:wizard_top>
    <wizard:screen_top> <div> <wizard:bind/> </div> </wizard:screen_top>
    <wizard:errors> <div> <ul> <wizard:item> <li> <wizard:bind/> </li> </wizard:item> </ul> </div> </wizard:errors>
    <div> <wizard:fields>
    <table>
    <wizard:line>
    <tr>
    <td>
    <wizard:label error_style="error"/> <wizard:help/> <wizard:field_errors> <ul> <wizard:error> <li> <wizard:bind/> </li> </wizard:error> </ul> </wizard:field_errors>
    </td>
    <td> <wizard:form/> </td>
    </tr>
    </wizard:line>
    </table>
    </wizard:fields> </div>
    <div> <table> <tr> <td> <wizard:prev/> </td> <td> <wizard:cancel/> </td> <td> <wizard:next/> </td> </tr> </table> </div>
    <wizard:screen_bottom> <div> <wizard:bind/> </div> </wizard:screen_bottom>
    <wizard:wizard_bottom> <div> <wizard:bind/> </div> </wizard:wizard_bottom>
    </div>

  protected def allTemplate: NodeSeq = TemplateFinder.findAnyTemplate(allTemplatePath) openOr allTemplateNodeSeq

  /**
   * What additional attributes should be put on the
   */
  protected def formAttrs: MetaData = scala.xml.Null


  protected def finish(): Unit

  protected def doFinish() {
    validate match {
      case Nil =>
        val snapshot = createSnapshot
        PrevSnapshot.set(Full(snapshot))
        finish()
        redirectBack()

      case xs => S.error(xs)
    }
  }

  protected def redirectBack() {
    S.redirectTo(Referer.is)
  }
}


trait IntField extends FieldIdentifier {
  self: AbstractScreen#Field =>
  type ValueType = Int

  def default = 0

  lazy val manifest = buildIt[Int]

  def minVal(len: Int, msg: => String): Int => List[FieldError] = s =>
      if (s < len) List(FieldError(this, Text(msg))) else Nil

  def maxVal(len: Int, msg: => String): Int => List[FieldError] = s =>
      if (s > len) List(FieldError(this, Text(msg))) else Nil
}

trait BooleanField extends FieldIdentifier {
  self: AbstractScreen#Field =>
  type ValueType = Boolean

  def default = false

  lazy val manifest = buildIt[Boolean]
}

trait StringField extends FieldIdentifier {
  self: AbstractScreen#Field =>
  type ValueType = String

  def default = ""

  lazy val manifest = buildIt[String]

  def minLen(len: Int, msg: => String): String => List[FieldError] = s =>
      if (s.length < len) List(FieldError(this, Text(msg))) else Nil

  def maxLen(len: Int, msg: => String): String => List[FieldError] = s =>
      if (s.length > len) List(FieldError(this, Text(msg))) else Nil
}

object LiftScreenRules extends Factory with FormVendor {
  private def m[T](implicit man: Manifest[T]): Manifest[T] = man

  val allTemplatePath: FactoryMaker[List[String]] = new FactoryMaker[List[String]](() => List("templates-hidden", "wizard-all")) {}
}

}
}
