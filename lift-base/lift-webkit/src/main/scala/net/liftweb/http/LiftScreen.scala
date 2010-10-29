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

  protected type Errors = List[FieldError]

  @volatile private[this] var _fieldList: List[() => FieldContainer] = Nil

  /**
   * any additional parameters that need to be put in the on the form (e.g., mime type)
   */
  def additionalAttributes: MetaData =
  if (hasUploadField) new UnprefixedAttribute("enctype", Text("multipart/form-data"), Null) else Null


  /**
   * Add a FieldContainer to the Screen.  A FieldContainer
   * can contain either a single field (a BaseField) or
   * a collection of BaseFields.  The key take-away is that
   * if the LiftScreen or Wizard is a singleton, you can still
   * display variable number of fields by returning a variable
   * number of BaseField instances from the FieldContainer.
   */
  protected def addFields(fields: () => FieldContainer) {
    _fieldList = _fieldList ::: List(fields)
  }

  /**
   * Use addFields
   * 
   * @deprecated
   */
  protected def _register(field: () => FieldContainer) = addFields(field)

  protected def hasUploadField: Boolean = screenFields.foldLeft(false)(_ | _.uploadField_?)

  /**
   *  A list of fields in this screen
   */
  def screenFields: List[BaseField] = _fieldList.flatMap(_.apply().allFields)


  def screenTop: Box[Elem] = Empty

  def screenBottom: Box[Elem] = Empty

  // an implicit coversion so we don't have to put Full around every Elem
  protected implicit def elemInABox(in: Elem): Box[Elem] = Box !! in

  /**
   * The name of the screen.  Override this to change the screen name
   */
  def screenName: String = "Screen"

  def screenNameAsHtml: NodeSeq = Text(screenName)

  def screenTitle: NodeSeq = screenNameAsHtml

  def cancelButton: Elem = <button>{S.??("Cancel")}</button>

  def finishButton: Elem = <button>{S.??("Finish")}</button>


  implicit def boxOfScreen[T <: AbstractScreen](in: T): Box[T] = Box !! in


  def validate: List[FieldError] = screenFields.flatMap(_.validate) ++ screenValidate

  def validations: List[() => List[FieldError]] = Nil

  def screenValidate: List[FieldError] = validations.flatMap(_())

  protected def vendForm[T](implicit man: Manifest[T]): Box[(T, T => Unit) => NodeSeq] = Empty

  protected def vendAVar[T](dflt: => T): NonCleanAnyVar[T]

  object Field {
    implicit def fToType[T](field: Field{type ValueType=T}): T = field.is
  }

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

    override def helpAsHtml: Box[NodeSeq] = Empty

    /**
     *  Is the field editable
     */
    def editable_? = true

    def toForm: Box[NodeSeq] = {
      val func: Box[(ValueType, ValueType => Unit) => NodeSeq] =
      AbstractScreen.this.vendForm(manifest) or otherFuncVendors(manifest) or
      LiftRules.vendForm(manifest)

      func.map(f => f(is, set _)).filter(x => editable_?)
    }

    protected def otherFuncVendors(what: Manifest[ValueType]): Box[(ValueType, ValueType => Unit) => NodeSeq] = Empty


    def validate: List[FieldError] = currentField.doWith(this) {
      validations.flatMap(_.apply(is))
    }

    def validations: List[ValueType => List[FieldError]] = Nil

    def setFilter: List[ValueType => ValueType] = Nil

    override lazy val uniqueFieldId: Box[String] = Full("I"+Helpers.hash(this.getClass.getName))

    override def toString = is.toString
  }

  protected object currentField extends ThreadGlobal[FieldIdentifier]

  protected class FieldBuilder[T](name: => String,
                                  default: => T,
                                  manifest: Manifest[T],
                                  help: Box[NodeSeq],
                                  validations: List[T => List[FieldError]],
                                  filters: List[T => T])
  {
    /**
     * Set the Help HTML
     */
    def help(h: NodeSeq): FieldBuilder[T] = new FieldBuilder[T](name, default,
                                                                manifest, Full(h), validations, filters)

    /**
     * Add a filter field (the wacky symbols are supposed to look like a filter symbol)
     */
    def <*>(f: T => T): FieldBuilder[T] =
    new FieldBuilder[T](name, default,
                        manifest, help, validations, filters ::: List(f))

    /**
     * Add a validtion (the wacky symbols are supposed to look like a check mark)
     */
    def ^/(f: T => List[FieldError]): FieldBuilder[T] =
    new FieldBuilder[T](name, default,
                        manifest, help, validations ::: List(f), filters)

    /**
     * Convert the field builder into a field
     */
    def make: Field{type ValueType = T} = new Field {
      type ValueType = T
      override def name: String = FieldBuilder.this.name
      override def default = FieldBuilder.this.default
      override implicit def manifest: Manifest[ValueType] = FieldBuilder.this.manifest
      override def helpAsHtml = help
      override def validations = FieldBuilder.this.validations
      override def setFilter = FieldBuilder.this.filters
      override lazy val uniqueFieldId: Box[String] = Full("I"+randomString(15))
    }
  }

  implicit def strToListFieldError(msg: String): List[FieldError] =
  List(FieldError(currentField.box openOr new FieldIdentifier{}, Text(msg)))

  implicit def xmlToListFieldError(msg: NodeSeq): List[FieldError] =
  List(FieldError(currentField.box openOr new FieldIdentifier{}, msg))

  implicit def boxStrToListFieldError(msg: Box[String]): List[FieldError] =
  msg.toList.map(msg =>
    FieldError(currentField.box openOr new FieldIdentifier{}, Text(msg)))

  implicit def boxXmlToListFieldError(msg: Box[NodeSeq]): List[FieldError] =
  msg.toList.map(msg => FieldError(currentField.box openOr new FieldIdentifier{}, msg))
  
  /**
   * Create a FieldBuilder so you can add help screens, validations and filters.  Remember to invoke "make" on
   * the returned FieldBuilder to convert it into a field
   *
   * @param name - the name of the field.  This is a call-by-name parameter, so you can dynamically calculate
   * the name of the fiels (e.g., localize its name)
   * @param default - the default value of the field
   * @param validate - any validation functions
   */
  protected def builder[T](name: => String, default: => T, stuff: FilterOrValidate[T]*)(implicit man: Manifest[T]): FieldBuilder[T] =
  new FieldBuilder[T](name, default, man, Empty, stuff.toList.flatMap{
    case AVal(v) => List(v) case _ => Nil}, stuff.toList.flatMap{
    case AFilter(v) => List(v) case _ => Nil})

  protected object FilterOrValidate {
    implicit def promoteFilter[T](f: T => T): FilterOrValidate[T] = AFilter(f)  
  implicit def promoteValidate[T](v: T => List[FieldError]): FilterOrValidate[T] = AVal(v)  
  }
  sealed protected trait FilterOrValidate[T]

  protected final case class AFilter[T](val f: T => T) extends FilterOrValidate[T]
  protected final case class AVal[T](val v: T => List[FieldError]) extends FilterOrValidate[T]

  /**
   * Create a field with a name, default value, and
   *
   * @param name - the name of the field.  This is a call-by-name parameter, so you can dynamically calculate
   * the name of the fiels (e.g., localize its name)
   * @param default - the default value of the field
   * @param validate - any validation functions
   */
  protected def field[T](name: => String, default: => T, stuff: FilterOrValidate[T]*)(implicit man: Manifest[T]): Field{type ValueType = T} =
  new FieldBuilder[T](name, default, man, Empty, stuff.toList.flatMap{
    case AVal(v) => List(v) case _ => Nil}, stuff.toList.flatMap{
    case AFilter(v) => List(v) case _ => Nil}).make

  protected def removeRegExChars(regEx: String): String => String =
  s => s match {
    case null => null
    case s => s.replaceAll(regEx, "")
  }

  protected def toLower: String => String =
  s => s match {
    case null => null
    case s => s.toLowerCase
  }

  protected def toUpper: String => String =
  s => s match {
    case null => null
    case s => s.toUpperCase
  }

  protected def trim: String => String =
  s => s match {
    case null => null
    case s => s.trim
  }

  protected def notNull: String => String =
  s => s match {
    case null => ""
    case x => x
  }

  /**
   * A validation helper.  Make sure the string is at least a particular
   * length and generate a validation issue if not
   */
  protected def valMinLen(len: => Int, msg: => String): String => List[FieldError] =
  s => s match {
    case str if (null ne str) && str.length >= len => Nil
    case _ => List(FieldError(currentField.box openOr new FieldIdentifier{}, Text(msg)))
  }


  /**
   * A validation helper.  Make sure the string is no more than a particular
   * length and generate a validation issue if not
   */
  protected def valMaxLen(len: => Int, msg: => String): String => List[FieldError] =
  s => s match {
    case str if (null ne str) && str.length <= len => Nil
    case _ => List(FieldError(currentField.box openOr new FieldIdentifier{}, Text(msg)))
  }

  /**
   * Make sure the field matches a regular expression
   */
  protected def valRegex(pat: => java.util.regex.Pattern, msg: => String): String => List[FieldError] =
  s => s match {
    case str if (null ne str) && pat.matcher(str).matches => Nil
    case _ => List(FieldError(currentField.box openOr new FieldIdentifier{}, Text(msg)))
  }

  protected def minVal[T](len: => T, msg: => String)(implicit f: T => Number): T => List[FieldError] =
  s => if (f(s).doubleValue < f(len).doubleValue) msg else Nil

  protected def maxVal[T](len: => T, msg: => String)(implicit f: T => Number): T => List[FieldError] =
  s => if (f(s).doubleValue > f(len).doubleValue) msg else Nil

  def noticeTypeToAttr(screen: AbstractScreen): Box[NoticeType.Value => MetaData] =
  inject[NoticeType.Value => MetaData] or LiftScreenRules.inject[NoticeType.Value => MetaData]
}



trait ScreenWizardRendered {
  protected def renderAll(currentScreenNumber: Box[NodeSeq],
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
                          nextId: (String, () => Unit),
                          prevId: Box[(String, () => Unit)],
                          cancelId: (String, () => Unit),
                          theScreen: AbstractScreen): NodeSeq = {

    val notices: List[(NoticeType.Value, NodeSeq, Box[String])] = S.getAllNotices


    def bindFieldLine(xhtml: NodeSeq): NodeSeq = {
      fields.flatMap {
        f =>
        val curId = f.field.uniqueFieldId openOr randomString(20)
        val myNotices = notices.filter(fi => fi._3.isDefined && fi._3 == f.field.uniqueFieldId)
        def doLabel(in: NodeSeq): NodeSeq =
        myNotices match {
          case Nil => bind("wizard", in, AttrBindParam("for", curId, "for"), "bind" -%> f.text)
          case _ =>
            val maxN = myNotices.map(_._1).sort{_.id > _.id}.head // get the maximum type of notice (Error > Warning > Notice)
            val metaData: MetaData = noticeTypeToAttr(theScreen).map(_(maxN)) openOr Null
            bind("wizard", in, AttrBindParam("for", curId, "for"), "bind" -%> f.text).map {
              case e: Elem => e % metaData
              case x => x
            }
        }
        bind("wizard", xhtml,
             "label" -%> doLabel _, 
             "form" -%> f.input.map(f => f.map{
            case e: Elem => e % ("id" -> curId)
            case x => x}: NodeSeq),
             FuncBindParam("help", xml => {
              f.help match {
                case Full(hlp) => bind("wizard", xml, "bind" -%> hlp)
                case _ => NodeSeq.Empty
              }
            }),
             FuncBindParam("field_errors", xml => {
              myNotices match {
                case Nil => NodeSeq.Empty
                case xs => bind("wizard", xml, "error" -%>
                                (innerXml => xs.flatMap {case (noticeType, msg, _) =>
                          val metaData: MetaData = noticeTypeToAttr(theScreen).map(_(noticeType)) openOr Null
                          bind("wizard", innerXml, "bind" -%> msg).map {
                            case e: Elem => e % metaData
                            case x => x
                          }}))
              }
            }))
      }
    }

    def url = S.uri

    val snapshot = createSnapshot

    def bindErrors(xhtml: NodeSeq): NodeSeq = notices.filter(_._3.isEmpty) match {
      case Nil => NodeSeq.Empty
      case xs =>
        def doErrors(in: NodeSeq): NodeSeq = xs.flatMap{case (noticeType, msg, _) =>
            val metaData: MetaData = noticeTypeToAttr(theScreen).map(_(noticeType)) openOr Null
            bind("wizard", in, "bind" -%>
                 (msg)).map {
              case e: Elem => e % metaData
              case x => x
            }}

        bind("wizard", xhtml,
             "item" -%> doErrors _)
    }



    def bindFields(xhtml: NodeSeq): NodeSeq =
    (<form id={nextId._1} action={url} method="post">{S.formGroup(-1)(SHtml.hidden(() =>
            snapshot.restore()))}{bind("wizard", xhtml, "line" -%> bindFieldLine _)}{S.formGroup(4)(SHtml.hidden(() =>
            {nextId._2(); val localSnapshot = createSnapshot; S.seeOther(url, () => localSnapshot.restore)}))}</form> %
      theScreen.additionalAttributes) ++
    prevId.toList.map{case (id, func) =>
        <form id={id} action={url} method="post">{SHtml.hidden(() => {snapshot.restore(); func();
                                                                      val localSnapshot = createSnapshot;
                                                                      S.seeOther(url, () => localSnapshot.restore)})}</form>
    } ++
    <form id={cancelId._1} action={url} method="post">{SHtml.hidden(() => {
            snapshot.restore();
            cancelId._2() // WizardRules.deregisterWizardSession(CurrentSession.is)
            S.seeOther(Referer.is)
          })}</form>

    def bindScreenInfo(xhtml: NodeSeq): NodeSeq = (currentScreenNumber, screenCount) match {
      case (Full(num), Full(cnt)) =>
        bind("wizard", xhtml, "screen_number" -%> num/*Text(CurrentScreen.is.map(s => (s.myScreenNum + 1).toString) openOr "")*/,
             "total_screens" -%> cnt /*Text(screenCount.toString)*/)
      case _ => NodeSeq.Empty
    }

    Helpers.bind("wizard", allTemplate,
                 "screen_info" -%> bindScreenInfo _,
                 FuncBindParam("wizard_top", xml => (wizardTop.map(top => bind("wizard", xml, "bind" -%> top)) openOr NodeSeq.Empty)),
                 FuncBindParam("screen_top", xml => (screenTop.map(top => bind("wizard", xml, "bind" -%> top)) openOr NodeSeq.Empty)),
                 FuncBindParam("wizard_bottom", xml => (wizardBottom.map(bottom => bind("wizard", xml, "bind" -%> bottom)) openOr NodeSeq.Empty)),
                 FuncBindParam("screen_bottom", xml => (screenBottom.map(bottom => bind("wizard", xml, "bind" -%> bottom)) openOr NodeSeq.Empty)),
                 "prev" -%> (prev openOr EntityRef("nbsp")),
                 "next" -%> ((next or finish) openOr EntityRef("nbsp")),
                 "cancel" -%> (cancel openOr EntityRef("nbsp")),
                 "errors" -%> bindErrors _,
                 FuncBindParam("fields", bindFields _))

  }

  protected def allTemplate: NodeSeq

  protected def allTemplateNodeSeq: NodeSeq = {
    <div>
      <wizard:screen_info><div>Page <wizard:screen_number/> of <wizard:total_screens/></div></wizard:screen_info>
      <wizard:wizard_top> <div> <wizard:bind/> </div> </wizard:wizard_top>
      <wizard:screen_top> <div> <wizard:bind/> </div> </wizard:screen_top>
      <wizard:errors> <div> <ul> <wizard:item> <li> <wizard:bind/> </li> </wizard:item> </ul> </div> </wizard:errors>
      <div> <wizard:fields>
          <table>
            <wizard:line>
              <tr>
                <td>
                  <wizard:label><label wizard:for=""><wizard:bind/></label></wizard:label>
                  <wizard:help><span><wizard:bind/></span></wizard:help> <wizard:field_errors> <ul> <wizard:error> <li> <wizard:bind/> </li> </wizard:error> </ul> </wizard:field_errors>
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
  }

  protected trait Snapshot {
    def restore(): Unit
  }

  protected def createSnapshot: Snapshot

  def noticeTypeToAttr(screen: AbstractScreen): Box[NoticeType.Value => MetaData]

  protected def Referer: AnyVar[String, _]
}


case class ScreenFieldInfo(field: FieldIdentifier, text: NodeSeq, help: Box[NodeSeq], input: Box[NodeSeq])

trait LiftScreen extends AbstractScreen with StatefulSnippet with ScreenWizardRendered {
  def dispatch = {
    case _ => ignore => this.toForm
  }

  private object ScreenVars extends RequestVar[Map[String, (NonCleanAnyVar[_], Any)]](Map())
  private object PrevSnapshot extends RequestVar[Box[ScreenSnapshot]](Empty)
  protected object Referer extends ScreenVar[String](S.referer openOr "/")
  private object FirstTime extends ScreenVar[Boolean](true)

  protected class ScreenSnapshot(private[http] val screenVars: Map[String, (NonCleanAnyVar[_], Any)],
                                 private[http] val snapshot: Box[ScreenSnapshot]) extends Snapshot {
    def restore() {
      registerThisSnippet();
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
      S.seeOther(S.uri, () => localSnapshot.restore)
    }

    val finishId = Helpers.nextFuncName
    val cancelId = Helpers.nextFuncName

    val theScreen = this

    val finishButton = theScreen.finishButton % ("onclick" -> ("document.getElementById(" + finishId.encJs + ").submit()"))

    val cancelButton: Elem = theScreen.cancelButton % ("onclick" -> ("document.getElementById(" + cancelId.encJs + ").submit()"))

    val url = S.uri


    renderAll(
      Empty, //currentScreenNumber: Box[NodeSeq],
          Empty, //screenCount: Box[NodeSeq],
          Empty, // wizardTop: Box[Elem],
          theScreen.screenTop, //screenTop: Box[Elem],
          theScreen.screenFields.flatMap(f =>
        if (f.show_?) List(ScreenFieldInfo(f, f.displayHtml, f.helpAsHtml, f.toForm)) else Nil), //fields: List[ScreenFieldInfo],
            Empty, // prev: Box[Elem],
            Full(cancelButton), // cancel: Box[Elem],
            Empty, // next: Box[Elem],
            Full(finishButton), //finish: Box[Elem],
            theScreen.screenBottom, // screenBottom: Box[Elem],
            Empty, //wizardBottom: Box[Elem],
            finishId -> doFinish _, // nextId: (String, () => Unit),
            Empty, // prevId: Box[(String, () => Unit)],
            cancelId -> (() => {}), //cancelId: (String, () => Unit),
            theScreen)
  }

  protected def allTemplatePath: List[String] = LiftScreenRules.allTemplatePath.vend

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
    S.seeOther(Referer.is)
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

trait StringField extends FieldIdentifier with StringValidators {
  self: AbstractScreen#Field =>
  type ValueType = String

  def default = ""

  lazy val manifest = buildIt[String]

  def maxLen = Integer.MAX_VALUE

  protected def valueTypeToBoxString(in: ValueType): Box[String] = Full(in)
  protected def boxStrToValType(in: Box[String]): ValueType = in openOr ""
}

object LiftScreenRules extends Factory with FormVendor {
  private def m[T](implicit man: Manifest[T]): Manifest[T] = man

  val allTemplatePath: FactoryMaker[List[String]] = new FactoryMaker[List[String]](() => List("templates-hidden", "wizard-all")) {}
  val messageStyles: FactoryMaker[NoticeType.Value => MetaData] =
  new FactoryMaker[NoticeType.Value => MetaData](() => {
      case NoticeType.Notice => new UnprefixedAttribute("class", "lift_notice", Null)
      case NoticeType.Warning =>  new UnprefixedAttribute("class", "lift_warning", Null)
      case NoticeType.Error =>  new UnprefixedAttribute("class", "lift_error", Null)
    }: PartialFunction[NoticeType.Value, MetaData]) {}


}

}
}
