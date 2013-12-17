/*
 * Copyright 2010-2011 WorldWide Conferencing, LLC
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

import xml._
import reflect.Manifest

import common._
import util._
import Helpers._
import js._
import JsCmds._

import java.util.concurrent.atomic.AtomicReference


/**
 * The trait that forms the basis for LiftScreen and the
 * Screen instances in Wizard
 */
trait AbstractScreen extends Factory with Loggable {
  override def toString = screenName

  protected type Errors = List[FieldError]

  @volatile private[this] var _fieldList: List[() => FieldContainer] = Nil

  /**
   * Any additional parameters that need to be put on the form (e.g., mime type)
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
   * <br/>
   * WARNING -- this method is public so it can be called
   * from a Wizard.  This method should only be called from within
   * the Screen or Wizard that owns the Screen and not
   * from external code.
   */
  def addFields(fields: () => FieldContainer) {
    _fieldList = _fieldList ::: List(fields)
  }

  protected def hasUploadField: Boolean = screenFields.foldLeft(false)(_ | _.uploadField_?)

  /**
   *  A list of fields in this screen
   */
  def screenFields: List[BaseField] = _fieldList.flatMap(_.apply().allFields)

  /**
   * Override this method to do any setup of this screen
   */
  protected def localSetup() {

  }


  def screenTop: Box[Elem] = Empty

  def screenBottom: Box[Elem] = Empty

  // An implicit conversion so we don't have to put Full around every Elem
  protected implicit def elemInABox(in: Elem): Box[Elem] = Box !! in

  /**
   * The name of the screen.  Override this to change the screen name.
   */
  def screenName: String = "Screen"

  def screenNameAsHtml: NodeSeq = Text(screenName)

  def screenTitle: NodeSeq = screenNameAsHtml

  def cancelButton: Elem = <button>
    {S.?("Cancel")}
  </button>

  def finishButton: Elem = <button>
    {S.?("Finish")}
  </button>


  implicit def boxOfScreen[T <: AbstractScreen](in: T): Box[T] = Box !! in


  def validate: List[FieldError] = screenFields.filter(_.shouldDisplay_?).
    filter(_.show_?).flatMap(_.validate) ++ screenValidate

  def validations: List[() => List[FieldError]] = Nil

  def screenValidate: List[FieldError] = validations.flatMap(_())

  protected def vendForm[T](implicit man: Manifest[T]): Box[(T, T => Any) => NodeSeq] = Empty

  protected def vendAVar[T](dflt: => T): NonCleanAnyVar[T]

  object Field {
    implicit def fToType[T](field: Field {type ValueType = T}): T = field.is
  }

  protected sealed trait OtherValueInitializer[T]

  protected case object NothingOtherValueInitializer extends OtherValueInitializer[Nothing]

  protected final case class OtherValueInitializerImpl[T](f: () => T) extends OtherValueInitializer[T]

  /**
   * By default, are all the fields on this screen on the confirm screen?
   */
  def onConfirm_? : Boolean

  /**
   * A field that's part of a Screen
   */
  trait Field extends ConfirmField {
    type OtherValueType
    // >: Nothing

    AbstractScreen.this.addFields(() => this)

    private val _currentValue: NonCleanAnyVar[ValueType] =
      vendAVar[ValueType](setFilter.foldLeft(default)((nv, f) => f(nv)))

    private val _otherValue: NonCleanAnyVar[OtherValueType] =
      vendAVar[OtherValueType](otherValueDefault)

    /**
     * Is this field on the confirm screen
     */
    def onConfirm_? : Boolean = AbstractScreen.this.onConfirm_?

    protected def otherValueDefault: OtherValueType = null.asInstanceOf[OtherValueType]

    /**
     * A field my have an "otherValue" which can be used
     * to store a list of options or other information that
     * the fields needs on a Screen/Wizard by Screen/Wizard
     * basis
     */
    def otherValue: OtherValueType = _otherValue.get

    def setOtherValue(v: OtherValueType) = _otherValue.set(v)

    def default: ValueType

    def is = _currentValue.is

    /**
     * Set to true if this field is part of a multi-part mime upload
     */
    override def uploadField_? = false

    def get = is

    def set(v: ValueType) = _currentValue.set(setFilter.foldLeft(v)((nv, f) => f(nv)))

    def set_? : Boolean = _currentValue.set_?

    def otherValueSet_? : Boolean = _otherValue.set_?

    implicit def manifest: Manifest[ValueType]

    protected def buildIt[T](implicit man: Manifest[T]): Manifest[T] = man

    override def helpAsHtml: Box[NodeSeq] = Empty

    /**
     * What form elements are we going to add to this field?
     */
    def formElemAttrs: Seq[SHtml.ElemAttr] = Nil

    /**
     *  Is the field editable
     */
    def editable_? = true

    def toForm: Box[NodeSeq] = {
      val func: Box[(ValueType, ValueType => Any) => NodeSeq] =
        AbstractScreen.this.vendForm(manifest) or otherFuncVendors(manifest) or
          LiftRules.vendForm(manifest)

      func.map(f => f(is, set _)).filter(x => editable_?).
        map(ns => SHtml.ElemAttr.applyToAllElems(ns, formElemAttrs))
    }

    protected def otherFuncVendors(what: Manifest[ValueType]): Box[(ValueType, ValueType => Any) => NodeSeq] = Empty


    def validate: List[FieldError] = currentField.doWith(this) {
      validations.flatMap(_.apply(is))
    }

    def validations: List[ValueType => List[FieldError]] = Nil

    def setFilter: List[ValueType => ValueType] = Nil

    override def uniqueFieldId: Box[String] = Full(_theFieldId.get)

    private lazy val _theFieldId: NonCleanAnyVar[String] =
      vendAVar(Helpers.nextFuncName)

    override def toString = if (is != null) is.toString else ""

    def binding: Box[FieldBinding] = Empty

    def transforms: List[BaseField => NodeSeq => NodeSeq] = Nil
  }

  protected object currentField extends ThreadGlobal[FieldIdentifier]

  protected class FieldBuilder[T](name: => String,
                                  default: => T,
                                  manifest: Manifest[T],
                                  help: Box[NodeSeq],
                                  validations: List[T => List[FieldError]],
                                  filters: List[T => T],
                                  stuff: Seq[FilterOrValidate[T]]) {
    /**
     * Set the Help HTML
     */
    def help(h: NodeSeq): FieldBuilder[T] = new FieldBuilder[T](name, default,
      manifest, Full(h), validations, filters, stuff)

    /**
     * Add a filter field (the wacky symbols are supposed to look like a filter symbol)
     */
    def <*>(f: T => T): FieldBuilder[T] =
      new FieldBuilder[T](name, default,
        manifest, help, validations, filters ::: List(f),
        stuff)

    /**
     * Add a validation (the wacky symbols are supposed to look like a check mark)
     */
    def ^/(f: T => List[FieldError]): FieldBuilder[T] =
      new FieldBuilder[T](name, default,
        manifest, help, validations ::: List(f), filters, stuff)

    /**
     * Convert the field builder into a field
     */
    def make: Field {type ValueType = T} = {
      val paramFieldId: Box[String] = (stuff.collect {
        case FormFieldId(id) => id }).headOption

      val confirmInfo = stuff.collect {
        case NotOnConfirmScreen => false
      }.headOption orElse
        stuff.collect {
          case OnConfirmScreen => true
        }.headOption

      val newBinding: Box[FieldBinding] = (stuff.collect {
        case AFieldBinding(i) => i
      }).headOption

      val newHelp: Box[NodeSeq] = help or (stuff.collect {
        case Help(ns) => ns
      }).headOption

      val newTransforms: List[BaseField => NodeSeq => NodeSeq] = stuff.collect({
        case FieldTransform(func) => func
      }).toList

      val newShow: Box[BaseField => Boolean] = (stuff.collect {
        case DisplayIf(func) => func
      }).headOption

      new Field {
        type ValueType = T

        /**
         * Is this field on the confirm screen
         */
        override def onConfirm_? : Boolean = confirmInfo getOrElse super.onConfirm_?

        override def name: String = FieldBuilder.this.name

        override def default = FieldBuilder.this.default

        /**
         * What form elements are we going to add to this field?
         */
        override lazy val formElemAttrs: Seq[SHtml.ElemAttr] = grabParams(stuff)

        override implicit def manifest: Manifest[ValueType] = FieldBuilder.this.manifest

        override def helpAsHtml = newHelp

        override def validations = FieldBuilder.this.validations

        override def setFilter = FieldBuilder.this.filters

        override def uniqueFieldId: Box[String] =
          paramFieldId or Full(_theFieldId.get)

        override def binding = newBinding

        private lazy val _theFieldId: NonCleanAnyVar[String] =
          vendAVar(Helpers.nextFuncName)

        override def transforms = newTransforms

        override def show_? = newShow map (_(this)) openOr (super.show_?)
      }
    }
  }

  implicit def strToListFieldError(msg: String): List[FieldError] =
    List(FieldError(currentField.box openOr new FieldIdentifier {}, Text(msg)))

  implicit def xmlToListFieldError(msg: NodeSeq): List[FieldError] =
    List(FieldError(currentField.box openOr new FieldIdentifier {}, msg))

  implicit def boxStrToListFieldError(msg: Box[String]): List[FieldError] =
    msg.toList.map(msg =>
      FieldError(currentField.box openOr new FieldIdentifier {}, Text(msg)))

  implicit def boxXmlToListFieldError(msg: Box[NodeSeq]): List[FieldError] =
    msg.toList.map(msg => FieldError(currentField.box openOr new FieldIdentifier {}, msg))

  /**
   * Create a FieldBuilder so you can add help screens, validations and filters.  Remember to invoke "make" on
   * the returned FieldBuilder to convert it into a field
   *
   * @param name - the name of the field.  This is a call-by-name parameter, so you can dynamically calculate
   * the name of the field (e.g., localize its name)
   * @param default - the default value of the field
   * @param stuff - any filter or validation functions
   */
  protected def builder[T](name: => String, default: => T, stuff: FilterOrValidate[T]*)(implicit man: Manifest[T]): FieldBuilder[T] = {
    new FieldBuilder[T](name, default, man, Empty,
      stuff.toList.collect {
        case AVal(v: Function1[_, _]) => v.asInstanceOf[T => List[FieldError]]
      },
      stuff.toList.collect {
        case AFilter(v) => v.asInstanceOf[T => T]
      },
      stuff)
  }

  protected object FilterOrValidate {
    implicit def promoteFilter[T](f: T => T): FilterOrValidate[T] = AFilter(f)

    implicit def promoteValidate[T](v: T => List[FieldError]): FilterOrValidate[T] = AVal(v)

    implicit def promoteToFormParam(a: SHtml.ElemAttr): FilterOrValidate[Nothing] = FormParam(a)

    implicit def promoteToFormParam(a: (String, String)): FilterOrValidate[Nothing] = FormParam(a)

    implicit def promoteFieldBinding(binding: FieldBinding): FilterOrValidate[Nothing] = AFieldBinding(binding)
  }

  sealed protected trait FilterOrValidate[+T]

  /**
   * Override the screen default for fields appearing on the confirm
   * screen and force this field to appear on the confirm screen
   */
  protected case object OnConfirmScreen extends FilterOrValidate[Nothing]


  /**
   * Override the screen default for fields appearing on the confirm
   * screen and force this field not to appear on the confirm screen
   */
  protected case object NotOnConfirmScreen extends FilterOrValidate[Nothing]

  protected final case class FormParam(fp: SHtml.ElemAttr) extends FilterOrValidate[Nothing]

  protected final case class FormFieldId(id: String) extends FilterOrValidate[Nothing]

  protected final case class AFilter[T](f: T => T) extends FilterOrValidate[T]

  protected final case class AVal[T](v: T => List[FieldError]) extends FilterOrValidate[T]

  protected final case class AFieldBinding(binding: FieldBinding) extends FilterOrValidate[Nothing]

  protected final case class Help(ns: NodeSeq) extends FilterOrValidate[Nothing]

  protected final case class FieldTransform(func: BaseField => NodeSeq => NodeSeq) extends FilterOrValidate[Nothing]

  protected final case class DisplayIf(func: BaseField => Boolean) extends FilterOrValidate[Nothing]

  protected def field[T](underlying: => BaseField {type ValueType = T},
                         stuff: FilterOrValidate[T]*)(implicit man: Manifest[T]): Field {type ValueType = T} = {
    val paramFieldId: Box[String] = (stuff.collect {
      case FormFieldId(id) => id
    }).headOption

    val confirmInfo = stuff.collect {
      case NotOnConfirmScreen => false
    }.headOption orElse
      stuff.collect {
        case OnConfirmScreen => true
      }.headOption

    val newBinding: Box[FieldBinding] = (stuff.collect {
      case AFieldBinding(i) => i
    }).headOption

    val newHelp: Box[NodeSeq] = (stuff.collect {
      case Help(ns) => ns
    }).headOption

    val newTransforms: List[BaseField => NodeSeq => NodeSeq] = stuff.collect({
      case FieldTransform(func) => func
    }).toList

    val newShow: Box[BaseField => Boolean] = (stuff.collect {
      case DisplayIf(func) => func
    }).headOption

    new Field {
      type ValueType = T

      /**
       * Is this field on the confirm screen
       */
      override def onConfirm_? : Boolean = confirmInfo getOrElse super.onConfirm_?

      override def toForm: Box[NodeSeq] =
        underlying.toForm.map(ns => SHtml.ElemAttr.applyToAllElems(ns, formElemAttrs))

      /**
       * Given the current state of things, should this field be shown
       */
      override def show_? = newShow map (_(this)) openOr underlying.show_?

      /**
       * What form elements are we going to add to this field?
       */
      override lazy val formElemAttrs: Seq[SHtml.ElemAttr] = grabParams(stuff)

      /**
       * Given the current context, should this field be displayed
       */
      override def shouldDisplay_? = underlying.shouldDisplay_?

      override def displayName = underlying.displayName

      override def displayNameHtml: Box[NodeSeq] = underlying.displayNameHtml

      override def asHtml = underlying.asHtml

      override def name: String = underlying.name

      override def default = underlying.get

      override implicit def manifest: Manifest[ValueType] = man

      override def helpAsHtml = newHelp or underlying.helpAsHtml

      override def validate = underlying.validate ::: super.validate

      override def validations = stuff.collect {
        case AVal(f) => f.asInstanceOf[ValueType => List[FieldError]]
      }.toList

      override def setFilter = stuff.collect {
        case AFilter(f) => f.asInstanceOf[ValueType => ValueType]
      }.toList

      override def is = underlying.get

      override def get = underlying.get

      override def set(v: T) = underlying.set(setFilter.foldLeft(v)((v, f) => f(v)))

      override def uniqueFieldId: Box[String] = paramFieldId or underlying.uniqueFieldId or super.uniqueFieldId

      override def binding = newBinding or super.binding

      override def transforms = super.transforms ++ newTransforms
    }
  }

  sealed protected trait BoxMarker

  /**
   * A little hack because => BaseField and => Box[BaseField]
   * have the same method signature
   */
  protected implicit object BoxMarkerObj extends BoxMarker

  protected def field[T](underlying: => Box[BaseField {type ValueType = T}],
                         stuff: FilterOrValidate[T]*)(implicit man: Manifest[T], marker: BoxMarker): Field {type ValueType = T} = {
    val paramFieldId: Box[String] = (stuff.collect {
      case FormFieldId(id) => id
    }).headOption

    val newBinding: Box[FieldBinding] = (stuff.collect {
      case AFieldBinding(i) => i
    }).headOption

    val newHelp: Box[NodeSeq] = (stuff.collect {
      case Help(ns) => ns
    }).headOption

    val newTransforms: List[BaseField => NodeSeq => NodeSeq] = stuff.collect({
      case FieldTransform(func) => func
    }).toList

    val newShow: Box[BaseField => Boolean] = (stuff.collect {
      case DisplayIf(func) => func
    }).headOption

    val confirmInfo = stuff.collect {
      case NotOnConfirmScreen => false
    }.headOption orElse
      stuff.collect {
        case OnConfirmScreen => true
      }.headOption

    new Field {
      type ValueType = T

      /**
       * Is this field on the confirm screen
       */
      override def onConfirm_? : Boolean = confirmInfo getOrElse super.onConfirm_?

      override def toForm: Box[NodeSeq] = underlying
          .flatMap(_.toForm)
          .map(ns => SHtml.ElemAttr.applyToAllElems(ns, formElemAttrs))

      /**
       * Given the current state of things, should this field be shown
       */
      override def show_? = newShow map (_(this)) openOr (underlying.map(_.show_?) openOr false)

      /**
       * What form elements are we going to add to this field?
       */
      override lazy val formElemAttrs: Seq[SHtml.ElemAttr] = grabParams(stuff)

      /**
       * Given the current context, should this field be displayed
       */
      override def shouldDisplay_? = underlying.map(_.shouldDisplay_?) openOr false

      override def displayName = underlying.map(_.displayName) openOr "N/A"

      override def displayNameHtml: Box[NodeSeq] = underlying.flatMap(_.displayNameHtml)

      override def asHtml = underlying.map(_.asHtml) openOr NodeSeq.Empty

      override def name: String = underlying.map(_.name) openOr "N/A"

      override def default = underlying.openOrThrowException("legacy code").get

      override implicit def manifest: Manifest[ValueType] = man

      override def helpAsHtml = newHelp or underlying.flatMap(_.helpAsHtml)

      override def validate = underlying.toList.flatMap(_.validate) ::: super.validate

      override def validations = stuff.collect {
        case AVal(f) => f.asInstanceOf[ValueType => List[FieldError]]
      }.toList

      override def setFilter = stuff.collect {
        case AFilter(f) => f.asInstanceOf[ValueType => ValueType]
      }.toList

      override def is = underlying.openOrThrowException("Legacy code").get

      override def get = underlying.openOrThrowException("Legacy code").get

      override def set(v: T) = underlying.openOrThrowException("Legacy code").set(setFilter.foldLeft(v)((v, f) => f(v)))

      override def uniqueFieldId: Box[String] = paramFieldId or underlying.flatMap(_.uniqueFieldId) or super.uniqueFieldId

      override def binding = newBinding or super.binding

      override def transforms = super.transforms ++ newTransforms
    }
  }


  /**
   * Create a field with a name, default value, and
   *
   * @param name - the name of the field.  This is a call-by-name parameter, so you can dynamically calculate
   * the name of the fiels (e.g., localize its name)
   * @param default - the default value of the field
   * @param validate - any validation functions
   */
  protected def field[T](name: => String, default: => T, stuff: FilterOrValidate[T]*)(implicit man: Manifest[T]): Field {type ValueType = T} =
    new FieldBuilder[T](name, default, man, Empty, stuff.toList.flatMap {
      case AVal(v: Function1[_, _]) => List(v.asInstanceOf[T => List[FieldError]])
      case _ => Nil
    }, stuff.toList.flatMap {
      case AFilter(v) => List(v.asInstanceOf[T => T])
      case _ => Nil
    }, stuff).make

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
   * length and generate a validation issue if not.
   */
  protected def valMinLen(len: => Int, msg: => String): String => List[FieldError] =
    s => s match {
      case str if (null ne str) && str.length >= len => Nil
      case _ => List(FieldError(currentField.box openOr new FieldIdentifier {}, Text(msg)))
    }


  /**
   * A validation helper.  Make sure the string is no more than a particular
   * length and generate a validation issue if not.
   */
  protected def valMaxLen(len: => Int, msg: => String): String => List[FieldError] =
    s => s match {
      case str if (null eq str) || str.length <= len => Nil
      case _ => List(FieldError(currentField.box openOr new FieldIdentifier {}, Text(msg)))
    }

  /**
   * Make sure the field matches a regular expression
   */
  protected def valRegex(pat: => java.util.regex.Pattern, msg: => String): String => List[FieldError] =
    s => s match {
      case str if (null ne str) && pat.matcher(str).matches => Nil
      case _ => List(FieldError(currentField.box openOr new FieldIdentifier {}, Text(msg)))
    }

  protected def minVal[T](len: => T, msg: => String)(implicit f: T => Number): T => List[FieldError] =
    s => if (f(s).doubleValue < f(len).doubleValue) msg else Nil

  protected def maxVal[T](len: => T, msg: => String)(implicit f: T => Number): T => List[FieldError] =
    s => if (f(s).doubleValue > f(len).doubleValue) msg else Nil

  def noticeTypeToAttr(screen: AbstractScreen): Box[NoticeType.Value => MetaData] =
    inject[NoticeType.Value => MetaData] or LiftScreenRules.inject[NoticeType.Value => MetaData]


  /**
   * Create a field that's added to the Screen
   *
   * @param theName - the name of the field.  This is call-by-name, so you
   * can do things like S.?("Dog's Name") such that the string will be
   * localized
   *
   * @param defaultValue - the starting value for the field.  This is
   * also call-by-name which is handy for constructs like:
   * <code class="scala">SomeExternalRequestVarOrSessionVar.get</code>
   *
   * @theToForm - a function to convert the field into a form
   *
   * @otherValue - a handy way include other values in the field.  The other value is
   * calcualted when the field is initialized.  You can, for example, put
   * a list of valid options in the field.
   *
   * @stuff - a list of filters and validations for the field
   *
   * @return a newly minted Field
   */
  protected def makeField[T, OV](theName: => String, defaultValue: => T,
                                 theToForm: (Field {type OtherValueType = OV
                                   type ValueType = T} =>
                                   Box[NodeSeq]),
                                 otherValue: OtherValueInitializer[OV],
                                 stuff: FilterOrValidate[T]*):
  Field {type ValueType = T; type OtherValueType = OV} = {
    val newBinding: Box[FieldBinding] = (stuff.collect {
      case AFieldBinding(i) => i
    }).headOption

    val newHelp: Box[NodeSeq] = (stuff.collect {
      case Help(ns) => ns
    }).headOption

    val newTransforms: List[BaseField => NodeSeq => NodeSeq] = stuff.collect({
      case FieldTransform(func) => func
    }).toList

    val newShow: Box[BaseField => Boolean] = (stuff.collect {
      case DisplayIf(func) => func
    }).headOption

    otherValue match {
      case OtherValueInitializerImpl(otherValueInitFunc) => {
        new Field {
          type OtherValueType = OV
          type ValueType = T

          override protected def otherValueDefault: OtherValueType = otherValueInitFunc()

          override def name: String = theName

          override implicit def manifest = buildIt[T]

          override def default: T = defaultValue

          /**
           * What form elements are we going to add to this field?
           */
          override lazy val formElemAttrs: Seq[SHtml.ElemAttr] = grabParams(stuff)

          override val setFilter = stuff.flatMap {
            case AFilter(f) => List(f.asInstanceOf[ValueType => ValueType])
            case _ => Nil
          }.toList
          override val validations = stuff.flatMap {
            case AVal(v: Function1[_, _]) => List(v.asInstanceOf[T => List[FieldError]])
            case _ => Nil
          }.toList

          override def binding = newBinding

          override def helpAsHtml = newHelp

          override def toForm: Box[NodeSeq] = theToForm(this)

          override def transforms = newTransforms

          override def show_? = newShow map (_(this)) openOr (super.show_?)
        }
      }

      case _ => {
        new Field {
          type ValueType = T
          type OtherValueType = OV

          override def name: String = theName

          override implicit def manifest = buildIt[T]

          override def default: T = defaultValue

          /**
           * What form elements are we going to add to this field?
           */
          override lazy val formElemAttrs: Seq[SHtml.ElemAttr] = grabParams(stuff)

          override val setFilter = stuff.flatMap {
            case AFilter(f) => List(f.asInstanceOf[ValueType => ValueType])
            case _ => Nil
          }.toList
          override val validations = stuff.flatMap {
            case AVal(v: Function1[_, _]) => List(v.asInstanceOf[T => List[FieldError]])
            case _ => Nil
          }.toList

          override def binding = newBinding

          override def helpAsHtml = newHelp

          override def toForm: Box[NodeSeq] = theToForm(this)

          override def transforms = newTransforms

          override def show_? = newShow map (_(this)) openOr (super.show_?)
        }
      }
    }
  }

  /**
   * Create a password field
   *
   * @param name the name of the field (call-by-name)
   * @param defaultValue the starting value of the field (call-by-name)
   * @param stuff the filters, validators and attributes
   *
   * @return a newly minted Field
   */
  protected def password(name: => String, defaultValue: => String, stuff: FilterOrValidate[String]*): Field {type ValueType = String} = {
    val eAttr = grabParams(stuff)

    makeField[String, Nothing](name,
      defaultValue,
      field => SHtml.password(field.get, field.set(_), eAttr: _*),
      NothingOtherValueInitializer,
      stuff: _*)
  }

  /**
   * Create a text field
   *
   * @param name the name of the field (call-by-name)
   * @param defaultValue the starting value of the field (call-by-name)
   * @param stuff the filters, validators and attributes
   *
   * @return a newly minted Field
   */
  protected def text(name: => String, defaultValue: => String, stuff: FilterOrValidate[String]*): Field {type ValueType = String} = {
    val eAttr = grabParams(stuff)

    makeField[String, Nothing](name,
      defaultValue,
      field => SHtml.text(field.get, field.set(_), eAttr: _*),
      NothingOtherValueInitializer,
      stuff: _*)
  }


  /**
   * Create a textarea field with 80 columns and 5 rows
   *
   * @param name the name of the field (call-by-name)
   * @param defaultValue the starting value of the field (call-by-name)
   *
   * @param stuff - a list of filters and validations for the field
   *
   * @return a newly minted Field{type ValueType = String}
   */
  protected def textarea(name: => String, defaultValue: => String, stuff: FilterOrValidate[String]*): Field {type ValueType = String} =
    textarea(name, defaultValue, 5, 80, stuff: _*)


  /**
   * Create a textarea field
   *
   * @param name the name of the field (call-by-name)
   * @param defaultValue the starting value of the field (call-by-name)
   * @param rows the number of rows in the textarea
   * @param cols the number of columns in the textarea
   *
   * @param stuff - a list of filters and validations for the field
   *
   * @return a newly minted Field{type ValueType = String}
   */
  protected def textarea(name: => String, defaultValue: => String, rows: Int, cols: Int, stuff: FilterOrValidate[String]*):
  Field {type ValueType = String} = {

    val eAttr: List[SHtml.ElemAttr] = (("rows" -> rows.toString): SHtml.ElemAttr) ::
      (("cols" -> cols.toString): SHtml.ElemAttr) :: grabParams(stuff)

    makeField[String, Nothing](name,
      defaultValue,
      field => SHtml.textarea(field.is,
        field.set(_),
        eAttr: _*),
      NothingOtherValueInitializer,
      stuff: _*)
  }


  /**
   * Create a select HTML element
   *
   * @param name the name of the field (call-by-name)
   * @param default the starting value of the field (call-by-name)
   * @param choices the possible choices for the select
   *
   * @param stuff - a list of filters and validations for the field
   * @param f a PairStringPromoter (a wrapper around a function) that converts T => display String
   *
   * @return a newly minted Field{type ValueType = String}
   */
  protected def select[T](name: => String, default: => T, choices: => Seq[T], stuff: FilterOrValidate[T]*)
                         (implicit f: SHtml.PairStringPromoter[T]): Field {type ValueType = T; type OtherValueType = Seq[T]}
  = {
    val eAttr = grabParams(stuff)

    makeField[T, Seq[T]](name, default,
      field =>
        SHtml.selectElem(field.otherValue,
          Full(field.is), eAttr: _*)(field.set(_)),
      OtherValueInitializerImpl[Seq[T]](() => choices),
      stuff: _*)
  }

  /**
   * Create a multi select HTML element
   *
   * @param name the name of the field (call-by-name)
   * @param default the starting value of the field (call-by-name)
   * @param choices the possible choices for the select
   *
   * @param stuff - a list of filters and validations for the field
   * @param f a PairStringPromoter (a wrapper around a function) that converts T => display String
   *
   * @return a newly minted Field{type ValueType = String}
   */
  protected def multiselect[T](name: => String, default: => Seq[T], choices: => Seq[T], stuff: FilterOrValidate[Seq[T]]*)
                              (implicit f: SHtml.PairStringPromoter[T]): Field {type ValueType = Seq[T]; type OtherValueType = Seq[T]}
  = {
    val eAttr = grabParams(stuff)

    makeField[Seq[T], Seq[T]](name, default,
      field =>
        SHtml.multiSelectElem(field.otherValue,
          field.is, eAttr: _*)(field.set(_)),
      OtherValueInitializerImpl[Seq[T]](() => choices),
      stuff: _*)
  }

  /**
   * Grabs the FormFieldId and FormParam parameters
   */
  protected def grabParams(in: Seq[FilterOrValidate[_]]):
  List[SHtml.ElemAttr] = {
    val sl = in.toList
    in.collect {
      case FormFieldId(id) => ("id" -> id): SHtml.ElemAttr
    }.
      headOption.toList :::
      sl.collect {
        case FormParam(fp) => fp
      }
  }

  /**
   * Create a radio HTML element
   *
   * @param name the name of the field (call-by-name)
   * @param default the starting value of the field (call-by-name)
   * @param choices the possible choices for the select
   * @param stuff - a list of filters and validations for the field
   *
   * @return a newly minted Field{type ValueType = String}
   */
  protected def radio(name: => String, default: => String, choices: => Seq[String], stuff: FilterOrValidate[String]*):
  Field {type ValueType = String; type OtherValueType = Seq[String]} = {
    val eAttr = grabParams(stuff)

    makeField[String, Seq[String]](name, default,
      field =>
        Full(SHtml.radio(field.otherValue,
          Full(field.is),
          field.set _,
          eAttr: _*).toForm),
      OtherValueInitializerImpl[Seq[String]](() => choices),
      stuff: _*)
  }

}


trait ScreenWizardRendered extends Loggable {
  protected def wrapInDiv(in: NodeSeq): Elem =
    <div style="display: inline" id={FormGUID.get}>
      {in}
    </div>

  def formName: String

  def labelSuffix: NodeSeq = Text(":")

  protected def additionalFormBindings: Box[CssSel] = Empty

  protected lazy val cssClassBinding = new CssClassBinding

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
                          nextId: (String, () => JsCmd),
                          prevId: Box[(String, () => JsCmd)],
                          cancelId: (String, () => JsCmd),
                          theScreen: AbstractScreen,
                          ajax_? : Boolean): NodeSeq = {

    import FieldBindingUtils._
    import FieldBinding._

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

    def dynamicFields: List[CssBindFunc] =
      for {
        field <- fields
        bindingInfo <- field.binding
        dynamic <- Some(bindingInfo.bindingStyle) collect { case d:Dynamic => d }
      } yield {
        val template = dynamic.func()
        traceInline("Binding dynamic field %s to %s".format(bindingInfo.selector(formName), template),
          bindingInfo.selector(formName) #> bindField(field)(template))
      }

    def bindFields: CssBindFunc = {
      logger.trace("Binding fields", fields)
      List(templateFields, selfFields, defaultFields, customFields, dynamicFields).flatten.reduceLeft(_ & _)
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

      if (f.transforms.isEmpty)
        bindAll()
      else
        (bindAll() :: f.transforms.map(_ apply (f.field))).reduceLeft(_ andThen _)
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

  protected def allTemplate: NodeSeq

  protected def allTemplateNodeSeq: NodeSeq = {
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


  protected trait Snapshot {
    def restore(): Unit
  }

  protected def createSnapshot: Snapshot

  def noticeTypeToAttr(screen: AbstractScreen): Box[NoticeType.Value => MetaData]

  protected def Referer: AnyVar[String, _]

  protected def Ajax_? : AnyVar[Boolean, _]

  protected def AjaxOnDone: AnyVar[JsCmd, _]

  protected def FormGUID: AnyVar[String, _]

  /**
   * What should be done at the end of an Ajax session.  By
   * default, RedirectTo(Referer.get)
   */
  protected def calcAjaxOnDone: JsCmd = {
    val notices = S.getAllNotices

    RedirectTo(Referer.get, () => S.appendNotices(notices))
  }

  /**
   * Should all instances of this Wizard or Screen default to Ajax
   * when not explicitly set
   */
  protected def defaultToAjax_? : Boolean = false

  /**
   * Calculate the referer (the page to go back to on finish).
   * defaults to S.referer openOr "/"
   */
  protected def calcReferer: String = S.referer openOr "/"

  /**
   * Calculate if this Screen/Wizard should be ajax
   */
  protected def calcAjax: Boolean = S.attr("ajax").flatMap(Helpers.asBoolean) openOr defaultToAjax_?

  protected def redirectBack(): JsCmd = {
    if (ajaxForms_?) {
      AjaxOnDone.get
    } else {
      S.seeOther(Referer.get)
    }
  }


  /**
   * Are the forms Ajax or regular HTTP/HTML.
   *
   * If the ajax=true attribute is present on the original snippet
   * invocation, the forms will be ajax.
   */
  protected def ajaxForms_? : Boolean = Ajax_?.get
}


case class ScreenFieldInfo(
    field: BaseField,
    text: NodeSeq,
    help: Box[NodeSeq],
    input: Box[NodeSeq],
    binding: Box[FieldBinding],
    transforms: List[BaseField => NodeSeq => NodeSeq]) {
  def this(field: BaseField, text: NodeSeq, help: Box[NodeSeq], input: Box[NodeSeq]) =
    this(field, text, help, input, Empty, Nil)
 }

object ScreenFieldInfo {
  def apply(field: BaseField, text: NodeSeq, help: Box[NodeSeq], input: Box[NodeSeq]) =
    new ScreenFieldInfo(field, text, help, input)
}

trait LiftScreen extends AbstractScreen with StatefulSnippet with ScreenWizardRendered {
  def dispatch = {
    case _ => template => {
      _defaultXml.set(template)
      this.toForm
    }
  }

  protected object SavedDefaultXml extends ScreenVar[NodeSeq](defaultXml) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  protected object LocalAction extends TransientRequestVar[String]("") {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  protected object LocalActionRef extends RequestVar[String](S.fmapFunc(setLocalAction _)(s => s)) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  protected object CancelId extends TransientRequestVar[String]("") {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  protected object LocalActions extends ScreenVar[AtomicReference[Map[String, () => JsCmd]]](
      new AtomicReference[Map[String, () => JsCmd]](Map.empty)) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  /**
   * By default, are all the fields on this screen on the confirm screen?
   */
  def onConfirm_? : Boolean = true

  /**
   * Holds the template passed via the snippet for the duration
   * of the request
   */
  protected object _defaultXml extends TransientRequestVar[NodeSeq](NodeSeq.Empty) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  /**
   * the NodeSeq passed as a parameter when the snippet was invoked
   */
  protected def defaultXml: NodeSeq = _defaultXml.get

  private object ScreenVars extends TransientRequestVar[Map[String, (NonCleanAnyVar[_], Any)]](Map()) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  protected object PrevSnapshot extends TransientRequestVar[Box[ScreenSnapshot]](Empty) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  protected object Referer extends ScreenVar[String](calcReferer) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  /**
   * A unique GUID for the form... this allows us to do an Ajax SetHtml
   * to replace the form
   */
  protected object FormGUID extends ScreenVar[String](Helpers.nextFuncName) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  protected object Ajax_? extends ScreenVar[Boolean](calcAjax) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  object NextId extends ScreenVar[String](Helpers.nextFuncName) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  /**
   * What to do when the Screen is done.  By default, will
   * do a redirect back to Whence, but you can change this behavior,
   * for example, put up some other Ajax thing or alternatively,
   * remove the form from the screen.
   */
  protected object AjaxOnDone extends ScreenVar[JsCmd](calcAjaxOnDone) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

  private object FirstTime extends ScreenVar[Boolean](true) {
    override lazy val __nameSalt = Helpers.nextFuncName
  }

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

  protected def createSnapshot = {
    val prev = PrevSnapshot.get
    new ScreenSnapshot(ScreenVars.get, prev)
  }

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

    override protected def wasInitialized(name: String, bn: String): Boolean = {
      val old: Boolean = ScreenVarHandler.get(bn) openOr false
      ScreenVarHandler.set(bn, this, true)
      old
    }

    override protected def testWasSet(name: String, bn: String): Boolean = {
      ScreenVarHandler.get(name).isDefined || (ScreenVarHandler.get(bn) openOr false)
    }

    /**
     * Different Vars require different mechanisms for synchronization. This method implements
     * the Var specific synchronization mechanism
     */
    def doSync[F](f: => F): F = f

    // no sync necessary for RequestVars... always on the same thread
  }


  private object ScreenVarHandler {
    def get[T](name: String): Box[T] =
      ScreenVars.is.get(name).map(_._2.asInstanceOf[T])


    def set[T](name: String, from: ScreenVar[_], value: T): Unit =
      ScreenVars.set(ScreenVars.get + (name -> (from, value)))

    def clear(name: String): Unit =
      ScreenVars.set(ScreenVars.get - name)
  }

  protected def bindLocalAction(selector: String, func: () => JsCmd): CssSel = {
    mapLocalAction(func)(name =>
      selector #> (
        SHtml.makeAjaxCall(LiftRules.jsArtifacts.serialize(NextId.get) + ("&" + LocalActionRef.get + "=" + name)).cmd
      ).toJsCmd)
  }

  protected def mapLocalAction[T](func: () => JsCmd)(f: String => T): T = {
    val name = randomString(20)
    val ref = LocalActions.get
    ref.synchronized {
      ref.set(ref.get + (name -> func))
    }
    f(name)
  }

  protected def setLocalAction(s: String) {
    logger.trace("Setting LocalAction (%s) to %s".format(
      Integer.toString(System.identityHashCode(LocalAction), 16), s))
    LocalAction.set(s)
  }


  def toForm: NodeSeq = {
    Referer.get // touch to capture the referer
    Ajax_?.get // capture the ajaxiness of these forms
    FormGUID.get
    NextId.get
    SavedDefaultXml.get
    LocalActionRef.get

    if (FirstTime) {
      FirstTime.set(false)
      localSetup()

      val localSnapshot = createSnapshot
      // val notices = S.getAllNotices

      // if we're not Ajax,
      if (!ajaxForms_?) {
        S.seeOther(S.uri, () => {
          // S.appendNotices(notices)
          localSnapshot.restore
        })
      }
    }

    val form = renderHtml()
    if (ajaxForms_?) wrapInDiv(form) else form
  }

  protected def renderHtml(): NodeSeq = {
    val finishId = NextId.get
    val cancelId = Helpers.nextFuncName

    val theScreen = this

    val finishButton = theScreen.finishButton %
      ("onclick" ->
        (if (ajaxForms_?) {
          SHtml.makeAjaxCall(LiftRules.jsArtifacts.serialize(finishId)).toJsCmd
        } else {
          "document.getElementById(" + finishId.encJs + ").submit()"
        }))

    val cancelButton: Elem = theScreen.cancelButton %
      ("onclick" ->
        (if (ajaxForms_?) {
          SHtml.makeAjaxCall(LiftRules.jsArtifacts.serialize(cancelId)).toJsCmd
        } else {
          "document.getElementById(" + cancelId.encJs + ").submit()"
        }))

    val url = S.uri

    def fieldBinding(field: BaseField): Box[FieldBinding] =
      field match {
        case f: Field => f.binding
        case _ => Empty
      }

    def fieldTransform(field: BaseField): List[BaseField => NodeSeq => NodeSeq] =
      field match {
        case f: Field => f.transforms
        case _ => Nil
      }

    CancelId.set(cancelId)

    renderAll(
      Empty, //currentScreenNumber: Box[NodeSeq],
      Empty, //screenCount: Box[NodeSeq],
      Empty, // wizardTop: Box[Elem],
      theScreen.screenTop, //screenTop: Box[Elem],
      theScreen.screenFields.filter(_.shouldDisplay_?).flatMap(f =>
        if (f.show_?) List(ScreenFieldInfo(f, f.displayHtml, f.helpAsHtml, f.toForm, fieldBinding(f), fieldTransform(f))) else Nil), //fields: List[ScreenFieldInfo],
      Empty, // prev: Box[Elem],
      Full(cancelButton), // cancel: Box[Elem],
      Empty, // next: Box[Elem],
      Full(finishButton), //finish: Box[Elem],
      theScreen.screenBottom, // screenBottom: Box[Elem],
      Empty, //wizardBottom: Box[Elem],
      finishId -> doFinish _,
      Empty,
      cancelId -> (() => {
        redirectBack()
      }), //cancelId: (String, () => Unit),
      theScreen,
      ajaxForms_?)
  }

  protected def allTemplatePath: List[String] = LiftScreenRules.allTemplatePath.vend

  protected def allTemplate: NodeSeq = {
    val ret = Templates(allTemplatePath) openOr allTemplateNodeSeq

    ret
  }

  /**
   * What additional attributes should be put on
   */
  protected def formAttrs: MetaData = scala.xml.Null

  protected def finish(): Unit

  protected def savedDefaultXml = SavedDefaultXml.get

  protected def doFinish(): JsCmd= {
    val fMap: Map[String, () => JsCmd] = LocalActions.get.get
    if (! LocalAction.get.isEmpty)
      fMap.get(LocalAction.get) map (_()) getOrElse (
        throw new IllegalArgumentException("No local action available with that binding"))
    else {
      validate match {
        case Nil =>
          val snapshot = createSnapshot
          PrevSnapshot.set(Full(snapshot))
          finish()
          redirectBack()
        case xs => {
          S.error(xs)
          if (ajaxForms_?) {
            replayForm
          } else {
            Noop
          }
        }
      }
    }
  }

  protected def renderWithErrors(errors: List[FieldError]) {
    S.error(errors)
    AjaxOnDone.set(replayForm)
  }

  protected def renderFormCmd: JsCmd = SetHtml(FormGUID, renderHtml())

  protected def replayForm: JsCmd = renderFormCmd
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
      case NoticeType.Warning => new UnprefixedAttribute("class", "lift_warning", Null)
      case NoticeType.Error => new UnprefixedAttribute("class", "lift_error", Null)
    }: PartialFunction[NoticeType.Value, MetaData]) {}


}

case class FieldBinding(val fieldName: String, val bindingStyle: FieldBinding.BindingStyle) {
  def fieldId(formName: String) = "%s_%s_field" format (formName, fieldName)
  def selector(formName: String) = "#%s" format (fieldId(formName))
  def childSelector(formName: String) = "#%s *" format (fieldId(formName))
  def idSelector(formName: String) = selector(formName) + " [id]"
}

object FieldBinding {
  sealed abstract class BindingStyle

  /**
   * Bind the field using the default template defined in an external template (as in Bind.helpers()-based binding)
   */
  case object Template extends BindingStyle

  /**
   * Bind the field using the template defined in the body of the field reference
   */
  case object Self extends BindingStyle

  /**
   * Bind the field using the template returned by the <code>defaultFieldNodeSeq</code> method
   */
  case object Default extends BindingStyle

  /**
   * Bind the field using the template provided
   */
  case class Custom(template: NodeSeq) extends BindingStyle

  /**
   * Bind the field using the results of a function.  The provided function will be called
   * every time the field is rendered.
   */
  case class Dynamic(func: () => NodeSeq) extends BindingStyle

  def apply(fieldName: String) = new FieldBinding(fieldName, Default)
}
