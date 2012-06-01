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


/**
 * The trait that forms the basis for LiftScreen and the
 * Screen instances in Wizard
 */
trait AbstractScreen extends Factory {
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
   * <br/>
   * WARNING -- this method is public so it can be called
   * from a Wizard.  This method should only be called from within
   * the Screen or Wizard that owns the Screen and not
   * from external code.
   */
  def addFields(fields: () => FieldContainer) {
    _fieldList = _fieldList ::: List(fields)
  }

  /**
   * Use addFields
   *
   * @deprecated
   */
  @deprecated("use addFields()")
  protected def _register(field: () => FieldContainer) = addFields(field)

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

  // an implicit coversion so we don't have to put Full around every Elem
  protected implicit def elemInABox(in: Elem): Box[Elem] = Box !! in

  /**
   * The name of the screen.  Override this to change the screen name
   */
  def screenName: String = "Screen"

  def screenNameAsHtml: NodeSeq = Text(screenName)

  def screenTitle: NodeSeq = screenNameAsHtml

  def cancelButton: Elem = <button>
    {S.??("Cancel")}
  </button>

  def finishButton: Elem = <button>
    {S.??("Finish")}
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

    override def toString = is.toString

    def binding: Box[FieldBinding] = Empty

    def transform: Box[() => CssSel] = Empty
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

      val newHelp: Box[NodeSeq] = help or (stuff.collect {
        case Help(ns) => ns
      }).headOption

      val newTransform: Box[() => CssSel] = (stuff.collect {
        case FieldTransform(func) => func
      }).headOption

      val newShow: Box[() => Boolean] = (stuff.collect {
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

        override def transform = newTransform

        override def show_? = newShow map (_()) openOr (super.show_?)
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
   * the name of the fiels (e.g., localize its name)
   * @param default - the default value of the field
   * @param validate - any validation functions
   */
  protected def builder[T](name: => String, default: => T, stuff: FilterOrValidate[T]*)(implicit man: Manifest[T]): FieldBuilder[T] =
    new FieldBuilder[T](name, default, man, Empty,
      stuff.toList.collect {
        case AVal(v) => v
      },
      stuff.toList.collect {
        case AFilter(v) => v
      },
      stuff)

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

  protected final case class FieldTransform(func: () => CssSel) extends FilterOrValidate[Nothing]

  protected final case class DisplayIf(func: () => Boolean) extends FilterOrValidate[Nothing]

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

    val newTransform: Box[() => CssSel] = (stuff.collect {
      case FieldTransform(func) => func
    }).headOption

    val newShow: Box[() => Boolean] = (stuff.collect {
      case DisplayIf(func) => func
    }).headOption

    new Field {
      type ValueType = T

      /**
       * Is this field on the confirm screen
       */
      override def onConfirm_? : Boolean = confirmInfo getOrElse super.onConfirm_?

      override def toForm: Box[NodeSeq] = underlying.toForm

      /**
       * Give the current state of things, should the this field be shown
       */
      override def show_? = newShow map (_()) openOr underlying.show_?

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

      override def validate: List[FieldError] = underlying.validate

      /*
      override def validations = stuff.collect {
        case AVal(f) => f
      }.toList ::: underlying.validations
      */

      override def setFilter = stuff.collect {
        case AFilter(f) => f
      }.toList

      override def is = underlying.get

      override def get = underlying.get

      override def set(v: T) = underlying.set(setFilter.foldLeft(v)((v, f) => f(v)))

      override def uniqueFieldId: Box[String] = paramFieldId or underlying.uniqueFieldId or super.uniqueFieldId

      override def binding = newBinding or super.binding

      override def transform = newTransform or super.transform
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

    val newTransform: Box[() => CssSel] = (stuff.collect {
      case FieldTransform(func) => func
    }).headOption

    val newShow: Box[() => Boolean] = (stuff.collect {
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

      override def toForm: Box[NodeSeq] = underlying.flatMap(_.toForm)

      /**
       * Give the current state of things, should the this field be shown
       */
      override def show_? = newShow map (_()) openOr (underlying.map(_.show_?) openOr false)

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

      override def default = underlying.open_!.get

      override implicit def manifest: Manifest[ValueType] = man

      override def helpAsHtml = newHelp or underlying.flatMap(_.helpAsHtml)

      override def validate: List[FieldError] = underlying.toList.flatMap(_.validate)

      override def setFilter = stuff.collect {
        case AFilter(f) => f
      }.toList

      override def is = underlying.open_!.get

      override def get = underlying.open_!.get

      override def set(v: T) = underlying.open_!.set(setFilter.foldLeft(v)((v, f) => f(v)))

      override def uniqueFieldId: Box[String] = paramFieldId or underlying.flatMap(_.uniqueFieldId) or super.uniqueFieldId

      override def binding = newBinding or super.binding

      override def transform = newTransform or super.transform
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
      case AVal(v) => List(v)
      case _ => Nil
    }, stuff.toList.flatMap {
      case AFilter(v) => List(v)
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
   * length and generate a validation issue if not
   */
  protected def valMinLen(len: => Int, msg: => String): String => List[FieldError] =
    s => s match {
      case str if (null ne str) && str.length >= len => Nil
      case _ => List(FieldError(currentField.box openOr new FieldIdentifier {}, Text(msg)))
    }


  /**
   * A validation helper.  Make sure the string is no more than a particular
   * length and generate a validation issue if not
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

    val newTransform: Box[() => CssSel] = (stuff.collect {
      case FieldTransform(func) => func
    }).headOption

    val newShow: Box[() => Boolean] = (stuff.collect {
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
            case AFilter(f) => List(f)
            case _ => Nil
          }.toList
          override val validations = stuff.flatMap {
            case AVal(v) => List(v)
            case _ => Nil
          }.toList

          override def binding = newBinding

          override def helpAsHtml = newHelp

          override def toForm: Box[NodeSeq] = theToForm(this)

          override def transform = newTransform

          override def show_? = newShow map (_()) openOr (super.show_?)
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
            case AFilter(f) => List(f)
            case _ => Nil
          }.toList
          override val validations = stuff.flatMap {
            case AVal(v) => List(v)
            case _ => Nil
          }.toList

          override def binding = newBinding

          override def helpAsHtml = newHelp

          override def toForm: Box[NodeSeq] = theToForm(this)

          override def transform = newTransform

          override def show_? = newShow map (_()) openOr (super.show_?)
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
   * Create a textarea Field with 80 columns and 5 rows
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
   * Create a textarea Field
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
   *
   * @param stuff - a list of filters and validations for the field
   * @param f a PairStringPromoter (a wrapper around a function) that converts T => display String
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


trait ScreenWizardRendered {
  protected def wrapInDiv(in: NodeSeq): Elem =
    <div style="display: inline" id={FormGUID.get}>
      {in}
    </div>

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

    val notices: List[(NoticeType.Value, NodeSeq, Box[String])] = S.getAllNotices

    def bindFieldLine(xhtml: NodeSeq): NodeSeq = {
      fields.flatMap {
        f =>
          val theFormEarly = f.input
          val curId = theFormEarly.flatMap(Helpers.findId) or
            f.field.uniqueFieldId openOr Helpers.nextFuncName

          val theForm = theFormEarly.map {
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
          def doLabel(in: NodeSeq): NodeSeq =
            myNotices match {
              case Nil => bind("wizard", in, AttrBindParam("for", curId, "for"), "bind" -%> f.text)
              case _ =>
                val maxN = myNotices.map(_._1).sortWith {
                  _.id > _.id
                }.head // get the maximum type of notice (Error > Warning > Notice)
                val metaData: MetaData = noticeTypeToAttr(theScreen).map(_(maxN)) openOr Null
                bind("wizard", in, AttrBindParam("for", curId, "for"), "bind" -%> f.text).map {
                  case e: Elem => e % metaData
                  case x => x
                }
            }
          bind("wizard", xhtml,
            "label" -%> doLabel _,
            "form" -%> theForm,
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
                  (innerXml => xs.flatMap {
                    case (noticeType, msg, _) =>
                      val metaData: MetaData = noticeTypeToAttr(theScreen).map(_(noticeType)) openOr Null
                      bind("wizard", innerXml, "bind" -%> msg).map {
                        case e: Elem => e % metaData
                        case x => x
                      }
                  }))
              }
            }))
      }
    }

    def url = S.uri

    val snapshot = createSnapshot

    def bindErrors(xhtml: NodeSeq): NodeSeq = notices.filter(_._3.isEmpty) match {
      case Nil => NodeSeq.Empty
      case xs =>
        def doErrors(in: NodeSeq): NodeSeq = xs.flatMap {
          case (noticeType, msg, _) =>
            val metaData: MetaData = noticeTypeToAttr(theScreen).map(_(noticeType)) openOr Null
            bind("wizard", in, "bind" -%>
              (msg)).map {
              case e: Elem => e % metaData
              case x => x
            }
        }

        bind("wizard", xhtml,
          "item" -%> doErrors _)
    }

    def bindFields(xhtml: NodeSeq): NodeSeq = {
      val ret =
        (<form id={nextId._1} action={url}
               method="post">
          {S.formGroup(-1)(SHtml.hidden(() =>
            snapshot.restore()))}{bind("wizard", xhtml,
            "line" -%> bindFieldLine _)}{S.formGroup(4)(
            SHtml.hidden(() => {
              val res = nextId._2();
              if (!ajax_?) {
                val localSnapshot = createSnapshot
                S.seeOther(url, () => {
                  localSnapshot.restore
                })
              }
              res
            }))}
        </form> %
          theScreen.additionalAttributes) ++
          prevId.toList.map {
            case (id, func) =>
              <form id={id} action={url} method="post">
                {SHtml.hidden(() => {
                snapshot.restore();
                val res = func();
                if (!ajax_?) {
                  val localSnapshot = createSnapshot;
                  S.seeOther(url, () => localSnapshot.restore)
                }
                res
              })}
              </form>
          } ++
          <form id={cancelId._1} action={url} method="post">
            {SHtml.hidden(() => {
            snapshot.restore();
            val res = cancelId._2() // WizardRules.deregisterWizardSession(CurrentSession.is)
            if (!ajax_?) {
              S.seeOther(Referer.get)
            }
            res
          })}
          </form>

      if (ajax_?) {
        SHtml.makeFormsAjax(ret)
      } else {
        ret
      }
    }

    def bindScreenInfo(xhtml: NodeSeq): NodeSeq = (currentScreenNumber, screenCount) match {
      case (Full(num), Full(cnt)) =>
        bind("wizard", xhtml, "screen_number" -%> num /*Text(CurrentScreen.is.map(s => (s.myScreenNum + 1).toString) openOr "")*/ ,
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
      <wizard:screen_info>
        <div>Page
            <wizard:screen_number/>
          of
            <wizard:total_screens/>
        </div>
      </wizard:screen_info>
      <wizard:wizard_top>
        <div>
            <wizard:bind/>
        </div>
      </wizard:wizard_top>
      <wizard:screen_top>
        <div>
            <wizard:bind/>
        </div>
      </wizard:screen_top>
      <wizard:errors>
        <div>
          <ul>
            <wizard:item>
              <li>
                  <wizard:bind/>
              </li>
            </wizard:item>
          </ul>
        </div>
      </wizard:errors>
      <div>
        <wizard:fields>
          <table>
            <wizard:line>
              <tr>
                <td>
                  <wizard:label>
                    <label wizard:for=" ">
                        <wizard:bind/>
                    </label>
                  </wizard:label>
                  <wizard:help>
                    <span>
                        <wizard:bind/>
                    </span>
                  </wizard:help> <wizard:field_errors>
                  <ul>
                    <wizard:error>
                      <li>
                          <wizard:bind/>
                      </li>
                    </wizard:error>
                  </ul>
                </wizard:field_errors>
                </td>
                <td>
                    <wizard:form/>
                </td>
              </tr>
            </wizard:line>
          </table>
        </wizard:fields>
      </div>
      <div>
        <table>
          <tr>
            <td>
                <wizard:prev/>
            </td> <td>
              <wizard:cancel/>
          </td> <td>
              <wizard:next/>
          </td>
          </tr>
        </table>
      </div>
      <wizard:screen_bottom>
        <div>
            <wizard:bind/>
        </div>
      </wizard:screen_bottom>
      <wizard:wizard_bottom>
        <div>
            <wizard:bind/>
        </div>
      </wizard:wizard_bottom>
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
   * Should all instances of this Wizard or Screen unless
   * they are explicitly set to Ajax
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
    field: FieldIdentifier,
    text: NodeSeq,
    help: Box[NodeSeq],
    input: Box[NodeSeq],
    binding: Box[FieldBinding],
    transform: Box[() => CssSel]) {
  def this(field: FieldIdentifier, text: NodeSeq, help: Box[NodeSeq], input: Box[NodeSeq]) =
    this(field, text, help, input, Empty, Empty)
 }

object ScreenFieldInfo {
  def apply(field: FieldIdentifier, text: NodeSeq, help: Box[NodeSeq], input: Box[NodeSeq]) =
    new ScreenFieldInfo(field, text, help, input)
}

trait LiftScreen extends AbstractScreen with StatefulSnippet with ScreenWizardRendered {
  def dispatch = {
    case _ => template => {
      _defaultXml.set(template)
      this.toForm
    }
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

  def toForm: NodeSeq = {
    Referer.get // touch to capture the referer
    Ajax_?.get // capture the ajaxiness of these forms
    FormGUID.get
    NextId.get

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

    def fieldTransform(field: BaseField): Box[() => CssSel] =
      field match {
        case f: Field => f.transform
        case _ => Empty
      }

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
   * What additional attributes should be put on the
   */
  protected def formAttrs: MetaData = scala.xml.Null

  protected def finish(): Unit

  protected def doFinish(): JsCmd = {
    validate match {
      case Nil =>
        val snapshot = createSnapshot
        PrevSnapshot.set(Full(snapshot))
        finish()
        redirectBack()

      case xs => {
        S.error(xs)
        if (ajaxForms_?) {
          SetHtml(FormGUID, renderHtml())
        } else {
          Noop
        }
      }
    }
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

  def apply(fieldName: String) = new FieldBinding(fieldName, Default)
}
