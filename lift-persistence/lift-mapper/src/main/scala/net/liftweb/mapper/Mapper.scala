/*
 * Copyright 2006-2010 WorldWide Conferencing, LLC
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
package mapper {

import _root_.scala.collection.mutable._
import _root_.scala.xml.{Elem, NodeSeq}
import _root_.net.liftweb.http.S
import S._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.util.{FieldError, FieldContainer, BaseField}
import _root_.net.liftweb.common.{Box, Empty, Full, ParamFailure}

trait BaseMapper extends FieldContainer {
  type MapperType <: Mapper[MapperType]

  def dbName: String
  def save: Boolean
}

@serializable
trait Mapper[A<:Mapper[A]] extends BaseMapper {
  self: A =>
  type MapperType = A

  private val secure_# = Safe.next
  private var was_deleted_? = false
  private var dbConnectionIdentifier: Box[ConnectionIdentifier] = Empty
  private[mapper] var addedPostCommit = false
  @volatile private[mapper] var persisted_? = false

  def getSingleton : MetaMapper[A];
  final def safe_? : Boolean = {
    Safe.safe_?(secure_#)
  }

  def dbName:String = getSingleton.dbName

  implicit def thisToMappee(in: Mapper[A]): A = this.asInstanceOf[A]

  def runSafe[T](f : => T) : T = {
    Safe.runSafe(secure_#)(f)
  }

  def connectionIdentifier(id: ConnectionIdentifier): A = {
    if (id != getSingleton.dbDefaultConnectionIdentifier || dbConnectionIdentifier.isDefined) dbConnectionIdentifier = Full(id)
    thisToMappee(this)
  }

  def connectionIdentifier = dbConnectionIdentifier openOr calcDbId

  def dbCalculateConnectionIdentifier: PartialFunction[A, ConnectionIdentifier] = Map.empty

  private def calcDbId = if (dbCalculateConnectionIdentifier.isDefinedAt(this)) dbCalculateConnectionIdentifier(this)
  else getSingleton.dbDefaultConnectionIdentifier

  /**
   * Append a function to perform after the commit happens
   * @param func - the function to perform after the commit happens
   */
  def doPostCommit(func: () => Unit): A = {
    DB.appendPostFunc(connectionIdentifier, func)
    this
  }

  /**
   * Save the instance and return the instance
   */
  def saveMe(): A = {
    this.save
    this
  }

  def save(): Boolean = {
    runSafe {
      getSingleton.save(this)
    }
  }

  def htmlLine : NodeSeq = {
    getSingleton.doHtmlLine(this)
  }

  def asHtml : NodeSeq = {
    getSingleton.asHtml(this)
  }

  /**
   * If the instance calculates any additional
   * fields for JSON object, put the calculated fields
   * here
   */
  def suplementalJs(ob: Box[KeyObfuscator]): List[(String, JsExp)] = Nil

  def validate : List[FieldError] = {
    runSafe {
      getSingleton.validate(this)
    }
  }

  /**
   * Returns the instance in a Full Box if the instance is valid, otherwise
   * returns a Failure with the validation errors
   */
  def asValid: Box[A] = validate match {
    case Nil => Full(this)
    case xs => ParamFailure(xs.map(_.msg.text).mkString(", "), Empty, Empty, xs)
  }

  /**
   * Convert the model to a JavaScript object
   */
  def asJs: JsExp = getSingleton.asJs(this)

  /**
   * Delete the model from the RDBMS
   */
  def delete_! : Boolean = {
    if (!db_can_delete_?) false else
    runSafe {
      was_deleted_? = getSingleton.delete_!(this)
      was_deleted_?
    }
  }

  /**
   * Get the fields (in order) for displaying a form
   */
  def formFields: List[MappedField[_, A]] =
  getSingleton.formFields(this)

   def allFields: scala.collection.Seq[BaseField] = formFields

  /**
   * map the fields titles and forms to generate a list
   * @param func called with displayHtml, fieldId, form
   */
  def mapFieldTitleForm[T](func: (NodeSeq, Box[NodeSeq], NodeSeq) => T): List[T] =
  getSingleton.mapFieldTitleForm(this, func)


  /**
   * flat map the fields titles and forms to generate a list
   * @param func called with displayHtml, fieldId, form
   */
  def flatMapFieldTitleForm[T]
  (func: (NodeSeq, Box[NodeSeq], NodeSeq) => scala.collection.Seq[T]): List[T] =
  getSingleton.flatMapFieldTitleForm(this, func)

    /**
   * flat map the fields titles and forms to generate a list
   * @param func called with displayHtml, fieldId, form
   */
  def flatMapFieldTitleForm2[T]
  (func: (NodeSeq, MappedField[_, A], NodeSeq) => scala.collection.Seq[T]): List[T] =
  getSingleton.flatMapFieldTitleForm2(this, func)

  /**
   * Present the model as a form and execute the function on submission of the form
   *
   * @param button - If it's Full, put a submit button on the form with the value of the parameter
   * @param onSuccess - redirect to the URL if the model validates, otherwise display the errors
   *
   * @return the form
   */
  def toForm(button: Box[String], onSuccess: String): NodeSeq =
  toForm(button, (what: A) => {what.validate match {
        case Nil => what.save ; S.redirectTo(onSuccess)
        case xs => S.error(xs)
      }})

  /**
   * Present the model as a HTML using the same formatting as toForm
   *
   * @return the html view of the model
   */
  def toHtml: NodeSeq = getSingleton.toHtml(this)

  /**
   * Present the model as a form and execute the function on submission of the form
   *
   * @param button - If it's Full, put a submit button on the form with the value of the parameter
   * @param f - the function to execute on form submission
   *
   * @return the form
   */
  def toForm(button: Box[String], f: A => Any): NodeSeq =
  getSingleton.toForm(this) ++
  S.fmapFunc((ignore: List[String]) => f(this)){
    (name: String) =>
    (<input type='hidden' name={name} value="n/a" />)} ++
  (button.map(b => getSingleton.formatFormElement( <xml:group>&nbsp;</xml:group> , <input type="submit" value={b}/> )) openOr _root_.scala.xml.Text(""))

  def toForm(button: Box[String], redoSnippet: NodeSeq => NodeSeq, onSuccess: A => Unit): NodeSeq = {
    val snipName = S.currentSnippet
    def doSubmit() {
      this.validate match {
        case Nil => onSuccess(this)
        case xs => S.error(xs)
          snipName.foreach(n => S.mapSnippet(n, redoSnippet))
      }
    }

    getSingleton.toForm(this) ++
    S.fmapFunc((ignore: List[String]) => doSubmit())(name => <input type='hidden' name={name} value="n/a" />) ++
    (button.map(b => getSingleton.formatFormElement( <xml:group>&nbsp;</xml:group> , <input type="submit" value={b}/> )) openOr _root_.scala.xml.Text(""))
  }

  def saved_? : Boolean = getSingleton.saved_?(this)

  /**
   * Can this model object be deleted?
   */
  def db_can_delete_? : Boolean =  getSingleton.saved_?(this) && !was_deleted_?

  def dirty_? : Boolean = getSingleton.dirty_?(this)

  override def toString = {
    val ret = new StringBuilder

    ret.append(this.getClass.getName)

    ret.append("={")

    ret.append(getSingleton.appendFieldToStrings(this))

    ret.append("}")

    ret.toString
  }

  def toXml: Elem = {
    getSingleton.toXml(this)
  }

  def checkNames {
    runSafe {
      getSingleton match {
        case null =>
        case s => s.checkFieldNames(this)
      }
    }
  }

  def comparePrimaryKeys(other: A) = false

  /**
   * Find the field by name
   * @param fieldName -- the name of the field to find
   *
   * @return Box[MappedField]
   */
  def fieldByName[T](fieldName: String): Box[MappedField[T, A]] = getSingleton.fieldByName[T](fieldName, this)

  type FieldPF = PartialFunction[String, NodeSeq => NodeSeq]

  def fieldMapperPF(transform: (BaseOwnedMappedField[A] => NodeSeq)): FieldPF = {
    getSingleton.fieldMapperPF(transform, this)
  }

  private var fieldPF_i: FieldPF = Map.empty

  def fieldPF = fieldPF_i

  def appendField(pf: FieldPF) {
    fieldPF_i = fieldPF_i orElse pf
    fieldPF_i
  }

  def prependField(pf: FieldPF) {
    fieldPF_i = pf orElse fieldPF_i
    fieldPF_i
  }

  /**
   * If there's a field in this record that defines the locale, return it
   */
  def localeField: Box[MappedLocale[A]] = Empty

  def timeZoneField: Box[MappedTimeZone[A]] = Empty

  def countryField: Box[MappedCountry[A]] = Empty
}

trait LongKeyedMapper[OwnerType <: LongKeyedMapper[OwnerType]] extends KeyedMapper[Long, OwnerType] with BaseLongKeyedMapper {
  self: OwnerType =>
}

trait BaseKeyedMapper extends BaseMapper {
  type TheKeyType
  type KeyedMapperType <: KeyedMapper[TheKeyType, KeyedMapperType]

  def primaryKeyField: MappedField[TheKeyType, MapperType] with IndexedField[TheKeyType]
  /**
   * Delete the model from the RDBMS
   */
  def delete_! : Boolean
}

trait BaseLongKeyedMapper extends BaseKeyedMapper {
  override type TheKeyType = Long
}

trait IdPK /* extends BaseLongKeyedMapper */ {
  self: BaseLongKeyedMapper =>
  def primaryKeyField = id
  object id extends MappedLongIndex[MapperType](this.asInstanceOf[MapperType])
}

/**
 * A trait you can mix into a Mapper class that gives you
 * a createdat column
 */
trait CreatedTrait {
  self: BaseMapper =>

  import _root_.net.liftweb.util._

  /**
   * Override this method to index the createdAt field
   */
  protected def createdAtIndexed_? = false

  /**
   * The createdAt field.  You can change the behavior of this
   * field:
   * <pre>
   * override lazy val createdAt = new MyCreatedAt(this) {
   *   override def dbColumnName = "i_eat_time"
   * }
   * </pre>
   */
  lazy val createdAt: MappedDateTime[MapperType] = new MyCreatedAt(this)

  protected class MyCreatedAt(obj: self.type) extends MappedDateTime[MapperType](obj.asInstanceOf[MapperType]) {
    override def defaultValue = Helpers.now
    override def dbIndexed_? = createdAtIndexed_?
  }

}

/**
 * A trait you can mix into a Mapper class that gives you
 * an updatedat column
 */
trait UpdatedTrait {
  self: BaseMapper =>

  import _root_.net.liftweb.util._

  /**
   * Override this method to index the updatedAt field
   */
  protected def updatedAtIndexed_? = false

  /**
   * The updatedAt field.  You can change the behavior of this
   * field:
   * <pre>
   * override lazy val updatedAt = new MyUpdatedAt(this) {
   *   override def dbColumnName = "i_eat_time_for_breakfast"
   * }
   * </pre>
   */
  lazy val updatedAt: MyUpdatedAt = new MyUpdatedAt(this)

  protected class MyUpdatedAt(obj: self.type) extends MappedDateTime(obj.asInstanceOf[MapperType]) with LifecycleCallbacks {
    override def beforeSave() {super.beforeSave; this.set(Helpers.now)}
    override def defaultValue = Helpers.now
       override def dbIndexed_? = updatedAtIndexed_?
  }

}

/**
* Mix this trait into your Mapper instance to get createdAt and updatedAt fields.
*/
trait CreatedUpdated extends CreatedTrait with UpdatedTrait {
  self: BaseMapper =>

}

trait KeyedMapper[KeyType, OwnerType<:KeyedMapper[KeyType, OwnerType]] extends Mapper[OwnerType] with BaseKeyedMapper {
  self: OwnerType =>

  type TheKeyType = KeyType
  type KeyedMapperType = OwnerType

  def primaryKeyField: MappedField[KeyType, OwnerType] with IndexedField[KeyType]
  def getSingleton: KeyedMetaMapper[KeyType, OwnerType];

  override def comparePrimaryKeys(other: OwnerType) = primaryKeyField.is == other.primaryKeyField.is

  def reload: OwnerType = getSingleton.find(By(primaryKeyField, primaryKeyField)) openOr this

  def asSafeJs(f: KeyObfuscator): JsExp = getSingleton.asSafeJs(this, f)

  override def hashCode(): Int = primaryKeyField.is.hashCode

  override def equals(other: Any): Boolean = {
    other match {
      case null => false
      case km: KeyedMapper[Nothing, Nothing] if this.getClass.isAssignableFrom(km.getClass) ||
        km.getClass.isAssignableFrom(this.getClass) => this.primaryKeyField == km.primaryKeyField
      case k => super.equals(k)
    }
  }
}

/**
* If this trait is mixed into a validation function, the validation for a field
* will stop if this validation function returns an error
*/
trait StopValidationOnError[T] extends Function1[T, List[FieldError]]

object StopValidationOnError {
  def apply[T](f: T => List[FieldError]): StopValidationOnError[T] =
  new StopValidationOnError[T] {
    def apply(in: T): List[FieldError] = f(in)
  }

  def apply[T](f: PartialFunction[T, List[FieldError]]): PartialFunction[T, List[FieldError]] with StopValidationOnError[T] =
  new PartialFunction[T, List[FieldError]] with StopValidationOnError[T] {
    def apply(in: T): List[FieldError] = f(in)
    def isDefinedAt(in: T): Boolean = f.isDefinedAt(in)
  }
}

}
}
