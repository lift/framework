package net.liftweb
package fixtures

import net.liftweb.common.Box
import net.liftweb.util.FieldError

import scala.collection.mutable
import scala.xml.{NodeSeq, Text}

/**
  * Helper type represents content for [[net.liftweb.proto.Crudify]]
  *
  * @param id    fake data `id` field
  * @param value fake data `value` field
  */
class SpecCrudType(var id: String, var value: String)

/** Helper object for [[net.liftweb.proto.Crudify]] trait testing */
object SpecCrudType {
  /** [[net.liftweb.proto.Crudify.FieldPointerType]] */
  type FieldType = SpecField[SpecCrudType]

  /** Default initial [[SpecCrudRepo]] content */
  val DEFAULT_REPO_CONTENT = List("1" -> new SpecCrudType("1", "first"),
    "2" -> new SpecCrudType("2", "second"),
    "3" -> new SpecCrudType("3", "third"))

  /** Default fields for [[SpecCrudType]] */
  val FIELDS: List[FieldType] = List(
    SpecField("id", _.id, (h, v) => h.id = v),
    SpecField("value", _.value, (h, v) => h.value = v))

  /** Build [[SpecCrudRepo]] with default content */
  def defaultRepo: SpecCrudRepo = new SpecCrudRepo(DEFAULT_REPO_CONTENT: _*)
}

/**
  * Helper type witch should be used as [[net.liftweb.proto.Crudify.FieldPointerType]]
  *
  * @param fieldName fake date field na,e
  * @param getter    return `fieldName` value as [[String]] from [[T]] instance
  * @param setter    convert given [[String]] and it as set `fieldName` value in [[T]] instance
  * @tparam T target fake data holder type ([[SpecCrudType]] for now)
  */
case class SpecField[T](fieldName: String, getter: T => String, setter: (T, String) => Unit) {

  /**
    * Field name as HTML
    *
    * @return Element with represents field name in HTML
    */
  def displayHtml: NodeSeq = Text(fieldName)
}


/**
  * Helper class for creating fake data repository for using as [[net.liftweb.proto.Crudify.CrudBridge]] and for others
  * methods needed by [[net.liftweb.proto.Crudify]] implementation
  *
  * @param initalContent initial content for repor
  */
class SpecCrudRepo(initalContent: (String, SpecCrudType)*) {
  private val dict: mutable.Map[String, SpecCrudType] = mutable.LinkedHashMap(initalContent: _*)

  /** Return items count in repo */
  def size: Int = dict.size

  /** Return repo content */
  //TODO: support for pagination
  def all: List[SpecCrudType] = dict.values.toList

  /** Find content in repo by [[String]] `id` param */
  def find(id: String): Box[SpecCrudType] = {
    dict.get(id)
  }

  /** Delete content from repo */
  def delete_!(target: SpecCrudType): Boolean = {
    dict.remove(target.id).isDefined
  }

  /** Save new instance to repo or replcace previous value inside repo if present */
  def save(target: SpecCrudType): Boolean = {
    dict += target.id -> target
    true
  }

  /** Validate instance */
  def validate(target: SpecCrudType): List[FieldError] = Nil


  /** Return [[String]] representation of instance primary field */
  def primaryKeyFieldAsString(target: SpecCrudType): String = target.id
}



