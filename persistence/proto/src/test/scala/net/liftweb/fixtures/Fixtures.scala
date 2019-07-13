package net.liftweb
package fixtures

import net.liftweb.common.{Box, Empty, Full}
import net.liftweb.http.S.SFuncHolder
import net.liftweb.http.{LiftSession, Req, RewriteRequest, RewriteResponse, S}
import net.liftweb.proto.Crudify
import net.liftweb.util.{BaseField, FieldError, FieldIdentifier, LiftValue}

import scala.collection.mutable
import scala.xml._

/**
  * Helper type represents content for [[net.liftweb.proto.Crudify]]
  *
  * @param id    fake data `id` field
  * @param value fake data `value` field
  */
case class SpecCrudType(var id: String, var value: String) {
  val _id = id
  val _value = value
}

/** Helper object for [[net.liftweb.proto.Crudify]] trait testing */
object SpecCrudType {
  /** [[net.liftweb.proto.Crudify.FieldPointerType]] */
  type FieldType = SpecField[SpecCrudType]

  /** Default initial [[SpecCrudRepo]] content */
  def DEFAULT_REPO_CONTENT = (0 until 100).map(n => n.toString -> new SpecCrudType(n.toString, s"Line number $n"))

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
case class SpecField[T](fieldName: String, getter: T => String, setter: (T, String) => Unit) extends FieldIdentifier {

  /**
    * Field name as HTML
    *
    * @return Element with represents field name in HTML
    */
  def displayHtml: NodeSeq = Text(fieldName)

  override def uniqueFieldId: Box[String] = Full(fieldName)
}


/**
  * Helper class for creating fake data repository for using as [[net.liftweb.proto.Crudify.CrudBridge]] and for others
  * methods needed by [[net.liftweb.proto.Crudify]] implementation
  *
  * @param initialContent initial content for repor
  */
class SpecCrudRepo(initialContent: (String, SpecCrudType)*) {
  private val dict: mutable.Map[String, SpecCrudType] = mutable.LinkedHashMap(initialContent: _*)

  /** Return items count in repo */
  def size: Int = dict.size

  /**
    * Return repo content part restricted by `start` and `count` parameters
    *
    * @param start first returned item index
    * @param count maximal returned items count
    * @return Repo items starting form `start` and truncated to `count` size
    */
  def content(start: Long, count: Int): List[SpecCrudType] = {
    val startIndex = start.toInt
    dict.values.slice(startIndex, startIndex + count).toList
  }

  /** Find content in repo by [[String]] `id` param */
  def find(id: String): Box[SpecCrudType] = {
    dict.get(id)
  }

  /** Delete content from repo */
  def delete_!(target: SpecCrudType): Boolean = {
    dict.remove(target.id).isDefined
  }

  /** Save new instance to repo or replace previous value inside repo if present */
  def save(target: SpecCrudType): Boolean = {
    dict.remove(target._id) //remove previous id if present
    val newValue = SpecCrudType(target.id, target.value)
    dict += newValue.id -> newValue
    true
  }

  /** Validate instance */
  def validate(target: SpecCrudType): List[FieldError] = {
    val numbersOnly = "(\\d+)".r
    target.id match {
      case numbersOnly(_) =>
        Nil
      case _ =>
        List(
          FieldError(SpecCrudType.FIELDS.head, "Id filed must be numeric")
        )
    }
  }


  /** Return [[String]] representation of instance primary field */
  def primaryKeyFieldAsString(target: SpecCrudType): String = target.id
}


/** Spec class implementation of [[net.liftweb.proto.Crudify]] trait */
trait SpecCrudify extends Crudify {

  def repo: SpecCrudRepo

  override type TheCrudType = SpecCrudType

  override type FieldPointerType = SpecCrudType.FieldType


  override def calcPrefix: List[String] = List("Prefix")

  override def create: SpecCrudType = new SpecCrudType("", "")

  override def fieldsForDisplay: List[FieldPointerType] = SpecCrudType.FIELDS

  override def findForList(start: Long, count: Int): List[TheCrudType] = repo.content(start, count)

  override def findForParam(in: String): Box[TheCrudType] = repo.find(in)


  override protected implicit def buildBridge(from: TheCrudType): CrudBridge = new CrudBridge {

    override def delete_! : Boolean = repo.delete_!(from)


    override def save: Boolean = repo.save(from)

    override def validate: List[FieldError] = repo.validate(from)


    override def primaryKeyFieldAsString: String = repo.primaryKeyFieldAsString(from)
  }

  override protected implicit def buildFieldBridge(from: FieldPointerType): FieldPointerBridge = new FieldPointerBridge {
    override def displayHtml: NodeSeq = from.displayHtml
  }

  override protected def computeFieldFromPointer(instance: TheCrudType, pointer: FieldPointerType): Box[BaseField] = {
    val result: BaseField = new BaseField with LiftValue[String] {
      override def setFilter: List[String => String] = Nil

      override def validations: List[String => List[FieldError]] = Nil

      override def validate: List[FieldError] = Nil

      override def toForm: Box[NodeSeq] = {
        Full(
          S.fmapFunc(SFuncHolder(set)) {
            funcName =>
                <input type="text" maxlength="70" name={funcName} value={get}/>
          } % idAttribute()
        )
      }

      override def displayNameHtml: Box[NodeSeq] = {
        Full(<label>{displayName}</label> % idAttribute("for"))
      }

      override def name: String = pointer.fieldName

      override def set(in: String): String = {
        pointer.setter(instance, in)
        in
      }

      override def get: String = pointer.getter(instance)

      override def fieldId: Option[NodeSeq] = Some(Text(displayName))

      override def uniqueFieldId: Box[String] = Some(pointer.fieldName)

      def idAttribute(name: String = "id"): MetaData = fieldId match {
        case Some(nodeSeq) => Attribute.apply(name, nodeSeq, Null)
        case _ => Null
      }
    }
    Full(result)
  }
}


/** Helper object for calling method inside context of `Lift` request */
object RequestContext {
  val testSession = new LiftSession("/context-path", "underlying id", Empty)

  /**
    * Produce HTTP request field with params to build Lift` context
    *
    * @param params HTTP request params
    * @return Test HTTP request filled with params`
    */
  def params(params: Map[String, String]): Req = {
    Req(Req.nil, List({
      case r: RewriteRequest =>
        RewriteResponse(r.path, params, stopRewriting = true)
    }))
  }

  /**
    * Call `function` inside `Lift` context produced from `request`
    * Make functions like `S ?` or `S.param` works during call of `function`
    *
    * @param request  HTTP request filled with needed params
    * @param function target function to execute in context of given `Lift` request
    * @tparam T `function` return type
    * @return result of `function` execution
    */
  def withRequest[T](request: Req)(function: => T): T = {
    S.statelessInit(request)({
      function
    })
  }

  /**
    * Call `function` inside session based `Lift` context  produced from `request`
    * Make functions like `S.fmapFunc` or `SHtml.onSubmitUnit` works during call of `function`
    *
    * @param request  HTTP request filled with needed params
    * @param function target function to execute in context of given `Lift` request
    * @tparam T `function` return type
    * @return result of `function` execution
    */
  def withSession[T](request: Req)(function: => T): T = {
    S.init(Some(request), testSession)({
      function
    })
  }
}



