/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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
package record {

import java.lang.reflect.Modifier
import net.liftweb._
import util._
import common._
import scala.collection.mutable.{ListBuffer}
import scala.xml._
import net.liftweb.http.js.{JsExp, JE, JsObj}
import net.liftweb.http.{SHtml, Req, LiftResponse, LiftRules}
import _root_.java.lang.reflect.Method
import field._
import Box._
import JE._
import Helpers._

/**
 * Holds meta information and operations on a record
 */
trait MetaRecord[BaseRecord <: Record[BaseRecord]] {
  self: BaseRecord =>

  private var fieldList: List[FieldHolder] = Nil
  private var fieldMap: Map[String, FieldHolder] = Map.empty

  private var lifecycleCallbacks: List[(String, Method)] = Nil

  /**
   * Set this to use your own form template when rendering a Record to a form.
   *
   * This template is any given XHtml that contains three nodes acting as placeholders such as:
   *
   * <pre>
   *
   * &lt;lift:field_label name="firstName"/&gt; - the label for firstName field will be rendered here
   * &lt;lift:field name="firstName"/&gt; - the firstName field will be rendered here (typically an input field)
   * &lt;lift:field_msg name="firstName"/&gt; - the <lift:msg> will be rendered here hafing the id given by
   *                                             uniqueFieldId of the firstName field.
   *
   *
   * Example.
   *
   * Having:
   *
   * class MyRecord extends Record[MyRecord] {
   *
   * 	def meta = MyRecordMeta
   *
   * 	object firstName extends StringField(this, "John")
   *
   * }
   *
   * object MyRecordMeta extends MyRecord with MetaRecord[MyRecord] {
   *  override def mutable_? = false
   * }
   *
   * ...
   *
   * val rec = MyRecordMeta.createRecord.firstName("McLoud")
   *
   * val template =
   * &lt;div&gt;
   * 	&lt;div&gt;
   * 		&lt;div&gt;&lt;lift:field_label name="firstName"/&gt;&lt;/div&gt;
   * 		&lt;div&gt;&lt;lift:field name="firstName"/&gt;&lt;/div&gt;
   * 		&lt;div&gt;&lt;lift:field_msg name="firstName"/&gt;&lt;/div&gt;
   * 	&lt;/div&gt;
   * &lt;/div&gt;
   *
   * MyRecordMeta.formTemplate = Full(template)
   * rec.toForm((r:MyRecord) => println(r));
   *
   * </pre>
   *
   */
  var formTemplate: Box[NodeSeq] = Empty

  protected val rootClass = this.getClass.getSuperclass

  private def isLifecycle(m: Method) = classOf[LifecycleCallbacks].isAssignableFrom(m.getReturnType)
  private def isField(m: Method) = classOf[Field[_, _]].isAssignableFrom(m.getReturnType)

  def introspect(rec: BaseRecord, methods: Array[Method])(f: (Method, Field[_, BaseRecord]) => Any) = {
    for (v <- methods  if isField(v)) {
      v.invoke(rec) match {
        case mf: Field[_, BaseRecord] if !mf.ignoreField_? =>
          mf.setName_!(v.getName)
          f(v, mf)
        case _ =>
      }
    }

  }

  this.runSafe {
    val tArray = new ListBuffer[FieldHolder]

    val methods = rootClass.getMethods

    lifecycleCallbacks = (for (v <- methods
                              if v.getName != "meta" && isLifecycle(v)) yield (v.getName, v)).toList

    introspect(this, methods) {
      case (v, mf) => tArray += FieldHolder(mf.name, v, mf)
    }
    
    val posMap = Map() ++ tArray.map(_.metaField).toList.zipWithIndex

    val resArray = new ListBuffer[FieldHolder]

    fieldOrder.foreach(f => posMap.get(f).foreach(pos => resArray += tArray.remove(pos)))

    resArray ++= tArray

    fieldList = resArray.toList
    fieldMap = Map() ++ fieldList.map(i => (i.name, i))
  }

  /**
   * Specifies if this Record is mutable or not
   */
  def mutable_? = true

  /**
   * Creates a new record
   */
  def createRecord: BaseRecord = {
    val rec = instantiateRecord
    rec runSafe {
      fieldList.foreach(fh => fh.field(rec).setName_!(fh.name))
    }
    rec
  }

  /** Make a new record instance. This method can be overridden to provide caching behavior or what have you. */
  protected def instantiateRecord: BaseRecord = rootClass.newInstance.asInstanceOf[BaseRecord]

  /**
   * Creates a new record setting the value of the fields from the original object but
   * apply the new value for the specific field
   *
   * @param - original the initial record
   * @param - field the new mutated field
   * @param - the new value of the field
   */
  def createWithMutableField[FieldType](original: BaseRecord,
                                        field: Field[FieldType, BaseRecord],
                                        newValue: Box[FieldType]): BaseRecord = {
    val rec = createRecord

    for (fh <- fieldList) {
      val recField = fh.field(rec)
      if (fh.name == field.name)
        recField.asInstanceOf[Field[FieldType, BaseRecord]].setBox(newValue)
      else
        recField.setFromAny(fh.field(original).valueBox)
    }

    rec
  }

  /**
   * Returns the HTML representation of inst Record.
   *
   * @param inst - th designated Record
   * @return a NodeSeq
   */
  def toXHtml(inst: BaseRecord): NodeSeq = fieldList.flatMap(_.field(inst).toXHtml ++ Text("\n"))


  /**
   * Validates the inst Record by calling validators for each field
   *
   * @pram inst - the Record tobe validated
   * @return a List of FieldError. If this list is empty you can assume that record was validated successfully
   */
  def validate(inst: BaseRecord): List[FieldError] = {
    foreachCallback(inst, _.beforeValidation)
    try{
	    fieldList.flatMap(_.field(inst).validate)
    } finally {
      foreachCallback(inst, _.afterValidation)
    }
  }

  /**
   * Returns the JSON representation of <i>inst</i> record
   *
   * @param inst: BaseRecord
   * @return JsObj
   */
  def asJSON(inst: BaseRecord): JsObj = {
    val tups = fieldList.map{ fh =>
      val field = fh.field(inst)
      field.name -> field.asJs
    }
    JsObj(tups:_*)
  }

  /**
   * Create a record with fields populated with values from the JSON construct
   *
   * @param json - The stringified JSON object
   * @return Box[BaseRecord]
   */
  def fromJSON(json: String): Box[BaseRecord] = {
    val inst = createRecord
    setFieldsFromJSON(inst, json) map (_ => inst)
  }

  /**
   * Populate the fields of the record instance with values from the JSON construct
   *
   * @param inst - The record to populate
   * @param json - The stringified JSON object
   * @return - Full(()) on success, other on failure
   */
  def setFieldsFromJSON(inst: BaseRecord, json: String): Box[Unit] =
    JSONParser.parse(json) match {
      case Full(nvp : Map[_, _]) =>
        for ((k, v) <- nvp;
             field <- inst.fieldByName(k.toString)) yield field.setFromAny(v)
        Full(inst)
      case Full(_) => Empty
      case failure => failure.asA[Unit]
    }

  protected def foreachCallback(inst: BaseRecord, f: LifecycleCallbacks => Any) {
    inst match {
      case (lc: LifecycleCallbacks) => f(lc)
      case _ => {}
    }
    lifecycleCallbacks.foreach(m => f(m._2.invoke(inst).asInstanceOf[LifecycleCallbacks]))
  }

  /**
   * Returns the XHTML representation of inst Record. If formTemplate is set,
   * this template will be used otherwise a default template is considered.
   *
   * @param inst - the record to be rendered
   * @return the XHTML content as a NodeSeq
   */
  def toForm(inst: BaseRecord): NodeSeq = {
    formTemplate match {
      case Full(template) => toForm(inst, template)
      case _ => fieldList.flatMap(_.field(inst).toForm.openOr(NodeSeq.Empty) ++ Text("\n"))
    }
  }

  /**
   * Returns the XHTML representation of inst Record. You must provide the Node template
   * to represent this record in the proprietary layout.
   *
   * @param inst - the record to be rendered
   * @param template - The markup template forthe form. See also the formTemplate variable
   * @return the XHTML content as a NodeSeq
   */
  def toForm(inst: BaseRecord, template: NodeSeq): NodeSeq = {
    template match {
      case e @ <lift:field_label>{_*}</lift:field_label> => e.attribute("name") match {
          case Some(name) => fieldByName(name.toString, inst).map(_.label).openOr(NodeSeq.Empty)
          case _ => NodeSeq.Empty
        }

      case e @ <lift:field>{_*}</lift:field> => e.attribute("name") match {
          case Some(name) => fieldByName(name.toString, inst).flatMap(_.toForm).openOr(NodeSeq.Empty)
          case _ => NodeSeq.Empty
        }

      case e @ <lift:field_msg>{_*}</lift:field_msg> => e.attribute("name") match {
          case Some(name) => fieldByName(name.toString, inst).map(_.uniqueFieldId match {
                case Full(id) => <lift:msg id={id}/>
                case _ => NodeSeq.Empty
              }).openOr(NodeSeq.Empty)
          case _ => NodeSeq.Empty
        }

      case Elem(namespace, label, attrs, scp, ns @ _*) =>
        Elem(namespace, label, attrs, scp, toForm(inst, ns.flatMap(n => toForm(inst, n))):_* )

      case s : Seq[_] => s.flatMap(e => e match {
            case Elem(namespace, label, attrs, scp, ns @ _*) =>
              Elem(namespace, label, attrs, scp, toForm(inst, ns.flatMap(n => toForm(inst, n))):_* )
            case x => x
          })

    }
  }

  /**
   * Get a field by the field name
   * @param fieldName -- the name of the field to get
   * @param actual -- the instance to get the field on
   *
   * @return Box[The Field] (Empty if the field is not found)
   */
  def fieldByName(fieldName: String, inst: BaseRecord): Box[Field[_, BaseRecord]] = {
    Box(fieldMap.get(fieldName).map(_.field(inst)))
  }

  /**
   * Prepend a DispatchPF function to LiftRules.dispatch. If the partial function id defined for a give Req
   * it will construct a new Record based on the HTTP query string parameters
   * and will pass this Record to the function returned by func parameter.
   *
   * @param func - a PartialFunction for associating a request with a user provided function and the proper Record
   */
  def prependDispatch(func: PartialFunction[Req, BaseRecord => Box[LiftResponse]])= {
    LiftRules.dispatch.prepend (makeFunc(func))
  }

  /**
   * Append a DispatchPF function to LiftRules.dispatch. If the partial function id defined for a give Req
   * it will construct a new Record based on the HTTP query string parameters
   * and will pass this Record to the function returned by func parameter.
   *
   * @param func - a PartialFunction for associating a request with a user provided function and the proper Record
   */
  def appendDispatch(func: PartialFunction[Req, BaseRecord => Box[LiftResponse]])= {
    LiftRules.dispatch.append (makeFunc(func))
  }


  private def makeFunc(func: PartialFunction[Req, BaseRecord => Box[LiftResponse]]) = new PartialFunction[Req, () => Box[LiftResponse]] {

      def isDefinedAt(r: Req): Boolean = func.isDefinedAt(r)

      def apply(r: Req): () => Box[LiftResponse] = {
        val rec = fromReq(r)
        () => func(r)(rec)
      }
    }

  /**
   * Create a record with fields populated with values from the request
   *
   * @param req - The Req to read from
   * @return the created record
   */
  def fromReq(r: Req): BaseRecord = {
    val inst = createRecord
    setFieldsFromReq(inst, r)
    inst
  }

  /**
   * Populate the fields of the record with values from the request
   *
   * @param inst - The record to populate
   * @param req - The Req to read from
   */
  def setFieldsFromReq(inst: BaseRecord, req: Req) {
    for(fh <- fieldList){
      fh.field(inst).setFromAny(req.param(fh.name))
    }
  }

  /**
   * Defined the order of the fields in this record
   *
   * @return a List of Field
   */
  def fieldOrder: List[Field[_, BaseRecord]] = Nil

  /**
   * Renamed from fields() due to a clash with fields() in Record. Use this method
   * to obtain a list of fields defined in the meta companion objects. Possibly a
   * breaking change? (added 14th August 2009, Tim Perrett)
   *
   * @see Record
   */
  def metaFields() : List[Field[_, BaseRecord]] = fieldList.map(_.metaField)

  /**
   * Obtain the fields for a particlar Record or subclass instance by passing
   * the instance itself.
   * (added 14th August 2009, Tim Perrett)
   */
  def fields(rec: BaseRecord) : List[Field[_, BaseRecord]] = fieldList.map(_.field(rec))

  case class FieldHolder(name: String, method: Method, metaField: Field[_, BaseRecord]) {
    def field(inst: BaseRecord): Field[_, BaseRecord] = method.invoke(inst).asInstanceOf[Field[_, BaseRecord]]
  }
}

trait LifecycleCallbacks {
  def beforeValidation {}
  def afterValidation {}

  def beforeSave {}
  def beforeCreate {}
  def beforeUpdate {}

  def afterSave {}
  def afterCreate {}
  def afterUpdate {}

  def beforeDelete {}
  def afterDelete {}
}

}
}
