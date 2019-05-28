package net.liftweb

import net.liftweb.common.{Box, Full}
import net.liftweb.fixtures.RequestContext._
import net.liftweb.fixtures._
import net.liftweb.proto.Crudify
import net.liftweb.util.{BaseField, FieldError, LiftValue}
import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification
import org.specs2.specification.Scope

import scala.xml.{NodeSeq, Text}

object CrudifySpec extends Specification with XmlMatchers {
  "Crudify Trait Specification".title

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

        override def toForm: Box[NodeSeq] = Full(Text(get))

        override def name: String = pointer.fieldName

        override def set(in: String): String = {
          pointer.setter(instance, in)
          in
        }

        override def get: String = pointer.getter(instance)
      }
      Full(result)
    }
  }

  "Crudify `showAllTemplate`" should {

    class SpecCrudifyWithContext extends SpecCrudify with Scope {
      val repo: SpecCrudRepo = SpecCrudType.defaultRepo

      def all: NodeSeq = withRequest(get()) {
        doCrudAll(this.showAllTemplate())
      }

    }

    "render proper rows count" in new SpecCrudifyWithContext {
      all \\ "tbody" \\ "tr" must have size rowsPerPage
    }

    "honor rowsPerPage settings" in new SpecCrudifyWithContext {
      override def rowsPerPage = 1

      all \\ "tbody" \\ "tr" must have size 1
    }

    "use `first` params for pagination" in new SpecCrudifyWithContext {
      withRequest(get(Map("first" -> "10"))) {
        val result = doCrudAll(this.showAllTemplate())
        val rowData = (result \\ "tbody" \\ "tr" \\ "td").take(fieldsForDisplay.size).map(_.text)
        val repoData = repo.content(10, 1).flatMap(i => List(i.id, i.value))
        rowData === repoData
      }
    }

    "render proper headers content" in new SpecCrudifyWithContext {
      val th: NodeSeq = all \\ "thead" \\ "th"
      val renderedHeaders = th.map(_.text).filterNot(_ == "&nbsp;")
      renderedHeaders must contain(exactly(SpecCrudType.FIELDS.map(_.fieldName): _*))
    }

    "render proper columns content" in new SpecCrudifyWithContext {
      val tr: NodeSeq = all \\ "tbody" \\ "tr"
      val renderedValues: List[List[String]] = tr.map(row => {
        (row \ "td")
          .filter(td => (td \ "@class").nonEmpty)
          .map(_.text).toList
      }).toList
      val expectedValues: List[List[String]] = repo.content(0, rowsPerPage).map(i => List(i.id, i.value))
      renderedValues === expectedValues
    }
  }
}
