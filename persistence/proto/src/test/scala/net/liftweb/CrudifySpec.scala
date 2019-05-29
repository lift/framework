package net.liftweb

import net.liftweb.fixtures.RequestContext._
import net.liftweb.fixtures._
import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification
import org.specs2.specification.Scope

import scala.xml.NodeSeq

object CrudifySpec extends Specification with XmlMatchers {
  "Crudify Trait Specification".title

  class SpecCrudifyWithContext extends SpecCrudify with Scope {
    val repo: SpecCrudRepo = SpecCrudType.defaultRepo
    val firstItem: SpecCrudType = repo.content(0, 1).head

    def all: NodeSeq = withRequest(get()) {
      doCrudAll(this.showAllTemplate())
    }

    def viewItem(item: SpecCrudType = firstItem): NodeSeq = withRequest(get()) {
      displayRecord(item)(viewTemplate())
    }
  }

  "doCrudAll method `showAllTemplate`" should {

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
      renderedHeaders must contain(exactly(fieldsForDisplay.map(_.fieldName): _*))
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


  "displayRecord on `viewTemplate`" should {

    "render row for each field" in new SpecCrudifyWithContext {
      viewItem() \\ "table" \\ "tr" must have size fieldsForDisplay.size
    }

    "render correct field names" in new SpecCrudifyWithContext {
      val filedNames: Seq[String] = (viewItem() \\ "table" \\ "tr" \\ "td").
        filter(e => (e \ "@class").text == "name")
        .map(_.text)
      filedNames must contain(exactly(fieldsForDisplay.map(_.fieldName): _*))
    }

    "render correct field values" in new SpecCrudifyWithContext {
      val filedNames: Seq[String] = (viewItem() \\ "table" \\ "tr" \\ "td").
        filter(e => (e \ "@class").text == "value")
        .map(_.text)
      filedNames must contain(exactly(firstItem.id, firstItem.value))
    }
  }
}
