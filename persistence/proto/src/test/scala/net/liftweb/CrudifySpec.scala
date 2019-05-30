package net.liftweb

import net.liftweb.fixtures.RequestContext._
import net.liftweb.fixtures._
import net.liftweb.http.Req
import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification
import org.specs2.specification.Scope

import scala.xml.NodeSeq

object CrudifySpec extends Specification with XmlMatchers {
  "Crudify Trait Specification".title

  class SpecCrudifyWithContext extends SpecCrudify with Scope {
    val repo: SpecCrudRepo = SpecCrudType.defaultRepo
    val firstItem: SpecCrudType = repo.content(0, 1).head

    def all: NodeSeq = withRequest(Req.nil) {
      doCrudAll(this.showAllTemplate())
    }

    def viewItem(item: SpecCrudType = firstItem): NodeSeq = withRequest(Req.nil) {
      displayRecord(item)(viewTemplate())
    }

    def editItem(item: SpecCrudType = firstItem): NodeSeq = withSession(Req.nil) {
      crudDoForm(item, "EditMsg")(editTemplate())
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
      withRequest(params(Map("first" -> "10"))) {
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

  "crudDoForm on `editTemplate`" in {

    "render row for each field" in new SpecCrudifyWithContext {
      val trElements = (editItem() \\ "table" \\ "tr")
        .filter(tr => (tr \ "@class").text == "field")

      trElements must haveSize(fieldsForDisplay.length)
    }

    "render label for each field" in new SpecCrudifyWithContext {
      val labels = (editItem() \\ "table" \\ "tr" \\ "td" \\ "label")
        .map(_.text)

      labels must contain(exactly(fieldsForDisplay.map(_.fieldName): _*))
    }

    "render inputs for each field" in new SpecCrudifyWithContext {
      val values = (editItem() \\ "table" \\ "tr" \\ "td" \\ "input")
        .map(i => (i \ "@value").text)

      values must contain(exactly(firstItem.id, firstItem.value))
    }

    "render save button" in new SpecCrudifyWithContext {
      val button = editItem() \\ "table" \\ "tr" \\ "td" \\ "button"
      button must haveSize(1)
      button must \\("button", "type" -> "submit")
    }
  }
}
