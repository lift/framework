package net.liftweb

import net.liftweb.common.{Box, Empty, Full}
import net.liftweb.fixtures.RequestContext._
import net.liftweb.fixtures._
import net.liftweb.http.{Req, ResponseShortcutException, S}
import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification
import org.specs2.specification.Scope

import scala.collection.immutable
import scala.xml.{NodeSeq, Text}

class CrudifySpec extends Specification with XmlMatchers {
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

    def deleteItem(item: SpecCrudType = firstItem): NodeSeq = withSession(Req.nil) {
      crudyDelete(item)(deleteTemplate())
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

    "render only next link on first page" in new SpecCrudifyWithContext {
      withRequest(Req.nil) {
        val html = doCrudAll(this.showAllTemplate())
        val nextLinkContainer = (html \\ "td").find(td => (td \ "@class").text == "next")
        val prevLinkContainer = (html \\ "td").find(td => (td \ "@class").text == "previous")
        nextLinkContainer must beSome[NodeSeq]
        val nextLink = nextLinkContainer.get
        nextLink must \\("a", "href")
        nextLink must \\("a").textIs(nextWord)
        prevLinkContainer must beNone
      }
    }

    "render both naviagation links inbeetwen" in new SpecCrudifyWithContext {
      withRequest(params(Map("first" -> s"$rowsPerPage"))) {
        val html = doCrudAll(this.showAllTemplate())
        val nextLinkContainer = (html \\ "td").find(td => (td \ "@class").text == "next")
        val prevLinkContainer = (html \\ "td").find(td => (td \ "@class").text == "previous")
        prevLinkContainer must beSome[NodeSeq]
        nextLinkContainer must beSome[NodeSeq]

        val prevLink = prevLinkContainer.get
        prevLink must \\("a", "href")
        prevLink must \\("a").textIs(previousWord)
        val nextLink = nextLinkContainer.get
        nextLink must \\("a", "href")
        nextLink must \\("a").textIs(nextWord)
      }
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

  trait FormHelpers {
    this: SpecCrudifyWithContext =>
    def buildEditForm(): NodeSeq = {
      crudDoForm(firstItem, "Edit Notice")(editTemplate())
    }

    def setId(form: NodeSeq, newId: String): Unit = {
      val setIdFunc: String = ((form \\ "input").find(i => (i \\ "@id").text == "id").head \\ "@name").text
      S.functionMap(setIdFunc).asInstanceOf[Any => Any].apply(List(newId))
    }

    def setValue(form: NodeSeq, newValue: String): Unit = {
      val setValueFunc: String = ((form \\ "input").find(i => (i \\ "@id").text == "value").head \\ "@name").text
      S.functionMap(setValueFunc).asInstanceOf[Any => Any].apply(List(newValue))
    }

    def submitForm(form: NodeSeq, expectRedirect: Boolean = true): Unit = {
      val submitFunc: String = ((form \\ "button").find(i => (i \\ "@type").text == "submit").head \\ "@name").text
      val lazySubmit = () => S.functionMap(submitFunc).asInstanceOf[Any => Any].apply(List(""))
      if (expectRedirect) {
        lazySubmit() must throwA[ResponseShortcutException]
      } else {
        lazySubmit()
      }
    }
  }

  "crudDoForm on `editTemplate`" should {

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

    "render error message for each filed" in new SpecCrudifyWithContext with FormHelpers {
      fieldsForDisplay.map { fp =>
        withSession(Req.nil) {
          S.error(fp.fieldName, s"Dummy error for ${fp.fieldName}")
          val form = buildEditForm()
          val filedRow = (form \\ "tr").filter(tr => {
            (tr \\ "td" \\ "input" \\ "@id").text == fp.fieldName
          })
          filedRow must \\("span").textIs(s"Dummy error for ${fp.fieldName}")
        }
      }
    }

    "produce notice on update" in new SpecCrudifyWithContext with FormHelpers {
      withSession(Req.nil) {
        val form = buildEditForm()
        submitForm(form)

        val notices: immutable.Seq[(NodeSeq, Box[String])] = S.notices
        notices.map(_._1).map(_.text) must contain(exactly("Edit Notice"))
      }
    }

    "validate input" in new SpecCrudifyWithContext with FormHelpers {
      withSession(Req.nil) {
        val form = buildEditForm()
        setId(form, "INVALID")
        submitForm(form, expectRedirect = false)
        S.errors == List((Text("Id filed must be numeric"), Full("id")))
      }
    }

    "allow to save modified content" in new SpecCrudifyWithContext with FormHelpers {
      withSession(Req.nil) {
        val form = buildEditForm()
        val oldId = firstItem.id
        val newId = "300"
        val newValue = "UPDATED LINE 300"

        setId(form, newId)
        setValue(form, newValue)
        submitForm(form)

        repo.find(oldId) === Empty
        val updated = repo.find(newId)

        updated.isDefined must beTrue
        val item = updated.openOrThrowException("Guarded before")
        item.id === newId
        item.value === newValue
      }
    }
  }

  "crudyDelete on `deleteTemplate`" should {

    "render row for each field" in new SpecCrudifyWithContext {
      val trElements = (deleteItem() \\ "table" \\ "tr")
        .filter(tr => (tr \ "@class").text == "field")

      trElements must haveSize(fieldsForDisplay.length)
    }

    "render label for each field" in new SpecCrudifyWithContext {
      val labels = (deleteItem() \\ "table" \\ "tr" \\ "td" \\ "label")
        .map(_.text)

      labels must contain(exactly(fieldsForDisplay.map(_.fieldName): _*))
    }

    "render values for each field" in new SpecCrudifyWithContext {
      val values = (deleteItem() \\ "table" \\ "tr" \\ "td")
        .filter(td => (td \\ "@class").text == "value")
        .map(_.text)

      values must contain(exactly(firstItem.id, firstItem.value))
    }

    "render delete button" in new SpecCrudifyWithContext {
      withSession(Req.nil) {
        val form = crudyDelete(firstItem)(deleteTemplate())
        val button = form
        button must haveSize(1)
        button must \\("button", "type" -> "submit")
        button must \\("button").textIs(deleteButton)
      }
    }

    "produce notice on delete" in new SpecCrudifyWithContext with FormHelpers {
      withSession(Req.nil) {
        val form = crudyDelete(firstItem)(deleteTemplate())
        submitForm(form)

        val notices: immutable.Seq[(NodeSeq, Box[String])] = S.notices
        notices.map(_._1).map(_.text) must contain(exactly(S ? "Deleted"))
      }
    }

    "remove item from repo on submit" in new SpecCrudifyWithContext with FormHelpers {
      withSession(Req.nil) {
        val form = crudyDelete(firstItem)(deleteTemplate())
        submitForm(form)

        repo.find(firstItem.id) === Empty
      }
    }

  }
}
