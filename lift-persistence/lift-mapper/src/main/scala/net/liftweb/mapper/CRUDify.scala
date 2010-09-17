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

import sitemap._
import Loc._
import http._
import util._
import common._
import Helpers._

import _root_.scala.xml._

/**
 * This trait automatically adds CRUD (Create, read, update and delete) operations
 * to an existing <b>MetaMapper</b> object. Various methods can be overridden to
 * customize which operations are available to a user and how things are displayed.
 * For example, you can disable deletion of entities by overriding deleteMenuLoc to Empty.
 *
 * Note: Compilation will fail if you try to mix this into a Mapper instead of the
 * associated MetaMapper. You have been warned.
 */
trait CRUDify[KeyType, CrudType <: KeyedMapper[KeyType, CrudType]] {
  self: CrudType with KeyedMetaMapper[KeyType, CrudType] =>

  lazy val Prefix = calcPrefix
  lazy val ListItems = calcListItems
  lazy val ViewItem = calcViewItem
  lazy val CreateItem = calcCreateItem
  lazy val EditItem = calcEditItem
  lazy val DeleteItem = calcDeleteItem

  def calcPrefix = List(_dbTableNameLC)

  def calcListItems = "list"

  def calcViewItem = "view"

  def calcCreateItem = "create"

  def calcEditItem = "edit"

  def calcDeleteItem = "delete"

  def displayName = displayHtml.text

  def displayHtml: NodeSeq = Text(calcPrefix.head)

  /**
  * The fields displayed on the list page.  By default all the displayed fields, but this list
  * can be shortened.
  */
  def fieldsForList: List[MappedField[_, CrudType]] = mappedFieldsForModel.filter(_.dbDisplay_?)

  def pageWrapper(body: NodeSeq): NodeSeq =
  <lift:surround with="default" at="content">
    {
      body
    }
  </lift:surround>

  /**
   * The menu item for listing items (make this "Empty" to disable)
   */
  def showAllMenuLoc: Box[Menu] =
  Full(Menu(Loc("List "+Prefix, listPath, showAllMenuName,
                locSnippets :: Loc.Template(showAllTemplate) :: showAllMenuLocParams)))

  /**
   * Override to include new Params for the show all menu
   */
  def showAllMenuLocParams: List[Loc.AnyLocParam] = Nil

  /**
   * The menu item for creating items (make this "Empty" to disable)
   */
  def createMenuLoc: Box[Menu] =
  Full(Menu(Loc("Create "+Prefix, createPath, createMenuName,
                locSnippets :: Loc.Template(createTemplate) :: createMenuLocParams)))
  /**
   * Override to include new Params for the create menu
   */
  def createMenuLocParams: List[Loc.AnyLocParam] = Nil


  /**
   * The menu item for viewing an item (make this "Empty" to disable)
   */
  def viewMenuLoc: Box[Menu] =
  Full(Menu(new Loc[CrudType]{
        // the name of the page
        def name = "View "+Prefix

        override val snippets: SnippetTest = {
          case ("crud.view", Full(wp: CrudType)) => displayRecord(wp) _
        }

        def defaultValue = Empty

        def params = viewMenuLocParams

        /**
         * What's the text of the link?
         */
        val text = new Loc.LinkText(calcLinkText _)

        def calcLinkText(in: CrudType): NodeSeq = Text(S.??("crudify.menu.view.displayName", displayName))

        /**
         * Rewrite the request and emit the type-safe parameter
         */
        override val rewrite: LocRewrite =
        Full(NamedPF(name) {
            case RewriteRequest(pp , _, _)
              if pp.wholePath.startsWith(viewPath) &&
              pp.wholePath.length == (viewPath.length + 1) &&
              findForParam(pp.wholePath.last).isDefined
              =>
              (RewriteResponse(viewPath),findForParam(pp.wholePath.last).open_!)
          })

        def displayRecord(entry: CrudType)(in: NodeSeq): NodeSeq = {
          def doRow(in: NodeSeq): NodeSeq =
          entry.formFields.flatMap(
            f => bind("crud", in, "name" -> f.displayHtml, "value" -> f.asHtml)
          )

          bind("crud", in, "row" -> doRow _)
        }

        override def calcTemplate = Full(viewTemplate)

        val link =
        new Loc.Link[CrudType](viewPath, false) {
          override def createLink(in: CrudType) =
          Full(Text(viewPathString+"/"+obscurePrimaryKey(in)))
        }
      }))
  /**
   * Override to include new Params for the view menu
   */
  def viewMenuLocParams: List[Loc.LocParam[CrudType]] = Nil


  /**
   * The menu item for editing an item (make this "Empty" to disable)
   */
  def editMenuLoc: Box[Menu] = {
    Full(Menu(new Loc[CrudType]{
          // the name of the page
          def name = "Edit "+Prefix

          override val snippets: SnippetTest = {
            case ("crud.edit", Full(wp: CrudType)) => crudDoForm(wp, S.??("Save"))
          }

          def defaultValue = Empty

          def params = editMenuLocParams

          /**
           * What's the text of the link?
           */
          val text = new Loc.LinkText(calcLinkText _)

          def calcLinkText(in: CrudType): NodeSeq = Text(S.??("crudify.menu.edit.displayName", displayName))

          /**
           * Rewrite the request and emit the type-safe parameter
           */
          override val rewrite: LocRewrite =
          Full(NamedPF(name) {
              case RewriteRequest(pp , _, _)
                if pp.wholePath.startsWith(editPath) &&
                pp.wholePath.length == (editPath.length + 1) &&
                findForParam(pp.wholePath.last).isDefined
                =>
                (RewriteResponse(editPath),findForParam(pp.wholePath.last).open_!)
            })

          override def calcTemplate = Full(editTemplate)

          val link =
          new Loc.Link[CrudType](editPath, false) {
            override def createLink(in: CrudType) =
            Full(Text(editPathString+"/"+obscurePrimaryKey(in)))
          }
        }))
  }

  /**
   * Override to include new Params for the edit menu
   */
  def editMenuLocParams: List[Loc.LocParam[CrudType]] = Nil


  def editMenuName = S.??("Edit")+" "+displayName

  def editTemplate(): NodeSeq = pageWrapper(_editTemplate)

  def editId = "edit_page"
  def editClass = "edit_class"
  def editErrorClass = "edit_error_class"

  def _editTemplate =
  <lift:crud.edit form="post">
    <table id={editId} class={editClass}>
      <crud:field>
        <tr>
          <td>
            <crud:name/>
          </td>
          <td>
            <crud:form/>
          </td>
        </tr>
      </crud:field>

      <tr>
        <td>&nbsp;</td>
        <td><crud:submit>{editButton}</crud:submit></td>
      </tr>
    </table>
  </lift:crud.edit>

  def editButton = S.??("Save")

  /**
   * The menu item for deleting an item (make this "Empty" to disable)
   */
  def deleteMenuLoc: Box[Menu] = {
    Full(Menu(new Loc[CrudType]{
          // the name of the page
          def name = "Delete "+Prefix

          override val snippets: SnippetTest = {
            case ("crud.delete", Full(wp: CrudType)) => crudyDelete(wp)
          }

          def crudyDelete(item: CrudType)(html: NodeSeq): NodeSeq = {
            val from = referer

            def doFields(html: NodeSeq): NodeSeq =
            item.formFields.flatMap(f =>
              bind("crud", html, "name" -> f.displayHtml, "value" -> f.asHtml))

            def doSubmit() = {
              S.notice("Deleted")
              item.delete_!
              S.redirectTo(from)
            }

            bind("crud", html,
                 "field" -> doFields _,
                 "submit" ->
                 ((text: NodeSeq) => SHtml.submit(text.text, doSubmit _)))
          }

          def defaultValue = Empty

          def params = deleteMenuLocParams

          /**
           * What's the text of the link?
           */
          val text = new Loc.LinkText(calcLinkText _)

          def calcLinkText(in: CrudType): NodeSeq = Text(S.??("crudify.menu.delete.displayName", displayName))

          /**
           * Rewrite the request and emit the type-safe parameter
           */
          override val rewrite: LocRewrite =
          Full(NamedPF(name) {
              case RewriteRequest(pp , _, _)
                if pp.wholePath.startsWith(deletePath) &&
                pp.wholePath.length == (deletePath.length + 1) &&
                findForParam(pp.wholePath.last).isDefined
                =>
                (RewriteResponse(deletePath),findForParam(pp.wholePath.last).open_!)
            })

          override def calcTemplate = Full(deleteTemplate)

          val link =
          new Loc.Link[CrudType](deletePath, false) {
            override def createLink(in: CrudType) =
            Full(Text(deletePathString+"/"+obscurePrimaryKey(in)))
          }
        }))
  }

  /**
   * Override to include new Params for the delete menu
   */
  def deleteMenuLocParams: List[Loc.LocParam[CrudType]] = Nil


  def deleteMenuName = S.??("Delete")+" "+displayName

  def deleteTemplate(): NodeSeq = pageWrapper(_deleteTemplate)

  def deleteId = "delete_page"
  def deleteClass = "delete_class"

  def _deleteTemplate =
  <lift:crud.delete form="post">
    <table id={deleteId} class={deleteClass}>
      <crud:field>
        <tr>
          <td>
            <crud:name/>
          </td>
          <td>
            <crud:value/>
          </td>
        </tr>
      </crud:field>

      <tr>
        <td>&nbsp;</td>
        <td><crud:submit>{deleteButton}</crud:submit></td>
      </tr>
    </table>
  </lift:crud.delete>

  def deleteButton = S.??("Delete")


  def createMenuName = S.??("Create")+" "+displayName

  def createTemplate(): NodeSeq = pageWrapper(_createTemplate)

  def createId = "create_page"
  def createClass = "create_class"

  def _createTemplate =
  <lift:crud.create form="post">
    <table id={createId} class={createClass}>
      <crud:field>
        <tr>
          <td>
            <crud:name/>
          </td>
          <td>
            <crud:form/>
          </td>
        </tr>
      </crud:field>

      <tr>
        <td>&nbsp;</td>
        <td><crud:submit>{createButton}</crud:submit></td>
      </tr>
    </table>
  </lift:crud.create>

  def createButton = S.??("Create")

  def viewMenuName = S.??("View")+" "+displayName

  def viewTemplate(): NodeSeq = pageWrapper(_viewTemplate)

  def viewId = "view_page"
  def viewClass = "view_class"

  def _viewTemplate =
  <lift:crud.view>
    <table id={viewId} class={viewClass}>
      <crud:row>
        <tr>
          <td><crud:name/></td>
          <td><crud:value/></td>
        </tr>
      </crud:row>
    </table>
  </lift:crud.view>

  def showAllMenuName = S.??("List")+" "+displayName

  def showAllTemplate(): NodeSeq = pageWrapper(_showAllTemplate)

  def showAllId = "show_all"
  def showAllClass = "show_all"

  def _showAllTemplate =
  <lift:crud.all>
    <table id={showAllId} class={showAllClass}>
      <thead>
        <tr>
          <crud:header_item><th><crud:name/></th></crud:header_item>
          <th>&nbsp;</th>
          <th>&nbsp;</th>
          <th>&nbsp;</th>
        </tr>
      </thead>
      <tbody>
        <crud:row>
          <tr>
            <crud:row_item><td><crud:value/></td></crud:row_item>
            <td><a crud:view_href="">{S.??("View")}</a></td>
            <td><a crud:edit_href="">{S.??("Edit")}</a></td>
            <td><a crud:delete_href="">{S.??("Delete")}</a></td>
          </tr>
        </crud:row>
      </tbody>
      <tfoot>
        <tr>
          <td colspan="3"><crud:prev>{previousWord}</crud:prev></td>
          <td colspan="3"><crud:next>{nextWord}</crud:next></td>
        </tr>
      </tfoot>
    </table>
  </lift:crud.all>

  def nextWord = S.??("Next")
  def previousWord = S.??("Previous")

  lazy val listPath = Prefix ::: List(ListItems)

  lazy val listPathString: String = mp(listPath)

  lazy val createPath = Prefix ::: List(CreateItem)

  lazy val createPathString: String = mp(createPath)

  lazy val viewPath = Prefix ::: List(ViewItem)

  lazy val viewPathString: String = mp(viewPath)

  lazy val editPath = Prefix ::: List(EditItem)

  lazy val editPathString: String = mp(editPath)

  lazy val deletePath = Prefix ::: List(DeleteItem)

  lazy val deletePathString: String = mp(deletePath)

  private def mp(in: List[String]) = in.mkString("/", "/", "")

  def menus: List[Menu] =
  List(showAllMenuLoc, createMenuLoc, viewMenuLoc,
       editMenuLoc, deleteMenuLoc).flatMap(x => x)

  def findForList(start: Long, count: Int): List[CrudType] =
  findAll(StartAt[CrudType](start) :: MaxRows[CrudType](count) ::
          findForListParams :_*)

  def findForListParams: List[QueryParam[CrudType]] =
  List(OrderBy(primaryKeyField, Ascending))

  def findForParam(in: String): Box[CrudType] = find(in)

  lazy val locSnippets = new DispatchLocSnippets {
    val dispatch: PartialFunction[String, NodeSeq => NodeSeq] = {
      case "crud.all" => doCrudAll
      case "crud.create" => crudDoForm(create, S.??("Created"))
    }

    def doCrudAll(in: NodeSeq): NodeSeq = {
      val first = S.param("first").map(toLong) openOr 0L
      val list = findForList(first, 20)

      def prev(in: NodeSeq) = if (first < 20) <xml:group>&nbsp;</xml:group>
      else <a href={listPathString+"?first="+(0L max (first - 20L))}>{in}</a>

      def next(in: NodeSeq) = if (list.length < 20) <xml:group>&nbsp;</xml:group>
      else <a href={listPathString+"?first="+(first + 20L)}>{in}</a>


      def doHeaderItems(in: NodeSeq): NodeSeq = fieldsForList.flatMap(f => 
        bind("crud", in, "name" -> f.displayHtml))

      def doRows(in: NodeSeq): NodeSeq =
      list.take(20).flatMap{
        c =>
        def doRowItem(in: NodeSeq): NodeSeq = fieldsForList.map(ffl => getActualField(c, ffl)).flatMap(
          f => bind("crud", in, "value" -> f.asHtml))

        bind("crud", in , "row_item" -> doRowItem _,
             FuncAttrBindParam("edit_href", { ignore : NodeSeq =>
               Text(editPathString+"/"+(obscurePrimaryKey(c))) },"href"),

             FuncAttrBindParam("view_href", { ignore : NodeSeq =>
            Text(viewPathString+"/"+
                 (obscurePrimaryKey(c)))},"href"),

             FuncAttrBindParam("delete_href", { ignore : NodeSeq =>
            Text(deletePathString+"/"+
                 (obscurePrimaryKey(c)))},"href")
        )}

      bind("crud", in, "header_item" -> doHeaderItems _,
           "row" -> doRows _,
           "prev" -> prev _, "next" -> next _)

    }
  }

  def obscurePrimaryKey(in: CrudType): String = obscurePrimaryKey(in.primaryKeyField.toString)
  def obscurePrimaryKey(in: String): String = in

  def referer: String = S.referer openOr listPathString


  def crudDoForm(item: CrudType, noticeMsg: String)(in: NodeSeq): NodeSeq = {
    val from = referer
    val snipName = S.currentSnippet

    def loop(html:NodeSeq): NodeSeq = {
      def doFields(html: NodeSeq): NodeSeq =
      item.flatMapFieldTitleForm2((title, field, form) => {
	def error: NodeSeq = field.uniqueFieldId match {
	  case fid @ Full(id) => S.getNotices.filter(_._3 == fid).flatMap(err =>
	    List(Text(" "), <span class={editErrorClass}>{err._2}</span>) )

	  case _ => NodeSeq.Empty
	}
	  
	
        bind("crud", html, "name" -> (title ++ error), "form" -> form)
      })

      def doSubmit() = item.validate match {
        case Nil =>
          S.notice(noticeMsg)
          item.save
          S.redirectTo(from)

        case xs =>
          S.error(xs)
          snipName.foreach(S.mapSnippet(_, loop))
      }

      bind("crud", html,
           "field" -> doFields _,
           "submit" ->
           ((text: NodeSeq) => SHtml.submit(text.text, doSubmit _)))
    }

    loop(in)
  }


}


/**
 * A specialization of CRUDify for LongKeyedMetaMappers.
 */
trait LongCRUDify[CrudType <: KeyedMapper[Long, CrudType]] extends CRUDify[Long, CrudType] {
  self: CrudType with KeyedMetaMapper[Long, CrudType] =>
}

}
}
