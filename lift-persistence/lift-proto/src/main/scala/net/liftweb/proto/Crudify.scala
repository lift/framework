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
package proto {

import sitemap._
import Loc._
import http._
import util._
import common._
import Helpers._

import _root_.scala.xml._

/**
 * This trait automatically adds CRUD (Create, read, update and delete) operations
 * to an existing persistence mechanism.
 * Various methods can be overridden to
 * customize which operations are available to a user and how things are displayed.
 * For example, you can disable deletion of entities by overriding deleteMenuLoc to Empty.
 *
 */
trait Crudify {
  /**
   * The type of records we're manipulating
   */
  type TheCrudType

  /**
   * A generic representation of a field.  For example, this represents the
   * abstract "name" field and is used along with an instance of TheCrudType
   * to compute the BaseField that is the "name" field on the specific instance
   * of TheCrudType
   */
  type FieldPointerType

  /**
   * This trait represents a Bridge between TheCrudType
   * and the Crudify trait.  It's not necessary to mix this
   * trait into TheCrudType, but instead provide a mechanism
   * for promoting a TheCrudType to CrudBridge
   */
  protected trait CrudBridge {
    /**
     * Delete the instance of TheCrudType from the backing store
     */
    def delete_! : Boolean

    /**
     * Save an instance of TheCrudType in backing store
     */
    def save : Boolean

    /**
     * Validate the fields in TheCrudType and return a List[FieldError]
     * representing the errors.
     */
    def validate: List[FieldError]

    /**
     * Return a string representation of the primary key field
     */
    def primaryKeyFieldAsString: String
  }

  /**
   * This method will instantiate a bridge from TheCrudType so
   * that the appropriate logical operations can be performed
   * on TheCrudType
   */
  protected implicit def buildBridge(from: TheCrudType): CrudBridge

  protected trait FieldPointerBridge {
    /**
     * What is the display name of this field?
     */
    def displayHtml: NodeSeq
  }

  /**
   * Based on a FieldPointer, build a FieldPointerBridge
   */
  protected implicit def buildFieldBridge(from: FieldPointerType): FieldPointerBridge
  
  lazy val Prefix = calcPrefix
  lazy val ListItems = calcListItems
  lazy val ViewItem = calcViewItem
  lazy val CreateItem = calcCreateItem
  lazy val EditItem = calcEditItem
  lazy val DeleteItem = calcDeleteItem

  /**
   * What's the prefix for this CRUD.  Typically the table name
   */
  def calcPrefix: List[String]

  /**
   * Vend a new instance of TheCrudType
   */
  def create: TheCrudType

  def calcListItems = "list"

  def calcViewItem = "view"

  def calcCreateItem = "create"

  def calcEditItem = "edit"

  def calcDeleteItem = "delete"

  def displayName = displayHtml.text

  def displayHtml: NodeSeq = Text(calcPrefix.head)

  /**
  * The fields displayed on the list page.  By default all
  * the displayed fields, but this list
  * can be shortened.
  */
  def fieldsForList: List[FieldPointerType] = fieldsForDisplay

  /**
   * When displaying a record, what fields do we display
   */
  def fieldsForDisplay: List[FieldPointerType]

  /**
   * The list of fields to present on a form form editting
   */
  def fieldsForEditing: List[FieldPointerType] = fieldsForDisplay

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
  Full(Menu(new Loc[TheCrudType]{
        // the name of the page
        def name = "View "+Prefix

        override val snippets: SnippetTest = {
          case ("crud.view", Full(wp: TheCrudType)) => displayRecord(wp) _
        }

        def defaultValue = Empty

        def params = viewMenuLocParams

        /**
         * What's the text of the link?
         */
        val text = new Loc.LinkText(calcLinkText _)

        def calcLinkText(in: TheCrudType): NodeSeq = Text(S.??("crudify.menu.view.displayName", displayName))

        /**
         * Rewrite the request and emit the type-safe parameter
         */
        override val rewrite: LocRewrite =
        Full(NamedPF(name) {
            case RewriteRequest(pp , _, _) if hasParamFor(pp, viewPath) =>
              (RewriteResponse(viewPath),
               findForParam(pp.wholePath.last).open_!)
          })

        def displayRecord(entry: TheCrudType)(in: NodeSeq): NodeSeq = {
          def doRow(in: NodeSeq): NodeSeq =
            for {
              pointer <- fieldsForDisplay
              field <- computeFieldFromPointer(entry, pointer).toList
              if field.shouldDisplay_?
              node <- bind("crud", in, 
                           "name" -> field.displayHtml, 
                           "value" -> field.asHtml)
            } yield node

          bind("crud", in, "row" -> doRow _)
        }

        override def calcTemplate = Full(viewTemplate)

        val link =
        new Loc.Link[TheCrudType](viewPath, false) {
          override def createLink(in: TheCrudType) =
          Full(Text(viewPathString+"/"+obscurePrimaryKey(in)))
        }
      }))
  /**
   * Override to include new Params for the view menu
   */
  def viewMenuLocParams: List[Loc.LocParam[TheCrudType]] = Nil


  /**
   * The menu item for editing an item (make this "Empty" to disable)
   */
  def editMenuLoc: Box[Menu] = {
    Full(Menu(new Loc[TheCrudType]{
          // the name of the page
          def name = "Edit "+Prefix

          override val snippets: SnippetTest = {
            case ("crud.edit", Full(wp: TheCrudType)) => crudDoForm(wp, S.??("Save"))
          }

          def defaultValue = Empty

          def params = editMenuLocParams

          /**
           * What's the text of the link?
           */
          val text = new Loc.LinkText(calcLinkText _)

          def calcLinkText(in: TheCrudType): NodeSeq = Text(S.??("crudify.menu.edit.displayName", displayName))

          /**
           * Rewrite the request and emit the type-safe parameter
           */
          override val rewrite: LocRewrite =
          Full(NamedPF(name) {
              case RewriteRequest(pp , _, _) if hasParamFor(pp, editPath) =>
                (RewriteResponse(editPath),
                 findForParam(pp.wholePath.last).open_!)
            })

          override def calcTemplate = Full(editTemplate)

          val link =
          new Loc.Link[TheCrudType](editPath, false) {
            override def createLink(in: TheCrudType) =
            Full(Text(editPathString+"/"+obscurePrimaryKey(in)))
          }
        }))
  }

  /**
   * Override to include new Params for the edit menu
   */
  def editMenuLocParams: List[Loc.LocParam[TheCrudType]] = Nil


  def editMenuName = S.??("Edit")+" "+displayName

  /**
   * This is the template that's used to render the page after the
   * optional wrapping of the template in the page wrapper
   */
  def editTemplate(): NodeSeq = pageWrapper(_editTemplate)

  def editId = "edit_page"
  def editClass = "edit_class"
  def editErrorClass = "edit_error_class"

  /**
   * The core template for editting.  Does not include any
   * page wrapping
   */
  protected def _editTemplate = {
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
  }

  def editButton = S.??("Save")

  /**
   * The menu item for deleting an item (make this "Empty" to disable)
   */
  def deleteMenuLoc: Box[Menu] = {
    Full(Menu(new Loc[TheCrudType]{
          // the name of the page
          def name = "Delete "+Prefix

          override val snippets: SnippetTest = {
            case ("crud.delete", Full(wp: TheCrudType)) => crudyDelete(wp)
          }

          def crudyDelete(item: TheCrudType)(html: NodeSeq): NodeSeq = {
            val from = referer

            def doFields(html: NodeSeq): NodeSeq =
              for {
                pointer <- fieldsForDisplay
                field <- computeFieldFromPointer(item, pointer).toList
                if field.shouldDisplay_?
                node <- bind("crud", html, 
                             "name" -> field.displayHtml,
                             "value" -> field.asHtml)
              } yield node

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

          def calcLinkText(in: TheCrudType): NodeSeq = Text(S.??("crudify.menu.delete.displayName", displayName))

          /**
           * Rewrite the request and emit the type-safe parameter
           */
          override val rewrite: LocRewrite =
          Full(NamedPF(name) {
            case RewriteRequest(pp , _, _) if hasParamFor(pp, deletePath) =>
                (RewriteResponse(deletePath),
                 findForParam(pp.wholePath.last).open_!)
            })

          override def calcTemplate = Full(deleteTemplate)

          val link =
          new Loc.Link[TheCrudType](deletePath, false) {
            override def createLink(in: TheCrudType) =
            Full(Text(deletePathString+"/"+obscurePrimaryKey(in)))
          }
        }))
  }

  private def hasParamFor(pp: ParsePath, toTest: List[String]): Boolean = {
    pp.wholePath.startsWith(toTest) &&
    pp.wholePath.length == (toTest.length + 1) &&
    findForParam(pp.wholePath.last).isDefined
  }

  /**
   * Override to include new Params for the delete menu
   */
  def deleteMenuLocParams: List[Loc.LocParam[TheCrudType]] = Nil


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

  /**
   * Given a range, find the records.  Your implementation of this
   * method should enforce ordering (e.g., on primary key)
   */
  def findForList(start: Long, count: Int): List[TheCrudType] 

  /**
   * Given a String that represents the primary key, find an instance of
   * TheCrudType
   */
  def findForParam(in: String): Box[TheCrudType]

  /**
   * Given an instance of TheCrudType and FieldPointerType, convert
   * that to an actual instance of a BaseField on the instance of TheCrudType
   */
  protected def computeFieldFromPointer(instance: TheCrudType, pointer: FieldPointerType): Box[BaseField]

  /**
   * This method defines how many rows are displayed per page.  By
   * default, it's hard coded at 20, but you can make it session specific
   * or change the default by overriding this method
   */
  protected def rowsPerPage: Int = 20

  lazy val locSnippets = new DispatchLocSnippets {
    val dispatch: PartialFunction[String, NodeSeq => NodeSeq] = {
      case "crud.all" => doCrudAll
      case "crud.create" => crudDoForm(create, S.??("Created"))
    }

    def doCrudAll(in: NodeSeq): NodeSeq = {
      val first = S.param("first").map(toLong) openOr 0L
      val list = findForList(first, rowsPerPage)

      def prev(in: NodeSeq) = if (first < rowsPerPage) <xml:group>&nbsp;</xml:group>
      else <a href={listPathString+"?first="+(0L max (first - rowsPerPage.toLong))}>{in}</a>

      def next(in: NodeSeq) = if (list.length < rowsPerPage) <xml:group>&nbsp;</xml:group>
      else <a href={listPathString+"?first="+(first + rowsPerPage.toLong)}>{in}</a>


      def doHeaderItems(in: NodeSeq): NodeSeq = fieldsForList.flatMap(f => 
        bind("crud", in, "name" -> f.displayHtml))

      def doRows(in: NodeSeq): NodeSeq =
      list.take(rowsPerPage).flatMap{
        c =>
        def doRowItem(in: NodeSeq): NodeSeq = 
          for {
            pointer <- fieldsForList
            field <- computeFieldFromPointer(c, pointer).toList
            node <- bind("crud", in, "value" -> field.asHtml)
          } yield node

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

  /**
   * This method can be used to obscure the primary key.  This is more secure
   * because end users will not have access to the primary key.
   */
  def obscurePrimaryKey(in: TheCrudType): String = obscurePrimaryKey(in.primaryKeyFieldAsString)

  /**
   * This method can be used to obscure the primary key.  This is more secure
   * because end users will not have access to the primary key.  This method
   * actually does the obfuscation.  You can use Mapper's KeyObfuscator class
   * to implement a nice implementation of this method for session-by-session
   * obfuscation.<br/><br/>
   *
   * By default, there's no obfuscation.  Note that if you obfuscate the
   * primary key, you need to update the findForParam method to accept
   * the obfuscated keys (and translate them back.)
   */
  def obscurePrimaryKey(in: String): String = in

  def referer: String = S.referer openOr listPathString

  /**
   * As the field names are being displayed for editting, this method
   * is called with the XHTML that will be displayed as the field name
   * an a flag indicating that the field is required (or not).  You
   * can wrap the fieldName in a span with a css class indicating that
   * the field is required or otherwise do something to update the field
   * name indiciating to the user that the field is required.  By default
   * the method wraps the fieldName in a span with the class attribute set
   * to "required_field"
   */
  def wrapNameInRequired(fieldName: NodeSeq, required: Boolean): NodeSeq = {
    if (required) {
      <span class="required_field">{fieldName}</span>
    } else {
      fieldName
    }
  }

  def crudDoForm(item: TheCrudType, noticeMsg: String)(in: NodeSeq): NodeSeq = {
    val from = referer
    val snipName = S.currentSnippet

    def loop(html:NodeSeq): NodeSeq = {
      def error(field: BaseField): NodeSeq = 
        field.uniqueFieldId match {
	  case fid @ Full(id) => S.getNotices.filter(_._3 == fid).flatMap(err =>
	    List(Text(" "), <span class={editErrorClass}>{err._2}</span>) )

	  case _ => NodeSeq.Empty
	}

      def doFields(html: NodeSeq): NodeSeq =
        for {
          pointer <- fieldsForEditing
          field <- computeFieldFromPointer(item, pointer).toList
          if field.show_?
          form <- field.toForm.toList
          node <- bind("crud", html,
                       "name" -> (wrapNameInRequired(field.displayHtml,
                                                    field.required_?) ++
                                  error(field)),
                       "form" -> form)
        } yield node

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

}
}
