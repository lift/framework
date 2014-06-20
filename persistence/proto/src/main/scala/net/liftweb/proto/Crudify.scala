/*
 * Copyright 2006-2011 WorldWide Conferencing, LLC
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

package net.liftweb
package proto

import sitemap._
import Loc._
import http._
import util._
import common._
import Helpers._

import scala.xml._

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
   * What's the prefix for this CRUD.  Typically the table name.
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
   * The list of fields to present on a form form editing
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
                addlMenuLocParams ::: (
                  locSnippets :: Loc.Template(showAllTemplate) :: 
                  showAllMenuLocParams))))

  /**
   * Override to include new Params for the show all menu
   */
  def showAllMenuLocParams: List[Loc.AnyLocParam] = Nil

  /**
   * The menu item for creating items (make this "Empty" to disable)
   */
  def createMenuLoc: Box[Menu] =
  Full(Menu(Loc("Create "+Prefix, createPath, createMenuName,
                (addlMenuLocParams ::: (
                  locSnippets :: Loc.Template(createTemplate) ::
                  createMenuLocParams)))))
  /**
   * Override to include new Params for the create menu
   */
  def createMenuLocParams: List[Loc.AnyLocParam] = Nil

  /**
   * If there are any Loc.LocParams that need to be
   * added to every menu (e.g., a guard for access control
   * of the Crudify screens)
   */
  protected def addlMenuLocParams: List[Loc.AnyLocParam] = Nil


  /**
   * Customize the display of a row for displayRecord
   */
  protected def doDisplayRecordRow(entry: TheCrudType): (NodeSeq)=>NodeSeq = {
    "^" #> {
      for {
        pointer <- fieldsForDisplay
        field <- computeFieldFromPointer(entry, pointer).toList
        if field.shouldDisplay_?
      } yield {
        ".name *" #> field.displayHtml &
        ".value *" #> field.asHtml
      }
    }
  }
  
  /**
   * Customize the display of records for view menu loc
   */
  protected def displayRecord(entry: TheCrudType): (NodeSeq)=>NodeSeq = {
    ".row" #> doDisplayRecordRow(entry)
  }


  /**
   * The menu item for viewing an item (make this "Empty" to disable)
   */
  def viewMenuLoc: Box[Menu] =
  Full(Menu(new Loc[TheCrudType]{
        // the name of the page
        def name = "View "+Prefix

        override val snippets: SnippetTest = {
          case ("crud.view", Full(wp)) => displayRecord(wp.asInstanceOf[TheCrudType])
        }

        def defaultValue = Empty

        lazy val params = addlMenuLocParams ::: viewMenuLocParams

        /**
         * What's the text of the link?
         */
        val text = new Loc.LinkText(calcLinkText _)

        def calcLinkText(in: TheCrudType): NodeSeq = Text(S.?("crudify.menu.view.displayName", displayName))

        /**
         * Rewrite the request and emit the type-safe parameter
         */
        override val rewrite: LocRewrite =
        Full(NamedPF(name) {
            case RewriteRequest(pp , _, _) if hasParamFor(pp, viewPath) =>
              (RewriteResponse(viewPath),
               findForParam(pp.wholePath.last).openOrThrowException("legacy code, it was open_!"))
          })

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
            case ("crud.edit", Full(wp)) => crudDoForm(wp.asInstanceOf[TheCrudType], S.?("Save"))
          }

          def defaultValue = Empty

          lazy val params = addlMenuLocParams ::: editMenuLocParams

          /**
           * What's the text of the link?
           */
          val text = new Loc.LinkText(calcLinkText _)

          def calcLinkText(in: TheCrudType): NodeSeq = Text(S.?("crudify.menu.edit.displayName", displayName))

          /**
           * Rewrite the request and emit the type-safe parameter
           */
          override val rewrite: LocRewrite =
          Full(NamedPF(name) {
              case RewriteRequest(pp , _, _) if hasParamFor(pp, editPath) =>
                (RewriteResponse(editPath),
                 findForParam(pp.wholePath.last).openOrThrowException("legacy code, it was open_!"))
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


  /**
   * The String displayed for menu editing
   */
  def editMenuName = S.?("Edit")+" "+displayName

  /**
   * This is the template that's used to render the page after the
   * optional wrapping of the template in the page wrapper
   */
  def editTemplate(): NodeSeq = pageWrapper(_editTemplate)

  def editId = "edit_page"
  def editClass = "edit_class"
  def editErrorClass = "edit_error_class"

  /**
   * The core template for editing.  Does not include any
   * page wrapping.
   */
  protected def _editTemplate = {
    <div data-lift="crud.edit?form=post">
      <table id={editId} class={editClass}>
        <tr class="field">
          <td class="name"></td>
          <td class="form"></td>
        </tr>

        <tr>
          <td>&nbsp;</td>
          <td><button type="submit">{editButton}</button></td>
        </tr>
      </table>
    </div>
  }

  def editButton = S.?("Save")

  /**
   * Override this method to change how fields are displayed for delete
   */
  protected def doDeleteFields(item: TheCrudType): (NodeSeq)=>NodeSeq = {
    "^" #> {
      for {
        pointer <- fieldsForDisplay
        field <- computeFieldFromPointer(item, pointer).toList
        if field.shouldDisplay_?
      } yield {
        ".name *" #> field.displayHtml &
        ".value *" #> field.asHtml
      }
    }
  }
  
  /**
   * Override this method to change the behavior of deleting an item
   */
  protected def doDeleteSubmit(item: TheCrudType, from: String)() = {
    S.notice(S ? "Deleted")
    item.delete_!
    S.redirectTo(from)
  }



  /**
   * Override this method to change how the delete screen is built
   */
  protected def crudyDelete(item: TheCrudType): (NodeSeq)=>NodeSeq = {
    val from = referer
    
    ".field" #> doDeleteFields(item) &
    "type=submit" #> SHtml.onSubmitUnit(doDeleteSubmit(item, from) _)
  }


  /**
   * The menu item for deleting an item (make this "Empty" to disable)
   */
  def deleteMenuLoc: Box[Menu] = {
    Full(Menu(new Loc[TheCrudType]{
          // the name of the page
          def name = "Delete "+Prefix

          override val snippets: SnippetTest = {
            case ("crud.delete", Full(wp)) => crudyDelete(wp.asInstanceOf[TheCrudType])
          }

          def defaultValue = Empty

          lazy val params = addlMenuLocParams ::: deleteMenuLocParams

          /**
           * What's the text of the link?
           */
          val text = new Loc.LinkText(calcLinkText _)

          def calcLinkText(in: TheCrudType): NodeSeq = Text(S.?("crudify.menu.delete.displayName", displayName))

          /**
           * Rewrite the request and emit the type-safe parameter
           */
          override val rewrite: LocRewrite =
          Full(NamedPF(name) {
            case RewriteRequest(pp , _, _) if hasParamFor(pp, deletePath) =>
                (RewriteResponse(deletePath),
                 findForParam(pp.wholePath.last).openOrThrowException("legacy code, it was open_!"))
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


  def deleteMenuName = S.?("Delete")+" "+displayName

  /**
   * This is the template that's used to render the page after the
   * optional wrapping of the template in the page wrapper
   */
  def deleteTemplate(): NodeSeq = pageWrapper(_deleteTemplate)

  def deleteId = "delete_page"
  def deleteClass = "delete_class"

  /**
   * The core template for deleting.  Does not include any
   * page wrapping.
   */
  def _deleteTemplate = {
    <div data-lift="crud.delete?form=post">
      <table id={deleteId} class={deleteClass}>
        <tr class="field">
          <td class="name"></td>
          <td class="value"></td>
        </tr>

        <tr>
          <td>&nbsp;</td>
          <td><button type="submit">{deleteButton}</button></td>
        </tr>
      </table>
    </div>
  }

  def deleteButton = S.?("Delete")


  def createMenuName = S.?("Create")+" "+displayName

  /**
   * This is the template that's used to render the page after the
   * optional wrapping of the template in the page wrapper.
   */
  def createTemplate(): NodeSeq = pageWrapper(_createTemplate)

  def createId = "create_page"
  def createClass = "create_class"

  /**
   * The core template for creating.  Does not include any
   * page wrapping.
   */
  def _createTemplate = {
    <div data-lift="crud.create?form=post">
      <table id={createId} class={createClass}>
        <tr class="field">
          <td class="name"></td>
          <td class="form"></td>
        </tr>

        <tr>
          <td>&nbsp;</td>
          <td><button type="submit">{createButton}</button></td>
        </tr>
      </table>
    </div>
  }

  def createButton = S.?("Create")

  def viewMenuName = S.?("View")+" "+displayName

  /**
   * This is the template that's used to render the page after the
   * optional wrapping of the template in the page wrapper
   */
  def viewTemplate(): NodeSeq = pageWrapper(_viewTemplate)

  def viewId = "view_page"
  def viewClass = "view_class"

  /**
   * The core template for viewing.  Does not include any
   * page wrapping.
   */
  def _viewTemplate = {
    <div data-lift="crud.view">
      <table id={viewId} class={viewClass}>
        <tr class="row">
          <td class="name"></td>
          <td class="value"></td>
        </tr>
      </table>
    </div>
  }
    
  def showAllMenuName = S.?("List", displayName)

  /**
   * This is the template that's used to render the page after the
   * optional wrapping of the template in the page wrapper
   */
  def showAllTemplate(): NodeSeq = pageWrapper(_showAllTemplate)

  def showAllId = "show_all"
  def showAllClass = "show_all"

  /**
   * The core template for showing record.  Does not include any
   * page wrapping
   */
  def _showAllTemplate = {
    <div data-lift="crud.all">
      <table id={showAllId} class={showAllClass}>
        <thead>
          <tr>
            <th class="header-item"></th>

            <th>&nbsp;</th>
            <th>&nbsp;</th>
            <th>&nbsp;</th>
          </tr>
        </thead>

        <tbody>
          <tr class="row">
            <td class="row-item"></td>

            <td><a class="view" href="view-uri">{S ? "View"}</a></td>
            <td><a class="edit" href="edit-uri">{S ? "Edit"}</a></td>
            <td><a class="delete" href="delete-uri">{S ? "Delete"}</a></td>
          </tr>
        </tbody>

        <tfoot>
          <tr>
            <td colspan="3" class="previous">{previousWord}</td>
            <td colspan="3" class="next">{nextWord}</td>
            </tr>
        </tfoot>
      </table>
    </div>
  }

  def nextWord = S.?("Next")
  def previousWord = S.?("Previous")

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
   * method should enforce ordering (e.g., on primary key).
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
   * or change the default by overriding this method.
   */
  protected def rowsPerPage: Int = 20

  /**
   * Override this method to customize how header items are treated
   */
  protected def doCrudAllHeaderItems: (NodeSeq)=>NodeSeq = {
    "^ *" #> fieldsForList.map(_.displayHtml)
  }
    
  /**
   * Override this method to customize how a crudAll line is generated
   */
  protected def doCrudAllRowItem(c: TheCrudType): (NodeSeq)=>NodeSeq = {
    "^" #> {
      for {
        pointer <- fieldsForList
        field <- computeFieldFromPointer(c, pointer).toList
      } yield {
        ".value *" #> field.asHtml
      }
    }
  }
  
  /**
   * Override this method to determine how all the rows on a crud
   * page are displayed
   */
  protected def doCrudAllRows(list: List[TheCrudType]): (NodeSeq)=>NodeSeq = {
    "^" #> list.take(rowsPerPage).map { rowItem =>
      ".row-item" #> doCrudAllRowItem(rowItem) &
      ".view [href]" #> (s"$viewPathString/${obscurePrimaryKey(rowItem)}") &
      ".edit [href]" #> (s"$editPathString/${obscurePrimaryKey(rowItem)}") &
      ".delete [href]" #> (s"$deletePathString/${obscurePrimaryKey(rowItem)}")
    }
  }
  
  /**
   * Override this method to change how the previous link is
   * generated
   */
  protected def crudAllPrev(first: Long): (NodeSeq)=>NodeSeq = {
    if (first < rowsPerPage) {
      ClearNodes
    } else {
      "^ <*>" #>
        <a href={listPathString+
                  "?first="+(0L max (first -
                                     rowsPerPage.toLong))}></a>
    }
  }
  
  /**
   * Override this method to change how the next link is generated
   */
  protected def crudAllNext(first: Long, list: List[TheCrudType]): (NodeSeq)=>NodeSeq = {
    if (first < rowsPerPage) {
      ClearNodes
    } else {
      "^ <*>" #>
        <a href={listPathString+"?first="+(first +
                                            rowsPerPage.toLong)}></a>
    }
  }

  /**
   * Override this method if you want to change the behavior
   * of displaying records via the crud.all snippet
   */
  protected def doCrudAll: (NodeSeq)=>NodeSeq = {
    val first = S.param("first").map(toLong) openOr 0L
    val list = findForList(first, rowsPerPage)

    ".header-item" #> doCrudAllHeaderItems &
    ".row" #> doCrudAllRows(list) &
    ".previous" #> crudAllPrev(first) &
    ".next" #> crudAllNext(first, list)
  }
  

  lazy val locSnippets = new DispatchLocSnippets {
    val dispatch: PartialFunction[String, NodeSeq => NodeSeq] = {
      case "crud.all" => doCrudAll
      case "crud.create" => crudDoForm(create, S.?("Created"))
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
   * As the field names are being displayed for editing, this method
   * is called with the XHTML that will be displayed as the field name
   * and a flag indicating whether the field is required.  You
   * can wrap the fieldName in a span with a css class indicating that
   * the field is required or otherwise do something to update the field
   * name indicating to the user that the field is required.  By default
   * the method wraps the fieldName in a span with the class attribute set
   * to "required_field".
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
      def error(field: BaseField): NodeSeq = {
        field.uniqueFieldId match {
          case fid @ Full(id) => S.getNotices.filter(_._3 == fid).flatMap(err =>
            List(Text(" "), <span class={editErrorClass}>{err._2}</span>) )

          case _ => NodeSeq.Empty
        }
      }

      def doFields(html: NodeSeq): NodeSeq =
        for {
          pointer <- fieldsForEditing
          field <- computeFieldFromPointer(item, pointer).toList
          if field.show_?
          form <- field.toForm.toList
          bindNode =
            ".name *" #> {
              wrapNameInRequired(field.displayHtml, field.required_?) ++
              error(field)
            } &
            ".form *" #> form
          node <- bindNode(html)
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

      val bind =
        ".field" #> doFields _ &
        "type=submit" #> SHtml.onSubmitUnit(doSubmit _)

      bind(html)
    }

    loop(in)
  }


}

