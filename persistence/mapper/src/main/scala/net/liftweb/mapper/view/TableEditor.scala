/*
 * Copyright 2009-2011 WorldWide Conferencing, LLC
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
package mapper
package view

import xml.{NodeSeq, Text}

import common.{Box, Full, Empty}
import util.Helpers
import Helpers._
import http.{SHtml, S, DispatchSnippet, js}
import S.?

import js.JsCmds.{Script, Run}

import mapper.{Mapper, MetaMapper, LongKeyedMetaMapper, MappedField}

import Util._


/**
 * Keeps track of pending adds to and removes from a list of mappers.
 * Supports in-memory sorting by a field.
 * Usage: override metaMapper with a MetaMapper instance, call sortBy
 * to specify the field to sort by. If it is already sorted by that
 * field it will sort descending, otherwise ascending.
 * Call save to actualize changes.
 * @author nafg
 */
trait ItemsList[T <: Mapper[T]] {
  /**
   * The MetaMapper that provides create and findAll functionality etc.
   * Must itself be a T (the mapper type it represents)
   */
  def metaMapper: T with MetaMapper[T]

  /**
   * Whether the sorting algorithm should put null first or last
   */
  var sortNullFirst = true
  /**
   * The list of items that correspond to items in the database
   */
  var current: List[T] = Nil
  /**
   * The list of items pending to be added to the database
   */
  var added: List[T] = Nil
  /**
   * The list of items to be deleted from current
   */
  var removed: List[T] = Nil
  /**
   * The field to sort by, if any
   */
  var sortField: Option[MappedField[_, T]] = None
  /**
   * The sort direction
   */
  var ascending = true

  /**
   * Returns the items (current + added - removed), sorted.
   * Sorting sorts strings case-insensitive, as well as Ordered and java.lang.Comparable.
   * Anything else where both values are nonnull are sorted via their toString method (case sensitive)
   */
  def items: Seq[T] = {
    import scala.util.Sorting._
    val unsorted: List[T] = current.filterNot(removed.contains) ++ added
    sortField match {
      case None =>
        unsorted
      case Some(field) =>
        unsorted.sortWith {
          (a, b) => ((field.actualField(a).get: Any, field.actualField(b).get: Any) match {
            case (aval: String, bval: String) => aval.toLowerCase < bval.toLowerCase
            case (aval: Ordered[_], bval: Ordered[_]) =>
              aval.asInstanceOf[Ordered[Any]] < bval.asInstanceOf[Ordered[Any]]
            case (aval: java.lang.Comparable[_], bval: java.lang.Comparable[_]) =>
              (aval.asInstanceOf[java.lang.Comparable[Any]] compareTo bval.asInstanceOf[java.lang.Comparable[Any]]) < 0
            case (null, _) => sortNullFirst
            case (_, null) => !sortNullFirst
            case (aval, bval) => aval.toString < bval.toString
          }) match {
            case cmp =>
              if(ascending) cmp else !cmp
          }
        }
    }
  }
  /**
   * Adds a new, unsaved item
   */
  def add {
    added ::= metaMapper.create
  }
  /**
   * Marks an item pending for removal
   */
  def remove(i: T) {
    if(added.exists(i.eq))
      added = added.filter(i.ne)
    else if(current.contains(i))
      removed ::= i
  }
  /**
   * Reset the ItemsList from the database: calls refresh, and 'added' and 'removed' are cleared.
   */
  def reload {
    refresh
    added = Nil
    removed = Nil
  }
  /**
   * Reloads the contents of 'current' from the database
   */
  def refresh {
    current = metaMapper.findAll
  }
  /**
   * Sends to the database:
   *  added is saved
   *  removed is deleted
   *  (current - removed) is saved
   */
  def save {
    val (successAdd, failAdd) = added.partition(_.save)
    added = failAdd

    val (successRemove, failRemove) = removed.partition(_.delete_!)
    current = current.filterNot(successRemove.contains)
    removed = failRemove

    for(c <- current if c.validate.isEmpty) c.save

    current ++= successAdd
  }


  def sortBy(field: MappedField[_, T]) = (sortField, ascending) match {
    case (Some(f), true) if f eq field =>
      ascending = false
    case _ | null =>
      sortField = Some(field)
      ascending = true
  }
  def sortFn(field: MappedField[_, T]) = (sortField, ascending) match {
    case (Some(f), true) if f eq field =>
      () => ascending = false
    case _ | null =>
      () => {
        sortField = Some(field)
        ascending = true
      }
  }

  reload
}


/**
 * Holds a registry of TableEditor delegates
 * Call TableEditor.registerTable(name_to_use_in_view, meta_mapper_for_the_table, display_title)
 * in Boot after DB.defineConnectionManager.
 * Referencing TableEditor triggers registering its snippet package and enabling
 * the provided template, /tableeditor/default.
 * @author nafg
 */
object TableEditor {
  net.liftweb.http.LiftRules.addToPackages("net.liftweb.mapper.view")

  private[view] val map = new scala.collection.mutable.HashMap[String, TableEditorImpl[_]]
  def registerTable[T<:Mapper[T]](name: String, meta: T with MetaMapper[T], title: String) =
    map(name) = new TableEditorImpl(title, meta)
}

package snippet {
  /**
   * This is the snippet that the view references.
   * It requires the following contents:
   * table:title - the title registered in Boot
   * header:fields - repeated for every field of the MetaMapper, for the header.
   *  field:name - the displayName of the field, capified. Links to sort by the field.
   * table:items - repeated for each record
   * item:fields - repeated for each field of the current record
   *  field:form - the result of toForm on the field
   * item:removeBtn - a button to remove the current item
   * table:insertBtn - a button to insert another item
   * For a default layout, use lift:embed what="/tableeditor/default", with
   * @author nafg
   */
  class TableEditor extends DispatchSnippet {
    private def getInstance: Box[TableEditorImpl[_]] = S.attr("table").map(TableEditor.map(_))
    def dispatch = {
      case "edit" =>
        val o = getInstance.openOrThrowException("if we don't have the table attr, we want the dev to know about it.")
        o.edit
    }
  }
}

/**
 * This class does the actual view binding against a ItemsList.
 * The implementation is in the base trait ItemsListEditor
 * @author nafg
 */
protected class TableEditorImpl[T <: Mapper[T]](val title: String, meta: T with MetaMapper[T]) extends ItemsListEditor[T] {
  var items = new ItemsList[T] {
    def metaMapper = meta
  }
}

/**
 * General trait to edit an ItemsList.
 * @author nafg
 */
trait ItemsListEditor[T<:Mapper[T]] {
  def items: ItemsList[T]
  def title: String

  def onInsert: Unit = items.add
  def onRemove(item: T): Unit = items.remove(item)
  def onSubmit: Unit = try {
    items.save
  } catch {
    case e: java.sql.SQLException =>
      S.error("Not all items could be saved!")
  }
  def sortFn(f: MappedField[_, T]): ()=>Unit = items.sortFn(f)

  val fieldFilter: MappedField[_,T]=>Boolean = (f: MappedField[_,T])=>true

  def customBind(item: T): NodeSeq=>NodeSeq = (ns: NodeSeq) => ns

  def edit: (NodeSeq)=>NodeSeq = {
    def unsavedScript = (<head>{Script(Run("""
                           var safeToContinue = false
                           window.onbeforeunload = function(evt) {{  // thanks Tim!
                             if(!safeToContinue) {{
                               var reply = "You have unsaved changes!";
                               if(typeof evt == 'undefined') evt = window.event;
                               if(evt) evt.returnValue = reply;
                               return reply;
                             }}
                           }}
    """))}</head>)
    val noPrompt = "onclick" -> "safeToContinue=true"
    val optScript = if(
      (items.added.length + items.removed.length == 0) &&
        items.current.forall(!_.dirty_?)
    ) {
      NodeSeq.Empty
    } else {
      unsavedScript
    }

    val bindRemovedItems =
      items.removed.map { item =>
        "^" #> customBind(item) andThen
        ".fields" #> eachField(item, { f: MappedField[_, T] => ".form" #> <strike>{f.asHtml}</strike> }) &
        ".removeBtn" #> SHtml.submit(?("Remove"), ()=>onRemove(item), noPrompt) &
        ".msg" #> Text(?("Deleted"))
      }

    val bindRegularItems =
      items.items.map { item =>
        "^" #> customBind(item) andThen
        ".fields" #> eachField(item, { f: MappedField[_, T] => ".form" #> f.toForm }) &
        ".removeBtn" #> SHtml.submit(?("Remove"), ()=>onRemove(item), noPrompt) &
        ".msg" #> {
          item.validate match {
            case Nil =>
              if (! item.saved_?)
                Text(?("New"))
              else if (item.dirty_?)
                Text(?("Unsaved"))
              else
                NodeSeq.Empty
            case errors =>
              <ul>{errors.flatMap(e => <li>{e.msg}</li>)}</ul>
          }
        }
      }

    "^ >*" #> optScript andThen
    ".fields *" #> {
      eachField[T](
        items.metaMapper,
        { f: MappedField[_, T] =>
          ".name" #> SHtml.link(S.uri, sortFn(f), Text(capify(f.displayName)))
        },
        fieldFilter
      )
    } &
    ".table" #> {
      ".title *" #> title &
      ".insertBtn" #> SHtml.submit(?("Insert"), onInsert _, noPrompt) &
      ".item" #> (bindRegularItems ++ bindRemovedItems) &
      ".saveBtn" #> SHtml.submit(?("Save"), onSubmit _, noPrompt)
    }
  }
}
