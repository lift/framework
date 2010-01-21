/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
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
package view {

import net.liftweb.http.{SHtml, S, DispatchSnippet}
import S.?
import net.liftweb.util.BindPlus._
import net.liftweb.util.{Helpers, BindHelpers}
import net.liftweb.common.{Box, Full, Empty}
import Helpers._

import net.liftweb.mapper.{Mapper,
                           MetaMapper,
                           LongKeyedMetaMapper,
                           MappedField}

import Util._

import scala.xml.{NodeSeq, Text}

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
  var sortNullFirst = true
  var current: List[T] = Nil
  var added: List[T] = Nil
  var removed: List[T] = Nil
  import scala.util.Sorting._
  def items: Seq[T] = {
    val unsorted: List[T] = current ++ added filter {i => removed.forall(i.ne)}
    sortField match {
      case None =>
        unsorted
      case Some(field) =>
        unsorted.sort {
          (a, b) => ((field.actualField(a).is: Any, field.actualField(b).is: Any) match {
            case (aval: String, bval: String) => aval.toLowerCase < bval.toLowerCase
            case (aval: Ordered[Any], bval: Ordered[Any]) => aval < bval
            case (aval: java.lang.Comparable[Any], bval: java.lang.Comparable[Any]) => (aval compareTo bval) < 0
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
  def add {
    added = metaMapper.create :: added
  }
  def remove(i: T) {
    removed = i :: removed
  }
  def reload {
    current = metaMapper.findAll
    added = Nil
    removed = Nil
  }
  def save {
    val toSave = (added++current) filter {i=>removed.forall(i.ne)}
    val toRemove = removed filter {_.saved_?}
    
//    val (valid, invalid) = toSave.partition(_.validate eq Nil)
//    val (saved, unsaved) = valid.partition(_.save)
    val saved = toSave filter {_.validate eq Nil} filter (_.save)
//    val (deleted, notdeleted) = toRemove.partition(_.delete_!)
    val deleted = toRemove filter (_.delete_!)
    
    removed --= deleted
    added --= saved
    current = current ++ saved removeDuplicates
    
  }
  
  var sortField: Option[MappedField[_, T]] = None
  var ascending = true
  
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
        val o = getInstance.open_!
        o.edit _
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
  def onSubmit: Unit = items.save
  def sortFn(f: MappedField[_, T]): ()=>Unit = items.sortFn(f)
  
  val fieldFilter: MappedField[_,T]=>Boolean = (f: MappedField[_,T])=>true
  
  def edit(xhtml: NodeSeq) = {
    xhtml.bind("header",
         "fields" -> eachField[T](
             items.metaMapper,
             (f: MappedField[_,T]) => Seq(
               "name" -> SHtml.link(S.uri, sortFn(f), Text(capify(f.displayName)))
             ),
             fieldFilter
         )
    ).bind("table",
         "title" -> title,
         "insertBtn" -> SHtml.submit(?("Insert"), onInsert _),
         "items" -> ((ns:NodeSeq)=>NodeSeq.fromSeq(items.items.flatMap {i =>
           bind("item",
                ns,
                "fields" -> eachField(
                  i,
                  (f: MappedField[_,T]) => Seq("form" -> f.toForm),
                  fieldFilter
                ),
                "removeBtn" -> SHtml.submit(?("Remove"), ()=>onRemove(i)),
                "msg" -> (i.validate match {
                  case Nil =>
                    if(!i.saved_?) Text(?("New")) else if(i.dirty_?) Text(?("Unsaved")) else NodeSeq.Empty
                  case errors => (<ul>{errors.flatMap(e => <li>{e.msg}</li>)}</ul>)
                })
           )
         })),
         "saveBtn" -> SHtml.submit(?("Save"), onSubmit _)
    )
  }
  
}

}
}
}
