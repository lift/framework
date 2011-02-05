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

import net.liftweb.http.{StatefulSnippet, S}
import S.?
import net.liftweb.util.Helpers._

import net.liftweb.mapper.{Mapper, MetaMapper}

import scala.xml.{NodeSeq, Text}


/**
 * A snippet that can list and edit items of a particular Mapper class
 * This trait can help reduce boilerplate in the common scenario where
 * you want a snippet class to provide list and edit snippets for a
 * specific Mapper class.
 * @author nafg
 */
trait ModelSnippet[T <: Mapper[T]] extends StatefulSnippet {
  import mapper.view.{ModelView => MV}
  class ModelView(e: T, snippet: ModelSnippet[T]) extends MV[T](e, snippet) {
    def this(e: T) {
      this(e, this)
    }
  }
  /**
   * The instance of ModelView that wraps the currently loaded entity
   */
  val view: MV[T]

  /**
   * Action when save is successful. Defaults to using the ModelView's redirectOnSave
   */
  var onSave = (view: MV[T])=> {
    view.redirectOnSave.foreach(redirectTo)
  }

  /**
   * The list snippet
   */
  def list(ns: NodeSeq): NodeSeq
  /**
   * The edit snippet
   */
  def edit(ns: NodeSeq): NodeSeq
  
  def load(entity: T) = view.entity = entity

  def dispatch = {
    case "list" =>       list _
    case "edit" =>       edit _
    case "newOrEdit" =>  view.newOrEdit _
  }
  
  /**
   * An "edit" BindParam
  */
  def editAction(e: T) = TheBindParam("edit", link("edit", ()=>load(e), Text(?("Edit"))))
  /**
   * A "remove" BindParam
  */
  def removeAction(e: T) = TheBindParam("remove", link("list", ()=>e.delete_!, Text(?("Remove"))))
}


/**
 * A wrapper around a Mapper that provides view-related utilities. Belongs to a parent ModelSnippet.
 * @author nafg
 */
class ModelView[T <: Mapper[T]](var entity: T, val snippet: ModelSnippet[T]) {
  /**
   * If Some(string), will redirect to string on a successful save.
   * If None, will load the same page.
   * Defaults to Some("list").
   * This var is used by ModelSnippet.onSave, which is a ModelView=>Unit
   */
  var redirectOnSave: Option[String] = Some("list")
  
  /**
   * Loads this entity into the snippet so it can be edited 
   */
  def load = snippet.load(entity)
  
  /**
   * Delete the entity
   */
  def remove =
    entity.delete_!
  /**
   * This function is used as a snippet in the edit view
   * to provide alternate text depending on whether an
   * existing entity is being edited or a new one is being
   * created.
   */
  def newOrEdit(xhtml: NodeSeq) =
    chooseTemplate("if",
                   if(entity.saved_?) "edit" else "new",
                   xhtml)
  
  /**
   * This method checks whether the entity
   * validates; if so it saves it, and if
   * successful redirects to the location
   * specified by redirectOnSave, if any.
   * If save or validation fails, the
   * appropriate message(s) is/are displayed
   * and no redirect is performed.
   */
  def save {
    entity.validate match {
      case Nil =>
        if(entity.save)
          snippet.onSave(this)
        else
          S.error("Save failed")
      case errors =>
        S.error(errors)
      }
  }

  /**
   * returns a string that represents the id, or &lt;new&gt;
   * if the entity is a new entity.
   * If the entity has been saved then the id is determined
   * as follows: If it is a KeyedMapper then it calls toString
   * on the entity's primaryKeyField. Otherwise it
   * calls toString on a field named "id."
   */
  def idString = if(entity.saved_?)
    entity match {
      case e: net.liftweb.mapper.KeyedMapper[_,T] => 
        e.primaryKeyField.toString
      case _ => entity.fieldByName("id").toString
    }
  else
    "<new>"
  
  
  /**
   * Returns a BindParam that contains a link to load and edit this entity
   */
  lazy val editAction = TheBindParam("edit", snippet.link("edit", ()=>load, Text(?("Edit"))))
  /**
   * Returns a BindParam that contains a link to delete this entity
   */
  lazy val removeAction = TheBindParam("remove", snippet.link("list", ()=>remove, Text(?("Remove"))))
  /**
   * Returns a BindParam that binds "name" to the field named "name."
   * If the field has a Full toForm implementation then that is used;
   * otherwise its asHtml is called.
   */
  def edit(name: String) = {
    entity.fieldByName(name).map { (field: net.liftweb.mapper.MappedField[_,_]) =>
      TheBindParam(name, field.toForm.openOr(field.asHtml))
    }.open_!
  }
}

}
}
}
