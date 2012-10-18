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

import scala.xml.Node
import net.liftweb.http.SHtml

/**
 * This trait can be added to existing Mapper fields to make them use AjaxUtils.editable
 * for field display.
 */
trait AjaxEditableField[FieldType,OwnerType <: Mapper[OwnerType]] extends MappedField[FieldType,OwnerType] {
  override def asHtml : Node =
    if (editableField) {
      <xml:group>{
        toForm.map { form =>
          SHtml.ajaxEditable(super.asHtml, form, () => {fieldOwner.save; onSave; net.liftweb.http.js.JsCmds.Noop})
        } openOr super.asHtml
      }</xml:group>
    } else {
      super.asHtml
    }

  /** This method is called when the element's data are saved. The default is to do nothing */
  def onSave {}

  /** This method allows you to do programmatic control of whether the field will display
   *  as editable. The default is true */
  def editableField = true
}

