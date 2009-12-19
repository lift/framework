/*
 * Copyright 2007-2009 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb.record.field

import scala.xml._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http.{S}
import S._
import Helpers._

class TextareaField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int) extends StringField(rec, maxLength) {

  private def elem = S.fmapFunc(SFuncHolder(this.setFromAny(_))){
    funcName => <textarea name={funcName}
      rows={textareaRows.toString}
      cols={textareaCols.toString}
      tabindex={tabIndex toString}>{value match {case null => "" case s => s.toString}}</textarea>
  }

  override def toForm = {
    var el = elem

    uniqueFieldId match {
      case Full(id) =>
        <div id={id+"_holder"}><div><label for={id+"_field"}>{displayName}</label></div>{el % ("id" -> (id+"_field"))}<lift:msg id={id}/></div>
      case _ => <div>{el}</div>
    }

  }

  override def asXHtml: NodeSeq = {
    var el = elem

    uniqueFieldId match {
      case Full(id) =>  el % ("id" -> (id+"_field"))
      case _ => el
    }
  }


  override def toString = {
    if (value == null || value.length < 100) super.toString
    else value.substring(0,40) + " ... " + value.substring(value.length - 40)
  }

  def textareaRows  = 8

  def textareaCols = 20

}

import _root_.java.sql.{ResultSet, Types}
import _root_.net.liftweb.mapper.{DriverType}

/**
 * A string field holding DB related logic
 */
abstract class DBTextareaField[OwnerType <: DBRecord[OwnerType]](rec: OwnerType, maxLength: Int) extends
TextareaField[OwnerType](rec, maxLength) with JDBCFieldFlavor[String]{

  def targetSQLType = Types.VARCHAR

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName+" VARCHAR("+maxLength+")"

  def jdbcFriendly(field : String) : String = value
}
