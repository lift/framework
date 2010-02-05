/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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
package record {
package field {

import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http.{S, SHtml}
import _root_.net.liftweb.http.js._
import Helpers._
import S._
import JE._

class BooleanField[OwnerType <: Record[OwnerType]](rec: OwnerType) extends Field[Boolean, OwnerType] {
  def owner = rec

  def this(rec: OwnerType, value: Boolean) = {
    this(rec)
    set(value)
  }

  def this(rec: OwnerType, value: Box[Boolean]) = {
    this(rec)
    setBox(value)
  }

  def setFromAny(in: Any): Box[Boolean] = genericSetFromAny(in)
  def setFromString(s: String): Box[Boolean] = setBox(tryo(toBoolean(s)))

  private def elem = SHtml.checkbox(value, this.set _, "tabIndex" -> tabIndex.toString)

  def toForm = {
    // FIXME? no support for optional_?
    //var el = elem
    uniqueFieldId match {
      case Full(id) =>
        <div id={id+"_holder"}><div><label
              for={id+"_field"}>{displayName}</label></div>{SHtml.checkbox(value, this.set _, "tabIndex" -> tabIndex.toString, "id" -> (id+"_field"))}<lift:msg id={id}/></div>
      case _ => <div>{elem}</div>
    }

  }

  def asXHtml: NodeSeq = {

    uniqueFieldId match {
      case Full(id) => SHtml.checkbox(value, this.set _,
                                      "tabIndex" -> tabIndex.toString,
                                      "id" -> (id+"_field"))
      case _ => elem
    }
  }


  def defaultValue = false

  def asJs: JsExp = valueBox.map(boolToJsExp) openOr JsNull

}

import _root_.java.sql.{ResultSet, Types}
import _root_.net.liftweb.mapper.{DriverType}

/**
 * An int field holding DB related logic
 */
class DBBooleanField[OwnerType <: DBRecord[OwnerType]](rec: OwnerType) extends BooleanField[OwnerType](rec) with JDBCFieldFlavor[Boolean] {

  def targetSQLType = _root_.java.sql.Types.BOOLEAN

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.enumColumnType

  def jdbcFriendly(field : String) : Boolean = value

}

}
}
}
