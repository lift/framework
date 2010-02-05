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

import scala.xml._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http.{S}
import net.liftweb.http.js._
import _root_.java.util.regex._
import S._
import Helpers._
import JE._

/**
 * A Field containing String content.
 */
class StringField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int) extends Field[String, OwnerType] {

  def this(rec: OwnerType, maxLength: Int, value: String) = {
    this(rec, maxLength)
    set(value)
  }

  def this(rec: OwnerType, maxLength: Int, value: Box[String]) = {
    this(rec, maxLength)
    setBox(value)
  }

  def this(rec: OwnerType, value: String) = {
    this(rec, 100)
    set(value)
  }

  def this(rec: OwnerType, value: Box[String]) = {
    this(rec, 100)
    setBox(value)
  }

  def owner = rec

  def setFromAny(in: Any): Box[String] = in match {
    case seq: Seq[_] if !seq.isEmpty => setFromAny(seq.first)
    case _ => genericSetFromAny(in)
  }

  def setFromString(s: String): Box[String] = s match {
    case "" if optional_? => setBox(Empty)
    case _                => setBox(Full(s))
  }

  private def elem = S.fmapFunc(SFuncHolder(this.setFromAny(_))) {
    funcName =>
    <input type="text" maxlength={maxLength.toString}
      name={funcName}
      value={valueBox openOr ""}
      tabindex={tabIndex toString}/>
  }

  def toForm = {
    uniqueFieldId match {
      case Full(id) =>
         <div id={id+"_holder"}><div><label for={id+"_field"}>{displayName}</label></div>{elem % ("id" -> (id+"_field"))}<lift:msg id={id}/></div>
      case _ => <div>{elem}</div>
    }

  }

  def asXHtml: NodeSeq = {
    var el = elem

    uniqueFieldId match {
      case Full(id) =>  el % ("id" -> (id+"_field"))
      case _ => el
    }
  }


  def defaultValue = ""

  /**
   * Make sure the field matches a regular expression
   */
  def valRegex(pat: Pattern, msg: => String)(valueBox: Box[String]): Box[Node] =
    valueBox flatMap {
      s => pat.matcher(s).matches match {
        case true => Empty
        case false => Full(Text(msg))
      }
    }

  final def toUpper(in: Box[String]): Box[String] = in.map(_.toUpperCase)

  final def trim(in: Box[String]): Box[String] = in.map(_.trim)

  final def notNull(in: Box[String]): Box[String] = in or Full("")

  def asJs = valueBox.map(Str) openOr JsNull

}


import _root_.java.sql.{ResultSet, Types}
import _root_.net.liftweb.mapper.{DriverType}

/**
 * A string field holding DB related logic
 */
class DBStringField[OwnerType <: DBRecord[OwnerType]](rec: OwnerType, maxLength: Int) extends
   StringField[OwnerType](rec, maxLength) with JDBCFieldFlavor[String]{

  def this(rec: OwnerType, maxLength: Int, value: String) = {
    this(rec, maxLength)
    set(value)
  }

  def this(rec: OwnerType, value: String) = {
    this(rec, 100)
    set(value)
  }

  def targetSQLType = Types.VARCHAR

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName+" VARCHAR("+maxLength+")"

  def jdbcFriendly(field : String) : String = value
 }

}
}
}
