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
import _root_.net.liftweb.mapper.{Safe}
import _root_.net.liftweb.http.{S}
import _root_.net.liftweb.http.js._
import _root_.java.util.regex._
import Helpers._
import S._
import JE._

object PasswordField {
  val blankPw = "*******"
}


class PasswordField[OwnerType <: Record[OwnerType]](rec: OwnerType) extends Field[String, OwnerType] {

  private val salt_i = FatLazy(Safe.randomString(16))
  private var invalidMsg : String = ""

  def owner = rec

  def salt = this.salt_i

  override def set_!(in: String) = hash("{"+in+"} salt={"+salt_i.get+"}")

  def setFromAny(in: Any): Box[String] = {
    in match {
      case a : Array[String] if (a.length == 2 && a(0) == a(1)) => Full(this.set(a(0)))
      case l : List[String] if (l.length == 2 && l.head == l(1)) => Full(this.set(l.head))
      case s : String  => Full(this.set(s))
      case o @ _ => Full(this.set(o.toString))
    }
  }

  def setFromString(s: String): Box[String] = Full(set(s))

  private def elem = S.fmapFunc(SFuncHolder(this.setFromAny(_))){
    funcName => <input type="pasword"
      name={funcName}
      value={value match {case null => "" case s => s.toString}}
      tabindex={tabIndex toString}/>}

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

  private def validatePassword(pwd: String): Box[Node] = pwd match {
    case "*" | PasswordField.blankPw if (pwd.length < 3) => Full(Text(S.??("password.too.short")))
    case "" | null => Full(Text(S.??("password.must.be.set")))
    case _ => Empty
  }

  override def validators = validatePassword _ :: Nil

  def defaultValue = ""

  def asJs = Str(value)

}

import _root_.java.sql.{ResultSet, Types}
import _root_.net.liftweb.mapper.{DriverType}

/**
 * A password field holding DB related logic
 */
abstract class DBPasswordField[OwnerType <: DBRecord[OwnerType]](rec: OwnerType, maxLength: Int) extends
  PasswordField[OwnerType](rec) with JDBCFieldFlavor[String]{

  def targetSQLType = Types.VARCHAR

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = if (colName.endsWith("_pw")) colName+" VARCHAR(48)" else colName+" VARCHAR(20)"

  def jdbcFriendly(columnName : String) = {
    if (columnName.endsWith("_slt")) {
      salt.get
    } else if (columnName.endsWith("_pw")) {
      value
    } else {
      null
    }
  }

}

}
}
}
