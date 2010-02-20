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
import _root_.net.liftweb.http.{S}
import _root_.java.util.regex._
import Helpers._
import S._


object EmailField {
  val emailPattern = Pattern.compile("^[a-z0-9._%-]+@(?:[a-z0-9-]+\\.)+[a-z]{2,4}$")
  def validEmailAddr_?(email: String): Boolean = emailPattern.matcher(email).matches
}

class EmailField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int) extends StringField[OwnerType](rec, maxLength) {

  private def validateEmail(emailBox: Box[String]): Box[Node] =
    emailBox.flatMap(email => EmailField.validEmailAddr_?(email) match {
      case false => Full(Text(S.??("invalid.email.address")))
      case _ => Empty
    })

  override def validators = validateEmail _ :: Nil

}


import _root_.java.sql.{ResultSet, Types}
import _root_.net.liftweb.mapper.{DriverType}

/**
 * An email field holding DB related logic
 */
class DBEmailField[OwnerType <: DBRecord[OwnerType]](rec: OwnerType, maxLength: Int) extends
  EmailField[OwnerType](rec, maxLength) with JDBCFieldFlavor[String]{

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
