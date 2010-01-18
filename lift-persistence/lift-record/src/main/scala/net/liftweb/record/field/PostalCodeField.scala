/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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

import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.mapper.{Safe}
import _root_.net.liftweb.http.{S}
import _root_.java.util.regex._
import Helpers._
import S._

class PostalCodeField[OwnerType <: Record[OwnerType]](rec: OwnerType, country: CountryField[OwnerType]) extends StringField(rec, 32) {

  override def setFilter = notNull _ :: toUpper _ :: trim _ :: super.setFilter

  private def genericCheck(zip: String): Box[Node] = {
    zip match {
      case null => Full(Text(S.??("invalid.postal.code")))
      case s if s.length < 3 => Full(Text(S.??("invalid.postal.code")))
      case _ => Empty
    }
  }

  def validate(in : String) = country.value match {
    case Countries.USA => valRegex(_root_.java.util.regex.Pattern.compile("[0-9]{5}(\\-[0-9]{4})?"), S.??("invalid.zip.code"))(value)
    case Countries.Sweden => valRegex(_root_.java.util.regex.Pattern.compile("[0-9]{3}[ ]?[0-9]{2}"), S.??("invalid.postal.code"))(value)
    case Countries.Australia => valRegex(_root_.java.util.regex.Pattern.compile("(0?|[1-9])[0-9]{3}"), S.??("invalid.postal.code"))(value)
    case Countries.Canada => valRegex(_root_.java.util.regex.Pattern.compile("[A-Z][0-9][A-Z][ ][0-9][A-Z][0-9]"), S.??("invalid.postal.code"))(value)
    case _ => genericCheck(country.value.toString)
  }

  override def validators = validate _ :: Nil

}

import _root_.java.sql.{ResultSet, Types}
import _root_.net.liftweb.mapper.{DriverType}

class DBPostalCodeField[OwnerType <: DBRecord[OwnerType]](rec: OwnerType, country: CountryField[OwnerType]) extends
  PostalCodeField[OwnerType](rec, country) with JDBCFieldFlavor[String]{

  def targetSQLType = Types.VARCHAR

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName+" VARCHAR("+32+")"

  def jdbcFriendly(field : String) : String = value

}
