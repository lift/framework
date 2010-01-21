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
import net.liftweb.http.{S, SHtml}
import _root_.java.util._
import S._
import Helpers._

object TimeZoneField {
  lazy val timeZoneList = TimeZone.getAvailableIDs.toList.
    filter(!_.startsWith("SystemV/")).
    filter(!_.startsWith("Etc/")).filter(_.length > 3).
    sort(_ < _).map(tz => (tz, tz))
}

class TimeZoneField[OwnerType <: Record[OwnerType]](rec: OwnerType) extends StringField(rec, 32) {

  override def defaultValue = TimeZone.getDefault.getID

  def isAsTimeZone: TimeZone = TimeZone.getTimeZone(value) match {
    case null => TimeZone.getDefault
    case x => x
  }

  private def elem = SHtml.select(TimeZoneField.timeZoneList, Full(value), set) % ("tabindex" -> tabIndex.toString)

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
      case Full(id) => el % ("id" -> (id+"_field"))
      case _ => el
    }
  }
}

import _root_.java.sql.{ResultSet, Types}
import _root_.net.liftweb.mapper.{DriverType}

class DBTimeZoneField[OwnerType <: Record[OwnerType]](rec: OwnerType) extends TimeZoneField(rec)
  with JDBCFieldFlavor[String] {

  def targetSQLType = Types.VARCHAR

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName+" VARCHAR("+32+")"

  def jdbcFriendly(field : String) : String = value

}

}
}
}
