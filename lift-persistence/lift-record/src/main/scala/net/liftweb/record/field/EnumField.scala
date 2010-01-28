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
import net.liftweb.http.js._
import S._
import Helpers._
import JE._


class EnumField[OwnerType <: Record[OwnerType], ENUM <: Enumeration](rec: OwnerType, enum: ENUM) extends Field[ENUM#Value, OwnerType] {

  def owner = rec

  def toInt = value.id

  def fromInt(in: Int): ENUM#Value = enum(in)

  override protected def set_!(value: ENUM#Value): ENUM#Value = {
    if (value != data) {
      data = value
      dirty_?(true)
    }
    data
  }

  def setFromAny(in: Any): Box[ENUM#Value] = {
    in match {
      case n: Int => Full(set(fromInt(n)))
      case n: Long => Full(set(fromInt(n.toInt)))
      case n: Number => Full(set(fromInt(n.intValue)))
      case (n: Number) :: _ => Full(set(fromInt(n.intValue)))
      case Some(n: Number) => Full(set(fromInt(n.intValue)))
      case Full(n: Number) => Full(set(fromInt(n.intValue)))
      case None | Empty | Failure(_, _, _) => Full(set(defaultValue))
      case (s: String) :: _ => Full(set(fromInt(Helpers.toInt(s))))
      case vs: ENUM#Value => Full(set(vs))
      case null => Full(set(defaultValue))
      case s: String => Full(set(fromInt(Helpers.toInt(s))))
      case o => Full(set(fromInt(Helpers.toInt(o))))
    }
  }

  def setFromString(s: String): Box[ENUM#Value] = setFromAny(s)

  /**
   * Build a list for v => this.set(fromInt(v)the select.  Return a tuple of (String, String) where the first string
   * is the id.string of the Value and the second string is the Text name of the Value.
   */
  def buildDisplayList: List[(Int, String)] = enum.map(a => (a.id, a.toString)).toList

  private def elem = SHtml.selectObj[Int](buildDisplayList, Full(toInt), this.setFromAny(_)) % ("tabindex" -> tabIndex.toString)

  def toForm = {
    var el = elem

    uniqueFieldId match {
      case Full(id) =>
        <div id={id+"_holder"}><div><label for={id+"_field"}>{displayName}</label></div>{el % ("id" -> (id+"_field"))}<lift:msg id={id}/></div>
      case _ => <div>{el}</div>
    }

  }

  def asXHtml: NodeSeq = {
    var el = elem

    uniqueFieldId match {
      case Full(id) => el % ("id" -> (id+"_field"))
      case _ => el
    }
  }

 def defaultValue: ENUM#Value = enum.iterator.next

 def asJs = Str(toString)

}

import _root_.java.sql.{ResultSet, Types}
import _root_.net.liftweb.mapper.{DriverType}

/**
 * An enum field holding DB related logic
 */
abstract class DBEnumField[OwnerType <: DBRecord[OwnerType], ENUM <: Enumeration](rec: OwnerType, enum: ENUM) extends
  EnumField(rec, enum) with JDBCFieldFlavor[Integer] {

  def targetSQLType = Types.VARCHAR

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.enumColumnType

  def jdbcFriendly(field: String) = new _root_.java.lang.Integer(toInt)

}

}
}
}
