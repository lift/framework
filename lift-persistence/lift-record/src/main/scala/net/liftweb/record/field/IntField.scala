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
import net.liftweb.common._
import net.liftweb.util._
import Helpers._
import net.liftweb.http.{S}
import S._

class IntField[OwnerType <: Record[OwnerType]](rec: OwnerType) extends NumericField[Int, OwnerType] {

  def owner = rec

  def this(rec: OwnerType, value: Int) = {
    this(rec)
    set(value)
  }

  /**
   * Sets the field value from an Any
   */
  def setFromAny(in: Any): Box[Int] = {
    in match {
      case n: Int => Full(this.set(n))
      case n: Number => Full(this.set(n.intValue))
      case (n: Number) :: _ => Full(this.set(n.intValue))
      case Some(n: Number) => Full(this.set(n.intValue))
      case Full(n: Number) => Full(this.set(n.intValue))
      case None | Empty | Failure(_, _, _) => Full(this.set(0))
      case (s: String) :: _ => setFromString(s)
      case null => Full( this.set(0))
      case s: String => setFromString(s)
      case o => setFromString(o.toString)
    }
  }

  def setFromString(s: String): Box[Int] = {
    try{
      Full(set(java.lang.Integer.parseInt(s)));
    } catch {
      case e: Exception => valueCouldNotBeSet = true; Empty
    }
  }

  def defaultValue = 0

}

import _root_.java.sql.{ResultSet, Types}
import _root_.net.liftweb.mapper.{DriverType}

/**
 * An int field holding DB related logic
 */
abstract class DBIntField[OwnerType <: DBRecord[OwnerType]](rec: OwnerType) extends IntField[OwnerType](rec)
  with JDBCFieldFlavor[Int]{

  def targetSQLType = Types.INTEGER

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.enumColumnType

  def jdbcFriendly(field : String) : Int = value

}

}
}
}
