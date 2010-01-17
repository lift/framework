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
import _root_.net.liftweb.http.{S}
import Helpers._
import S._

class DoubleField[OwnerType <: Record[OwnerType]](rec: OwnerType) extends NumericField[Double, OwnerType] {

  def owner = rec

  private def toDouble(in: Any): Double = {
    in match {
      case null => 0.0
      case i: Int => i
      case n: Long => n
      case n : Number => n.doubleValue
      case (n: Number) :: _ => n.doubleValue
      case Some(n) => toDouble(n)
      case None => 0.0
      case s: String => s.toDouble
      case x :: xs => toDouble(x)
      case o => toDouble(o.toString)
	}
  }

  /**
   * Sets the field value from an Any
   */
  def setFromAny(in: Any): Box[Double] = {
    in match {
      case n: Double => Full(this.set(n))
      case n: Number => Full(this.set(n.doubleValue))
      case (n: Number) :: _ => Full(this.set(n.doubleValue))
      case Some(n: Number) => Full(this.set(n.doubleValue))
      case None => Full(this.set(0.0))
      case (s: String) :: _ => setFromString(s)
      case null => Full(this.set(0L))
      case s: String => setFromString(s)
      case o => setFromString(o.toString)
    }
  }

  def setFromString(s: String): Box[Double] = {
    try{
      Full(set(java.lang.Double.parseDouble(s)));
    } catch {
      case e: Exception => valueCouldNotBeSet = true; Empty
    }
  }

  def defaultValue = 0.0

}

import _root_.java.sql.{ResultSet, Types}
import _root_.net.liftweb.mapper.{DriverType}

/**
 * An int field holding DB related logic
 */
abstract class DBDoubleField[OwnerType <: DBRecord[OwnerType]](rec: OwnerType) extends DoubleField[OwnerType](rec)
  with JDBCFieldFlavor[Double]{

  def targetSQLType = Types.DOUBLE

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.enumColumnType

  def jdbcFriendly(field : String) : Double = value

}
