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
import net.liftweb.http.{S}
import net.liftweb.json.JsonAST.{JInt, JNothing, JNull, JValue}
import net.liftweb.util._
import Helpers._
import S._

class LongField[OwnerType <: Record[OwnerType]](rec: OwnerType) extends NumericField[Long, OwnerType] {

  def this(rec: OwnerType, value: Long) = {
    this(rec)
    set(value)
  }

  def this(rec: OwnerType, value: Box[Long]) = {
    this(rec)
    setBox(value)
  }

  def owner = rec

  def setFromAny(in: Any): Box[Long] = setNumericFromAny(in, _.longValue)

  def setFromString(s: String): Box[Long] = setBox(asLong(s))

  def defaultValue = 0L

  def asJValue: JValue = valueBox.map(l => JInt(BigInt(l))) openOr (JNothing: JValue)
  def setFromJValue(jvalue: JValue): Box[Long] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JInt(i)                      => setBox(Full(i.longValue))
    case other                        => setBox(FieldHelpers.expectedA("JLong", other))
  }

}

import _root_.java.sql.{ResultSet, Types}
import _root_.net.liftweb.mapper.{DriverType}

/**
 * An int field holding DB related logic
 */
abstract class DBLongField[OwnerType <: DBRecord[OwnerType]](rec: OwnerType) extends LongField[OwnerType](rec)
  with JDBCFieldFlavor[Long]{

  def targetSQLType = Types.BIGINT

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.enumColumnType

  def jdbcFriendly(field : String) : Long = value

}

}
}
}
